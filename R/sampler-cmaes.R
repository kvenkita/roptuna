#' Create a CMA-ES sampler for continuous optimization
#' @param n_startup_trials Random trials before CMA-ES activates (default 1).
#' @param sigma0 Initial step size (default 0.3 * (high - low) per param).
#' @param seed Random seed (NULL for random).
#' @return A `CmaEsSampler` R6 object.
#' @export
cmaes_sampler <- function(n_startup_trials = 1L, sigma0 = NULL, seed = NULL) {
  CmaEsSampler$new(as.integer(n_startup_trials), sigma0, seed)
}

#' @export
CmaEsSampler <- R6::R6Class("CmaEsSampler",
  public = list(
    initialize = function(n_startup_trials, sigma0, seed) {
      private$.n_startup <- n_startup_trials
      private$.sigma0    <- sigma0
      if (!is.null(seed)) set.seed(seed)
    },

    infer_relative_search_space = function(study, trial) {
      complete <- study$storage_ref$get_all_trials(study$study_id, states = "complete")
      if (length(complete) == 0) return(list())
      result <- list()
      for (t in complete) {
        for (nm in names(t$distributions)) {
          if (is.null(result[[nm]]) &&
              inherits(t$distributions[[nm]], "roptuna_float_distribution")) {
            result[[nm]] <- t$distributions[[nm]]
          }
        }
      }
      result
    },

    sample_relative = function(study, trial, search_space, all_params) {
      if (length(search_space) == 0) return(list())
      complete <- study$storage_ref$get_all_trials(study$study_id, states = "complete")
      eligible <- Filter(function(t) {
        all(names(search_space) %in% names(t$params))
      }, complete)

      if (length(eligible) < private$.n_startup) {
        return(private$.random_sample(search_space))
      }

      param_nms <- names(search_space)
      n         <- length(param_nms)
      private$.maybe_init(n, param_nms, eligible, search_space)
      private$.maybe_update(eligible, search_space)

      # Sample: mean + sigma * B * (D * z), in raw parameter space
      z     <- rnorm(n)
      x_raw <- private$.mean + private$.sigma * as.numeric(
        private$.B %*% (private$.D * z))

      result <- list()
      for (i in seq_along(param_nms)) {
        nm   <- param_nms[[i]]
        dist <- search_space[[nm]]
        result[[nm]] <- max(dist$low, min(dist$high, x_raw[[i]]))
      }
      result
    },

    sample_independent = function(study, trial, param_name, dist) {
      dist_sample_random(dist)
    }
  ),

  private = list(
    .n_startup = NULL,
    .sigma0    = NULL,
    .mean      = NULL,
    .C         = NULL,
    .sigma     = NULL,
    .p_c       = NULL,
    .p_sigma   = NULL,
    .B         = NULL,
    .D         = NULL,
    .param_nms = NULL,
    .last_n    = 0L,
    .mu        = NULL,
    .weights   = NULL,
    .mu_w      = NULL,
    .c_sigma   = NULL,
    .d_sigma   = NULL,
    .c_c       = NULL,
    .c_1       = NULL,
    .c_mu      = NULL,
    .chi_n     = NULL,
    .lo        = NULL,
    .hi        = NULL,

    .random_sample = function(search_space) {
      out <- list()
      for (nm in names(search_space)) out[[nm]] <- dist_sample_random(search_space[[nm]])
      out
    },

    .maybe_init = function(n, param_nms, eligible, search_space) {
      if (!is.null(private$.mean) && identical(private$.param_nms, param_nms)) return()
      private$.param_nms <- param_nms

      lam     <- max(4L + as.integer(floor(3 * log(n))), 2L)
      mu      <- max(as.integer(floor(lam / 2)), 1L)
      w_raw   <- log(mu + 0.5) - log(seq_len(mu))
      weights <- w_raw / sum(w_raw)
      mu_w    <- 1 / sum(weights^2)
      c_sigma <- (mu_w + 2) / (n + mu_w + 5)
      d_sigma <- 1 + 2 * max(0, sqrt((mu_w - 1) / (n + 1)) - 1) + c_sigma
      c_c     <- (4 + mu_w / n) / (n + 4 + 2 * mu_w / n)
      c_1     <- 2 / ((n + 1.3)^2 + mu_w)
      c_mu    <- min(1 - c_1, 2 * (mu_w - 2 + 1/mu_w) / ((n + 2)^2 + mu_w))
      chi_n   <- sqrt(n) * (1 - 1/(4*n) + 1/(21*n^2))

      private$.mu      <- mu
      private$.weights <- weights
      private$.mu_w    <- mu_w
      private$.c_sigma <- c_sigma
      private$.d_sigma <- d_sigma
      private$.c_c     <- c_c
      private$.c_1     <- c_1
      private$.c_mu    <- c_mu
      private$.chi_n   <- chi_n

      lo <- sapply(param_nms, function(nm) search_space[[nm]]$low)
      hi <- sapply(param_nms, function(nm) search_space[[nm]]$high)
      private$.lo <- lo
      private$.hi <- hi

      # Initialize sigma based on search space range
      if (is.null(private$.sigma0)) {
        private$.sigma <- mean(hi - lo) * 0.3
      } else {
        private$.sigma <- private$.sigma0
      }

      # Init mean from best trial (raw parameter values)
      best_val <- Inf; best_tr <- eligible[[1]]
      for (t in eligible) {
        v <- t$value %||% Inf
        if (!is.null(v) && !is.na(v) && v < best_val) { best_val <- v; best_tr <- t }
      }
      private$.mean    <- sapply(param_nms, function(nm) as.numeric(best_tr$params[[nm]]))
      private$.C       <- diag(n)
      private$.p_c     <- numeric(n)
      private$.p_sigma <- numeric(n)
      private$.B       <- diag(n)
      private$.D       <- rep(1.0, n)
      private$.last_n  <- length(eligible)
    },

    .maybe_update = function(eligible, search_space) {
      n_new <- length(eligible)
      # Only update once per generation (lam new evaluations)
      lam   <- max(4L + as.integer(floor(3 * log(length(private$.param_nms)))), 2L)
      if (n_new < private$.last_n + lam) return()
      private$.last_n <- n_new

      param_nms <- private$.param_nms
      n         <- length(param_nms)
      mu        <- private$.mu

      # Select best mu trials by objective value
      sorted  <- eligible[order(sapply(eligible, function(t) t$value %||% Inf))]
      selected <- head(sorted, mu)
      if (length(selected) < 1L) return()
      mu_eff <- length(selected)

      # Extract raw parameter vectors
      x_mat <- sapply(selected, function(t) {
        sapply(param_nms, function(nm) as.numeric(t$params[[nm]]))
      })
      if (!is.matrix(x_mat)) x_mat <- matrix(x_mat, nrow = n)

      w <- private$.weights[seq_len(mu_eff)]
      w <- w / sum(w)

      old_mean <- private$.mean
      new_mean <- as.numeric(x_mat %*% w)
      y        <- (new_mean - old_mean) / max(private$.sigma, 1e-10)

      # Inverse sqrt of C for p_sigma update
      invsqrtC <- tryCatch({
        ei <- eigen(private$.C, symmetric = TRUE)
        ei$vectors %*% diag(1 / sqrt(pmax(ei$values, 1e-10))) %*% t(ei$vectors)
      }, error = function(e) diag(n))

      p_sigma_new <- as.numeric((1 - private$.c_sigma) * private$.p_sigma +
        sqrt(private$.c_sigma * (2 - private$.c_sigma) * private$.mu_w) *
        (invsqrtC %*% y))

      norm_ps  <- sqrt(sum(p_sigma_new^2))
      sigma_new <- private$.sigma * exp(
        (private$.c_sigma / private$.d_sigma) * (norm_ps / private$.chi_n - 1))

      h_sigma <- as.numeric(
        norm_ps / sqrt(1 - (1 - private$.c_sigma)^(2 * n_new)) <
        (1.4 + 2/(n + 1)) * private$.chi_n)

      p_c_new <- as.numeric((1 - private$.c_c) * private$.p_c +
        h_sigma * sqrt(private$.c_c * (2 - private$.c_c) * private$.mu_w) * y)

      # Rank-mu covariance update
      y_mat <- (x_mat - old_mean) / max(private$.sigma, 1e-10)
      rank_mu <- matrix(0, n, n)
      for (k in seq_len(mu_eff)) {
        rank_mu <- rank_mu + w[[k]] * outer(y_mat[, k], y_mat[, k])
      }
      C_new <- (1 - private$.c_1 - private$.c_mu) * private$.C +
        private$.c_1 * (outer(p_c_new, p_c_new) +
                        (1 - h_sigma) * private$.c_c * (2 - private$.c_c) * private$.C) +
        private$.c_mu * rank_mu

      ei <- tryCatch(eigen(C_new, symmetric = TRUE),
                     error = function(e) list(values = rep(1, n), vectors = diag(n)))

      private$.B       <- ei$vectors
      private$.D       <- sqrt(pmax(ei$values, 1e-10))
      private$.mean    <- new_mean
      private$.sigma   <- min(max(sigma_new, 1e-8), mean(private$.hi - private$.lo) * 2)
      private$.C       <- C_new
      private$.p_c     <- p_c_new
      private$.p_sigma <- p_sigma_new
    }
  )
)
