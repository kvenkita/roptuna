#' Create a TPE sampler
#' @param n_startup_trials Random trials before TPE activates.
#' @param gamma Fraction of trials treated as "good" (default 0.25).
#' @param n_ei_candidates Candidates sampled per parameter.
#' @param seed Random seed.
#' @return A `TpeSampler` R6 object.
#' @examples
#' sampler <- tpe_sampler(n_startup_trials = 5L, seed = 42L)
#' study <- create_study(sampler = sampler)
#' @export
tpe_sampler <- function(n_startup_trials = 10L, gamma = 0.25,
                        n_ei_candidates = 24L, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  TpeSampler$new(as.integer(n_startup_trials), gamma, as.integer(n_ei_candidates))
}

TpeSampler <- R6::R6Class("TpeSampler",
  public = list(
    initialize = function(n_startup_trials, gamma, n_ei_candidates) {
      private$.n_startup       <- n_startup_trials
      private$.gamma           <- gamma
      private$.n_ei_candidates <- n_ei_candidates
      private$.random          <- RandomSampler$new()
    },

    sample_independent = function(study, trial, param_name, distribution) {
      completed <- study$storage_ref$get_all_trials(study$study_id, states = "complete")
      relevant  <- Filter(function(t) !is.null(t$params[[param_name]]), completed)

      if (length(relevant) < private$.n_startup)
        return(dist_sample_random(distribution))

      private$.tpe_sample_one(study, completed, param_name, distribution)
    },

    infer_relative_search_space = function(study, trial) {
      completed <- study$storage_ref$get_all_trials(study$study_id, states = "complete")
      if (length(completed) == 0) return(list())
      dists <- list()
      for (t in completed)
        for (pname in names(t$distributions))
          if (is.null(dists[[pname]])) dists[[pname]] <- t$distributions[[pname]]
      dists
    },

    sample_relative = function(study, trial, distributions, search_space) {
      completed <- study$storage_ref$get_all_trials(study$study_id, states = "complete")
      if (length(completed) < private$.n_startup || length(distributions) == 0)
        return(private$.random$sample_relative(study, trial, distributions, search_space))

      result <- list()
      for (pname in names(distributions)) {
        result[[pname]] <- private$.tpe_sample_one(
          study, completed, pname, distributions[[pname]])
      }
      result
    }
  ),

  private = list(
    .n_startup = NULL, .gamma = NULL, .n_ei_candidates = NULL, .random = NULL,

    .split_trials = function(study, completed) {
      vals    <- sapply(completed, `[[`, "value")
      n_below <- max(1L, as.integer(floor(private$.gamma * length(completed))))
      if (study$direction == "minimize") {
        below_idx <- order(vals)[seq_len(n_below)]
      } else {
        below_idx <- order(vals, decreasing = TRUE)[seq_len(n_below)]
      }
      list(
        below = completed[below_idx],
        above = completed[setdiff(seq_along(completed), below_idx)]
      )
    },

    .tpe_sample_one = function(study, completed, pname, dist) {
      split  <- private$.split_trials(study, completed)
      bv     <- unlist(lapply(split$below, function(t) t$params[[pname]]))
      av     <- unlist(lapply(split$above, function(t) t$params[[pname]]))
      bv     <- bv[!sapply(bv, is.null)]
      av     <- av[!sapply(av, is.null)]
      n_cand <- private$.n_ei_candidates

      if (inherits(dist, "roptuna_float_distribution")) {
        l <- .build_parzen(bv, dist$low, dist$high, dist$log)
        g <- .build_parzen(av, dist$low, dist$high, dist$log)
        cands <- .sample_parzen(l, n_cand)
        cands[which.max(.eval_parzen(l, cands) - .eval_parzen(g, cands))]

      } else if (inherits(dist, "roptuna_int_distribution")) {
        use_log <- isTRUE(dist$log)
        if (dist$step > 1L) {
          # Treat stepped integers as categorical over valid values
          valid <- seq(dist$low, dist$high, by = dist$step)
          lw <- .cat_weights(bv, valid)
          gw <- .cat_weights(av, valid)
          cands_idx <- sample(seq_along(valid), n_cand, replace = TRUE, prob = lw)
          best_idx  <- cands_idx[which.max(log(lw[cands_idx]) - log(gw[cands_idx]))]
          as.integer(valid[best_idx])
        } else {
          low_c  <- if (use_log) log(dist$low) else dist$low - 0.5
          high_c <- if (use_log) log(dist$high) else dist$high + 0.5
          bv_t   <- if (use_log && length(bv) > 0) log(bv) else bv
          av_t   <- if (use_log && length(av) > 0) log(av) else av
          l <- .build_parzen(bv_t, low_c, high_c, FALSE)
          g <- .build_parzen(av_t, low_c, high_c, FALSE)
          raw <- .sample_parzen(l, n_cand)
          if (use_log) raw <- exp(raw)
          cands <- pmax(pmin(as.integer(round(raw)), dist$high), dist$low)
          as.integer(cands[which.max(.eval_parzen(l, if (use_log) log(cands) else cands) -
                                    .eval_parzen(g, if (use_log) log(cands) else cands))])
        }
      } else {
        lw <- .cat_weights(bv, dist$choices)
        gw <- .cat_weights(av, dist$choices)
        cands <- sample(dist$choices, n_cand, replace = TRUE, prob = lw)
        idx   <- match(cands, dist$choices)
        cands[which.max(log(lw[idx]) - log(gw[idx]))]
      }
    }
  )
)

.build_parzen <- function(obs, low, high, log_scale) {
  if (log_scale) { obs <- log(obs); low <- log(low); high <- log(high) }
  prior_sigma <- high - low
  if (length(obs) == 0)
    return(list(means = (low+high)/2, sigmas = prior_sigma,
                weights = 1.0, low = low, high = high, log = log_scale))
  n <- length(obs); sorted <- sort(obs)
  obs_sigmas <- if (n == 1) prior_sigma else {
    diffs <- diff(sorted)
    c(diffs[1], pmax(diffs[-length(diffs)], diffs[-1]), diffs[length(diffs)])
  }
  obs_sigmas <- pmin(obs_sigmas, prior_sigma)
  # Minimum sigma prevents over-concentration and NaN from zero-variance kernels.
  # floor = prior_sigma / (n+2) matches Scott's rule scaling and ensures the
  # Parzen estimate can still explore the full search space as trials accumulate.
  obs_sigmas <- pmax(obs_sigmas, prior_sigma / (n + 2))
  w <- rep(1/(n+1), n+1)
  list(means   = c(sorted, (low+high)/2),
       sigmas  = c(obs_sigmas, prior_sigma),
       weights = w / sum(w), low = low, high = high, log = log_scale)
}

.sample_parzen <- function(est, n) {
  idx  <- sample.int(length(est$weights), n, replace = TRUE, prob = est$weights)
  samp <- pmax(pmin(stats::rnorm(n, est$means[idx], est$sigmas[idx]),
                    est$high), est$low)
  if (est$log) exp(samp) else samp
}

.eval_parzen <- function(est, x) {
  xt <- if (est$log) log(x) else x
  log_w <- log(est$weights)
  log_p <- matrix(nrow = length(est$means), ncol = length(xt))
  for (i in seq_along(est$means))
    log_p[i, ] <- log_w[i] + stats::dnorm(xt, est$means[i], est$sigmas[i], log = TRUE)
  apply(log_p, 2, function(col) { mx <- max(col); mx + log(sum(exp(col - mx))) })
}

.cat_weights <- function(obs, choices) {
  counts <- tabulate(match(obs, choices), nbins = length(choices))
  w <- (counts + 1.0) / (length(obs) + length(choices))
  w / sum(w)
}
