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
      dist_sample_random(distribution)
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

      vals <- sapply(completed, `[[`, "value")
      n_below <- max(1L, as.integer(floor(private$.gamma * length(completed))))
      if (study$direction == "minimize") {
        below_idx <- order(vals)[seq_len(n_below)]
      } else {
        below_idx <- order(vals, decreasing = TRUE)[seq_len(n_below)]
      }
      above_idx <- setdiff(seq_along(completed), below_idx)
      below_trials <- completed[below_idx]
      above_trials <- completed[above_idx]

      result <- list()
      n_cand <- private$.n_ei_candidates
      for (pname in names(distributions)) {
        dist <- distributions[[pname]]
        bv <- unlist(lapply(below_trials, function(t) t$params[[pname]]))
        av <- unlist(lapply(above_trials, function(t) t$params[[pname]]))

        if (inherits(dist, "roptuna_float_distribution")) {
          l <- .build_parzen(bv, dist$low, dist$high, dist$log)
          g <- .build_parzen(av, dist$low, dist$high, dist$log)
          cands <- .sample_parzen(l, n_cand)
          result[[pname]] <- cands[which.max(.eval_parzen(l, cands) - .eval_parzen(g, cands))]

        } else if (inherits(dist, "roptuna_int_distribution")) {
          l <- .build_parzen(bv, dist$low - 0.5, dist$high + 0.5, FALSE)
          g <- .build_parzen(av, dist$low - 0.5, dist$high + 0.5, FALSE)
          cands <- pmax(pmin(round(.sample_parzen(l, n_cand)), dist$high), dist$low)
          result[[pname]] <- as.integer(cands[which.max(
            .eval_parzen(l, cands) - .eval_parzen(g, cands))])

        } else {
          lw <- .cat_weights(bv, dist$choices)
          gw <- .cat_weights(av, dist$choices)
          cands <- sample(dist$choices, n_cand, replace = TRUE, prob = lw)
          idx <- match(cands, dist$choices)
          result[[pname]] <- cands[which.max(log(lw[idx]) - log(gw[idx]))]
        }
      }
      result
    }
  ),
  private = list(.n_startup = NULL, .gamma = NULL,
                 .n_ei_candidates = NULL, .random = NULL)
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
  # Build matrix by index to avoid match() returning wrong index for duplicate means.
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
