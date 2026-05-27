#' Create an NSGA-II sampler for multi-objective optimization
#' @param population_size Size of parent population (default 50).
#' @param eta_c SBX crossover parameter (default 20).
#' @param eta_m Polynomial mutation parameter (default 20).
#' @param mutation_prob Per-param mutation probability (default 1/n_params).
#' @param seed Random seed (NULL for random).
#' @return An `NSGAIISampler` R6 object.
#' @export
nsgaii_sampler <- function(population_size = 50L, eta_c = 20, eta_m = 20,
                           mutation_prob = NULL, seed = NULL) {
  NSGAIISampler$new(as.integer(population_size), eta_c, eta_m, mutation_prob, seed)
}

#' NSGA-II sampler R6 class
#'
#' See [nsgaii_sampler()] for the recommended constructor.
#' @export
NSGAIISampler <- R6::R6Class("NSGAIISampler",
  public = list(
    initialize = function(population_size, eta_c, eta_m, mutation_prob, seed) {
      private$.pop_size <- population_size
      private$.eta_c    <- eta_c
      private$.eta_m    <- eta_m
      private$.mut_prob <- mutation_prob
      if (!is.null(seed)) set.seed(seed)
    },

    infer_relative_search_space = function(study, trial) {
      complete <- study$storage_ref$get_all_trials(study$study_id, states = "complete")
      if (length(complete) == 0) return(list())
      all_names <- unique(unlist(lapply(complete, function(t) names(t$distributions))))
      result <- list()
      for (nm in all_names) {
        for (t in complete) {
          if (!is.null(t$distributions[[nm]])) {
            result[[nm]] <- t$distributions[[nm]]
            break
          }
        }
      }
      result
    },

    sample_relative = function(study, trial, search_space, all_params) {
      complete  <- study$storage_ref$get_all_trials(study$study_id, states = "complete")
      mo_trials <- Filter(function(t) !is.null(t$values), complete)

      if (length(mo_trials) < 2L || length(search_space) == 0) {
        return(private$.random_sample(search_space))
      }

      dirs   <- study$directions
      ranked <- private$.fast_nondominated_sort(mo_trials, dirs)
      ranked <- private$.crowding_distance(ranked)

      p1 <- private$.tournament_select(ranked)
      p2 <- private$.tournament_select(ranked)

      n_params <- length(search_space)
      mut_prob <- private$.mut_prob %||% (1.0 / max(n_params, 1L))
      child_params <- list()
      for (nm in names(search_space)) {
        dist <- search_space[[nm]]
        v1   <- p1$params[[nm]]
        v2   <- p2$params[[nm]]
        if (is.null(v1) || is.null(v2)) {
          child_params[[nm]] <- dist_sample_random(dist)
          next
        }
        if (inherits(dist, "roptuna_float_distribution")) {
          child_params[[nm]] <- private$.sbx_mutate(
            as.numeric(v1), as.numeric(v2),
            dist$low, dist$high, private$.eta_c, private$.eta_m, mut_prob)
        } else if (inherits(dist, "roptuna_int_distribution")) {
          cv <- private$.sbx_mutate(
            as.numeric(v1), as.numeric(v2),
            dist$low - 0.5, dist$high + 0.5, private$.eta_c, private$.eta_m, mut_prob)
          child_params[[nm]] <- as.integer(round(max(dist$low, min(dist$high, cv))))
        } else {
          child_params[[nm]] <- if (runif(1) < 0.5) v1 else v2
        }
      }
      child_params
    },

    sample_independent = function(study, trial, param_name, dist) {
      dist_sample_random(dist)
    }
  ),

  private = list(
    .pop_size = NULL,
    .eta_c    = NULL,
    .eta_m    = NULL,
    .mut_prob = NULL,

    .random_sample = function(search_space) {
      out <- list()
      for (nm in names(search_space)) out[[nm]] <- dist_sample_random(search_space[[nm]])
      out
    },

    .fast_nondominated_sort = function(trials, dirs) {
      n <- length(trials)
      rank <- integer(n)
      remaining <- seq_len(n)
      current_rank <- 1L
      while (length(remaining) > 0) {
        front <- integer(0)
        for (i in remaining) {
          dominated <- FALSE
          for (j in remaining) {
            if (i == j) next
            a <- trials[[j]]$values
            b <- trials[[i]]$values
            a_n <- ifelse(dirs == "minimize", a, -a)
            b_n <- ifelse(dirs == "minimize", b, -b)
            if (all(a_n <= b_n) && any(a_n < b_n)) {
              dominated <- TRUE
              break
            }
          }
          if (!dominated) front <- c(front, i)
        }
        rank[front] <- current_rank
        remaining <- setdiff(remaining, front)
        current_rank <- current_rank + 1L
      }
      lapply(seq_len(n), function(i)
        c(trials[[i]], list(.rank = rank[[i]], .crowd = 0)))
    },

    .crowding_distance = function(ranked) {
      n <- length(ranked)
      if (n == 0) return(ranked)
      n_obj <- length(ranked[[1]]$values)
      crowd <- numeric(n)
      for (obj in seq_len(n_obj)) {
        vals <- sapply(ranked, function(t) t$values[[obj]])
        ord  <- order(vals)
        crowd[ord[[1]]] <- Inf
        crowd[ord[[n]]] <- Inf
        rng <- diff(range(vals))
        if (rng == 0 || n < 3) next
        for (k in 2:(n - 1)) {
          crowd[ord[[k]]] <- crowd[ord[[k]]] +
            (vals[ord[[k + 1]]] - vals[ord[[k - 1]]]) / rng
        }
      }
      for (i in seq_len(n)) ranked[[i]]$.crowd <- crowd[[i]]
      ranked
    },

    .tournament_select = function(ranked) {
      n <- length(ranked)
      i <- sample.int(n, 1L)
      j <- sample.int(n, 1L)
      a <- ranked[[i]]; b <- ranked[[j]]
      if (a$.rank < b$.rank) return(a)
      if (b$.rank < a$.rank) return(b)
      if (a$.crowd >= b$.crowd) return(a) else return(b)
    },

    .sbx_mutate = function(v1, v2, lo, hi, eta_c, eta_m, mut_prob) {
      u <- runif(1)
      beta <- if (u <= 0.5) (2 * u)^(1 / (eta_c + 1)) else
                (1 / (2 * (1 - u)))^(1 / (eta_c + 1))
      child <- max(lo, min(hi, 0.5 * ((1 + beta) * v1 + (1 - beta) * v2)))
      if (runif(1) < mut_prob) {
        delta <- min(child - lo, hi - child) / (hi - lo + 1e-10)
        u2 <- runif(1)
        dq <- if (u2 < 0.5)
          (2 * u2 + (1 - 2 * u2) * (1 - delta)^(eta_m + 1))^(1 / (eta_m + 1)) - 1
        else
          1 - (2 * (1 - u2) + 2 * (u2 - 0.5) * (1 - delta)^(eta_m + 1))^(1 / (eta_m + 1))
        child <- max(lo, min(hi, child + dq * (hi - lo)))
      }
      child
    }
  )
)
