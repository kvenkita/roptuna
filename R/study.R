#' Create a new optimization study
#'
#' @param direction "minimize" or "maximize".
#' @param sampler A sampler object (e.g. `tpe_sampler()`). Defaults to `random_sampler()`.
#' @param pruner A pruner object (e.g. `median_pruner()`). NULL disables pruning.
#' @param storage A storage backend. Defaults to `InMemoryStorage$new()`.
#' @param study_name Character name. Auto-generated if NULL.
#' @param load_if_exists For SQLite: if TRUE, load existing study with this name.
#' @return A `Study` R6 object.
#' @examples
#' study <- create_study(direction = "minimize", sampler = tpe_sampler(seed = 1L))
#' study$optimize(function(trial) trial$suggest_float("x", -5, 5)^2, n_trials = 5)
#' study$best_value
#' @export
create_study <- function(direction = "minimize", sampler = NULL,
                         pruner = NULL, storage = NULL,
                         study_name = NULL, load_if_exists = FALSE,
                         directions = NULL) {
  if (!is.null(directions)) {
    directions <- match.arg(directions, c("minimize", "maximize"), several.ok = TRUE)
    direction  <- directions[[1]]
  } else {
    direction  <- match.arg(direction, c("minimize", "maximize"))
  }
  if (is.null(storage))    storage    <- InMemoryStorage$new()
  if (is.null(sampler))    sampler    <- random_sampler()
  if (is.null(study_name)) study_name <- paste0("study-", format(Sys.time(), "%Y%m%d%H%M%S"))

  if (load_if_exists) {
    existing <- storage$find_study(study_name)
    study_id <- if (!is.null(existing)) existing else
      storage$create_study(study_name, direction, directions = directions)
  } else {
    study_id <- storage$create_study(study_name, direction, directions = directions)
  }

  Study$new(
    study_id   = study_id,
    study_name = study_name,
    direction  = direction,
    directions = directions,
    sampler    = sampler,
    pruner     = pruner,
    storage    = storage
  )
}

#' Study R6 class for hyperparameter optimization
#'
#' Manages the optimization loop. Create via [create_study()].
#' @examples
#' study <- create_study(direction = "minimize")
#' study$optimize(function(trial) trial$suggest_float("x", -5, 5)^2, n_trials = 5)
#' study$best_value
#' @export
Study <- R6::R6Class("Study",
  cloneable = FALSE,

  public = list(
    study_id   = NULL,
    study_name = NULL,
    direction  = NULL,

    initialize = function(study_id, study_name, direction,
                          sampler, pruner, storage, directions = NULL) {
      self$study_id   <- study_id
      self$study_name <- study_name
      self$direction  <- direction
      private$.directions    <- directions
      private$.sampler       <- sampler
      private$.pruner        <- pruner
      private$.storage       <- storage
      private$.queued_params <- list()
      private$.stopped       <- FALSE
    },

    #' @description Run the objective function for n_trials iterations.
    #' @param func Objective function taking a Trial and returning a numeric scalar.
    #' @param n_trials Number of trials to run.
    #' @param timeout Wall-clock timeout in seconds (NULL = no limit).
    #' @param catch Character vector of additional condition classes to catch as failed trials.
    #' @param callbacks List of functions `function(study, trial_snapshot)` called after each trial.
    #' @param n_jobs Number of parallel workers (>1 requires `parallel` package).
    optimize = function(func, n_trials = 100, timeout = NULL,
                        catch = character(), callbacks = NULL, n_jobs = 1L) {
      private$.stopped <- FALSE
      if (as.integer(n_jobs) > 1L) {
        private$.optimize_parallel(func, n_trials, timeout, catch, callbacks,
                                   as.integer(n_jobs))
      } else {
        private$.optimize_sequential(func, n_trials, timeout, catch, callbacks)
      }
      invisible(self)
    },

    #' @description Create a trial without evaluating the objective (decoupled API).
    #'   Call tell() with the returned trial to record the result.
    #' @return A `Trial` R6 object ready for parameter suggestion.
    ask = function() {
      trial_id <- private$.storage$create_trial(self$study_id)
      if (length(private$.queued_params) > 0) {
        queued <- private$.queued_params[[1]]
        private$.queued_params <- private$.queued_params[-1]
        for (pname in names(queued)) {
          val  <- queued[[pname]]
          dist <- private$.infer_dist(val)
          private$.storage$set_trial_param(self$study_id, trial_id, pname, dist, val)
        }
      }
      already_set <- private$.storage$get_trial(self$study_id, trial_id)$params
      rel_space   <- private$.sampler$infer_relative_search_space(self, NULL)
      rel_space   <- rel_space[setdiff(names(rel_space), names(already_set))]
      if (length(rel_space) > 0) {
        rel_params <- private$.sampler$sample_relative(self, NULL, rel_space, rel_space)
        for (pname in names(rel_params)) {
          private$.storage$set_trial_param(
            self$study_id, trial_id, pname, rel_space[[pname]], rel_params[[pname]])
        }
      }
      Trial$new(
        trial_id = trial_id,
        study_id = self$study_id,
        storage  = private$.storage,
        pruner   = private$.pruner,
        sampler  = private$.sampler,
        study    = self
      )
    },

    #' @description Record the result of a trial created by ask().
    #' @param trial A `Trial` object returned by ask().
    #' @param values Numeric scalar or vector (multi-obj), or NULL for failed/pruned.
    #' @param state "complete", "failed", or "pruned".
    tell = function(trial, values, state = "complete") {
      trial_id <- trial$trial_id
      if (state == "complete" && !is.null(values)) {
        if (!is.null(private$.directions) && length(values) > 1) {
          private$.storage$set_trial_values(self$study_id, trial_id, values)
          private$.storage$set_trial_state(
            self$study_id, trial_id, "complete", value = values[[1]])
        } else {
          private$.storage$set_trial_state(
            self$study_id, trial_id, "complete", value = as.numeric(values[[1]]))
        }
      } else {
        private$.storage$set_trial_state(self$study_id, trial_id, state)
      }
      invisible(self)
    },

    #' @description Signal the optimize loop to stop after the current trial.
    stop = function() {
      private$.stopped <- TRUE
      invisible(self)
    },

    #' @description Set a user attribute on the study.
    set_user_attr = function(key, value) {
      private$.storage$set_study_user_attr(self$study_id, key, value)
      invisible(self)
    },

    #' @description Queue a trial with fixed parameter values.
    #'   The next call to optimize() will use these values instead of sampling.
    #' @param params Named list of parameter name -> value.
    enqueue_trial = function(params) {
      private$.queued_params <- c(private$.queued_params, list(params))
      invisible(self)
    },

    #' @description Add a completed (or failed/pruned) trial directly to the study.
    #' @param trial_info Named list with: `params` (named list), `value` (numeric),
    #'   and optionally `state` ("complete"), `distributions` (named list),
    #'   `user_attrs` (named list), `intermediate_values` (named numeric).
    add_trial = function(trial_info) {
      state       <- trial_info$state %||% "complete"
      params      <- trial_info$params %||% list()
      dists       <- trial_info$distributions %||% list()
      user_attrs  <- trial_info$user_attrs %||% list()
      iv          <- trial_info$intermediate_values %||% list()

      tid <- private$.storage$create_trial(self$study_id)

      for (pname in names(params)) {
        val  <- params[[pname]]
        dist <- dists[[pname]] %||% private$.infer_dist(val)
        private$.storage$set_trial_param(self$study_id, tid, pname, dist, val)
      }
      for (key in names(user_attrs)) {
        private$.storage$set_trial_user_attr(self$study_id, tid, key, user_attrs[[key]])
      }
      for (step_key in names(iv)) {
        private$.storage$set_trial_intermediate_value(
          self$study_id, tid, as.integer(step_key), iv[[step_key]])
      }

      private$.storage$set_trial_state(
        self$study_id, tid, state, value = trial_info$value %||% trial_info$values[[1]])
      if (!is.null(trial_info$values))
        private$.storage$set_trial_values(self$study_id, tid, trial_info$values)
      invisible(self)
    },

    #' @description Add multiple trials at once. See [add_trial()].
    #' @param trial_list List of trial_info lists.
    add_trials = function(trial_list) {
      for (t in trial_list) self$add_trial(t)
      invisible(self)
    },

    #' @description Return all trials as a data frame with one row per trial.
    #'   Columns: number, value, state, datetime_start, datetime_complete,
    #'   plus `params_<name>` for every hyperparameter and
    #'   `user_attrs_<name>` for every user attribute.
    trials_dataframe = function() {
      tlist <- private$.storage$get_all_trials(self$study_id)
      if (length(tlist) == 0) {
        return(data.frame(number = integer(0), value = numeric(0),
                          state = character(0), stringsAsFactors = FALSE))
      }

      rows <- lapply(tlist, function(t) {
        base <- list(
          number            = t$number,
          value             = if (is.null(t$value)) NA_real_ else t$value,
          state             = t$state,
          datetime_start    = as.character(t$datetime_start),
          datetime_complete = as.character(t$datetime_complete)
        )
        for (nm in names(t$params))
          base[[paste0("params_", nm)]] <- t$params[[nm]]
        for (nm in names(t$user_attrs))
          base[[paste0("user_attrs_", nm)]] <- t$user_attrs[[nm]]
        base
      })

      all_cols <- unique(unlist(lapply(rows, names)))
      rows <- lapply(rows, function(r) {
        missing <- setdiff(all_cols, names(r))
        for (col in missing) r[[col]] <- NA
        r[all_cols]
      })

      as.data.frame(do.call(rbind, lapply(rows, as.data.frame,
        stringsAsFactors = FALSE)), stringsAsFactors = FALSE)
    }
  ),

  active = list(
    trials = function() {
      private$.storage$get_all_trials(self$study_id)
    },
    best_trial = function() {
      complete <- private$.storage$get_all_trials(self$study_id, states = "complete")
      if (length(complete) == 0) return(NULL)
      vals <- sapply(complete, `[[`, "value")
      idx  <- if (self$direction == "minimize") which.min(vals) else which.max(vals)
      if (length(idx) == 0) return(NULL)
      complete[[idx]]
    },
    best_value = function() {
      bt <- self$best_trial
      if (is.null(bt)) {
        if (self$direction == "minimize") Inf else -Inf
      } else {
        bt$value
      }
    },
    best_params = function() {
      bt <- self$best_trial
      if (is.null(bt)) return(NULL)
      bt$params
    },
    directions = function() {
      private$.directions %||% self$direction
    },
    best_trials = function() {
      complete <- private$.storage$get_all_trials(self$study_id, states = "complete")
      mo <- Filter(function(t) !is.null(t$values), complete)
      if (length(mo) == 0) return(list())
      private$.pareto_front(mo)
    },
    n_trials = function() {
      length(private$.storage$get_all_trials(self$study_id))
    },
    user_attrs = function() {
      private$.storage$get_study_user_attrs(self$study_id)
    },
    storage_ref = function() private$.storage
  ),

  private = list(
    .sampler       = NULL,
    .pruner        = NULL,
    .storage       = NULL,
    .queued_params = NULL,
    .stopped       = FALSE,
    .directions    = NULL,

    .optimize_sequential = function(func, n_trials, timeout, catch, callbacks) {
      start     <- proc.time()[["elapsed"]]
      completed <- 0L
      while (completed < n_trials && !private$.stopped) {
        if (!is.null(timeout) &&
            proc.time()[["elapsed"]] - start > timeout) break

        trial_id <- private$.storage$create_trial(self$study_id)

        if (length(private$.queued_params) > 0) {
          queued <- private$.queued_params[[1]]
          private$.queued_params <- private$.queued_params[-1]
          for (pname in names(queued)) {
            val  <- queued[[pname]]
            dist <- private$.infer_dist(val)
            private$.storage$set_trial_param(self$study_id, trial_id, pname, dist, val)
          }
        }

        already_set <- private$.storage$get_trial(self$study_id, trial_id)$params
        rel_space   <- private$.sampler$infer_relative_search_space(self, NULL)
        rel_space   <- rel_space[setdiff(names(rel_space), names(already_set))]
        if (length(rel_space) > 0) {
          rel_params <- private$.sampler$sample_relative(self, NULL, rel_space, rel_space)
          for (pname in names(rel_params)) {
            private$.storage$set_trial_param(
              self$study_id, trial_id, pname, rel_space[[pname]], rel_params[[pname]])
          }
        }

        trial <- Trial$new(
          trial_id = trial_id, study_id = self$study_id,
          storage  = private$.storage, pruner  = private$.pruner,
          sampler  = private$.sampler, study   = self
        )

        tryCatch(
          {
            value_raw <- func(trial)
            if (!is.numeric(value_raw))
              stop("Objective must return a numeric value or vector, got: ", class(value_raw))
            if (!is.null(private$.directions)) {
              if (length(value_raw) != length(private$.directions))
                stop("Objective returned ", length(value_raw), " values but study has ",
                     length(private$.directions), " directions.")
              private$.storage$set_trial_values(self$study_id, trial_id, value_raw)
              private$.storage$set_trial_state(
                self$study_id, trial_id, "complete", value = value_raw[[1]])
            } else {
              if (length(value_raw) != 1L)
                stop("Objective must return a single numeric value, got length: ",
                     length(value_raw))
              private$.storage$set_trial_state(
                self$study_id, trial_id, "complete", value = value_raw)
            }
          },
          roptuna_trial_pruned = function(e) {
            private$.storage$set_trial_state(self$study_id, trial_id, "pruned")
          },
          error = function(e) {
            if (any(sapply(catch, function(cls) inherits(e, cls)))) {
              private$.storage$set_trial_state(self$study_id, trial_id, "failed")
            } else {
              stop(e)
            }
          }
        )

        if (!is.null(callbacks)) {
          snap <- private$.storage$get_trial(self$study_id, trial_id)
          for (cb in callbacks) cb(self, snap)
        }
        completed <- completed + 1L
      }
    },

    .optimize_parallel = function(func, n_trials, timeout, catch, callbacks, n_jobs) {
      if (!requireNamespace("parallel", quietly = TRUE))
        stop("Package 'parallel' is required for n_jobs > 1.")

      start     <- proc.time()[["elapsed"]]
      completed <- 0L
      directions <- private$.directions

      cl <- parallel::makeCluster(n_jobs, type = "PSOCK")
      on.exit(parallel::stopCluster(cl), add = TRUE)

      while (completed < n_trials && !private$.stopped) {
        if (!is.null(timeout) && proc.time()[["elapsed"]] - start > timeout) break
        batch_size <- min(n_jobs, n_trials - completed)

        # Sample all batch trials sequentially in main process
        batch <- vector("list", batch_size)
        for (i in seq_len(batch_size)) {
          trial_id <- private$.storage$create_trial(self$study_id)
          if (length(private$.queued_params) > 0) {
            queued <- private$.queued_params[[1]]
            private$.queued_params <- private$.queued_params[-1]
            for (pname in names(queued)) {
              val  <- queued[[pname]]
              dist <- private$.infer_dist(val)
              private$.storage$set_trial_param(self$study_id, trial_id, pname, dist, val)
            }
          }
          already_set <- private$.storage$get_trial(self$study_id, trial_id)$params
          rel_space   <- private$.sampler$infer_relative_search_space(self, NULL)
          rel_space   <- rel_space[setdiff(names(rel_space), names(already_set))]
          if (length(rel_space) > 0) {
            rel_params <- private$.sampler$sample_relative(self, NULL, rel_space, rel_space)
            for (pname in names(rel_params)) {
              private$.storage$set_trial_param(
                self$study_id, trial_id, pname, rel_space[[pname]], rel_params[[pname]])
            }
          }
          snap <- private$.storage$get_trial(self$study_id, trial_id)
          batch[[i]] <- list(trial_id = trial_id, params = snap$params)
        }

        # Evaluate batch in parallel using frozen trial proxies.
        # Pre-sampled params are served directly; others are sampled randomly in worker.
        results <- parallel::parLapply(cl, batch, function(item) {
          sampled <- item$params
          frozen <- list(
            suggest_float = function(name, low, high, log = FALSE, step = NULL) {
              if (!is.null(sampled[[name]])) return(sampled[[name]])
              v <- if (log) exp(runif(1, log(low), log(high))) else runif(1, low, high)
              sampled[[name]] <<- v; v
            },
            suggest_int = function(name, low, high, log = FALSE, step = 1L) {
              if (!is.null(sampled[[name]])) return(sampled[[name]])
              v <- as.integer(sample(seq(low, high, by = step), 1L))
              sampled[[name]] <<- v; v
            },
            suggest_categorical = function(name, choices) {
              if (!is.null(sampled[[name]])) return(sampled[[name]])
              v <- sample(choices, 1L)[[1]]
              sampled[[name]] <<- v; v
            },
            suggest_uniform     = function(name, low, high) {
              if (!is.null(sampled[[name]])) return(sampled[[name]])
              v <- runif(1, low, high); sampled[[name]] <<- v; v
            },
            suggest_log_uniform = function(name, low, high) {
              if (!is.null(sampled[[name]])) return(sampled[[name]])
              v <- exp(runif(1, log(low), log(high))); sampled[[name]] <<- v; v
            },
            report        = function(...) invisible(NULL),
            should_prune  = function() FALSE,
            set_user_attr = function(...) invisible(NULL),
            number        = NA_integer_,
            params        = item$params
          )
          tryCatch(
            list(value = func(frozen), sampled = sampled, error = NULL),
            error = function(e) list(value = NULL, sampled = sampled,
                                     error = conditionMessage(e))
          )
        })

        # Record results in main process
        for (i in seq_along(batch)) {
          res      <- results[[i]]
          trial_id <- batch[[i]]$trial_id
          # Store any params sampled in the worker that weren't pre-filled
          if (!is.null(res$sampled)) {
            already <- batch[[i]]$params
            new_params <- setdiff(names(res$sampled), names(already))
            snap_dists <- private$.storage$get_trial(self$study_id, trial_id)$distributions
            for (pname in new_params) {
              val  <- res$sampled[[pname]]
              dist <- snap_dists[[pname]] %||% private$.infer_dist(val)
              private$.storage$set_trial_param(self$study_id, trial_id, pname, dist, val)
            }
          }
          if (!is.null(res$error)) {
            private$.storage$set_trial_state(self$study_id, trial_id, "failed")
            if (length(catch) == 0)
              warning("Parallel trial ", trial_id, " failed: ", res$error)
          } else {
            value <- res$value
            if (!is.null(directions) && length(value) > 1) {
              private$.storage$set_trial_values(self$study_id, trial_id, value)
              private$.storage$set_trial_state(
                self$study_id, trial_id, "complete", value = value[[1]])
            } else {
              private$.storage$set_trial_state(
                self$study_id, trial_id, "complete", value = as.numeric(value[[1]]))
            }
          }
          if (!is.null(callbacks)) {
            snap <- private$.storage$get_trial(self$study_id, trial_id)
            for (cb in callbacks) cb(self, snap)
          }
        }
        completed <- completed + batch_size
      }
    },

    .pareto_front = function(trials) {
      dirs <- private$.directions
      n <- length(trials)
      dominated <- logical(n)
      for (i in seq_len(n)) {
        for (j in seq_len(n)) {
          if (i == j || dominated[[j]]) next
          a <- trials[[i]]$values
          b <- trials[[j]]$values
          a_n <- ifelse(dirs == "minimize", a, -a)
          b_n <- ifelse(dirs == "minimize", b, -b)
          if (all(a_n <= b_n) && any(a_n < b_n)) dominated[[j]] <- TRUE
        }
      }
      trials[!dominated]
    },

    .infer_dist = function(val) {
      if (is.character(val))   return(categorical_distribution(val))
      if (is.logical(val))     return(categorical_distribution(c("TRUE", "FALSE")))
      if (is.integer(val))     return(int_distribution(val, val))
      lo <- min(val - abs(val) - 1, -1)
      hi <- max(val + abs(val) + 1,  1)
      float_distribution(lo, hi)
    }
  )
)
