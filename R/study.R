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
                         study_name = NULL, load_if_exists = FALSE) {
  direction <- match.arg(direction, c("minimize", "maximize"))
  if (is.null(storage))    storage    <- InMemoryStorage$new()
  if (is.null(sampler))    sampler    <- random_sampler()
  if (is.null(study_name)) study_name <- paste0("study-", format(Sys.time(), "%Y%m%d%H%M%S"))

  if (load_if_exists) {
    existing <- storage$find_study(study_name)
    study_id <- if (!is.null(existing)) existing else storage$create_study(study_name, direction)
  } else {
    study_id <- storage$create_study(study_name, direction)
  }

  Study$new(
    study_id   = study_id,
    study_name = study_name,
    direction  = direction,
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
                          sampler, pruner, storage) {
      self$study_id   <- study_id
      self$study_name <- study_name
      self$direction  <- direction
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
    optimize = function(func, n_trials = 100, timeout = NULL,
                        catch = character(), callbacks = NULL) {
      start     <- proc.time()[["elapsed"]]
      completed <- 0L
      private$.stopped <- FALSE

      while (completed < n_trials && !private$.stopped) {
        if (!is.null(timeout) &&
            proc.time()[["elapsed"]] - start > timeout) break

        trial_id <- private$.storage$create_trial(self$study_id)

        # Consume enqueued params first (pre-fill storage so suggest_* returns them)
        if (length(private$.queued_params) > 0) {
          queued <- private$.queued_params[[1]]
          private$.queued_params <- private$.queued_params[-1]
          for (pname in names(queued)) {
            val  <- queued[[pname]]
            dist <- private$.infer_dist(val)
            private$.storage$set_trial_param(
              self$study_id, trial_id, pname, dist, val)
          }
        }

        # Pre-fill relative params (GridSampler, TPE), skip already-set params
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
          trial_id = trial_id,
          study_id = self$study_id,
          storage  = private$.storage,
          pruner   = private$.pruner,
          sampler  = private$.sampler,
          study    = self
        )

        tryCatch(
          {
            value <- func(trial)
            if (!is.numeric(value) || length(value) != 1)
              stop("Objective must return a single numeric value, got: ", class(value))
            private$.storage$set_trial_state(
              self$study_id, trial_id, "complete", value = value)
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
        self$study_id, tid, state, value = trial_info$value)
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
