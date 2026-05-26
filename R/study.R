#' Create a new optimization study
#'
#' @param direction "minimize" or "maximize".
#' @param sampler A sampler object (e.g. `tpe_sampler()`). Defaults to `random_sampler()`.
#' @param pruner A pruner object (e.g. `median_pruner()`). NULL disables pruning.
#' @param storage A storage backend. Defaults to `InMemoryStorage$new()`.
#' @param study_name Character name. Auto-generated if NULL.
#' @param load_if_exists For SQLite: if TRUE, load existing study with this name.
#' @return A `Study` R6 object.
#' @export
create_study <- function(direction = "minimize", sampler = NULL,
                         pruner = NULL, storage = NULL,
                         study_name = NULL, load_if_exists = FALSE) {
  direction <- match.arg(direction, c("minimize", "maximize"))
  if (is.null(storage))    storage    <- InMemoryStorage$new()
  if (is.null(sampler))    sampler    <- random_sampler()
  if (is.null(study_name)) study_name <- paste0("study-", format(Sys.time(), "%Y%m%d%H%M%S"))

  study_id <- storage$create_study(study_name, direction)

  Study$new(
    study_id   = study_id,
    study_name = study_name,
    direction  = direction,
    sampler    = sampler,
    pruner     = pruner,
    storage    = storage
  )
}

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
      private$.sampler <- sampler
      private$.pruner  <- pruner
      private$.storage <- storage
    },

    #' @description Run the objective function for n_trials iterations.
    optimize = function(func, n_trials = 100, timeout = NULL, catch = character()) {
      start <- proc.time()[["elapsed"]]
      completed <- 0L

      while (completed < n_trials) {
        if (!is.null(timeout) &&
            proc.time()[["elapsed"]] - start > timeout) break

        trial_id <- private$.storage$create_trial(self$study_id)

        # Pre-fill relative params (used by GridSampler, TPE, etc.)
        rel_space <- private$.sampler$infer_relative_search_space(self, NULL)
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
              stop("Objective must return a single numeric value, got: ",
                   class(value))
            private$.storage$set_trial_state(
              self$study_id, trial_id, "complete", value = value
            )
          },
          roptuna_trial_pruned = function(e) {
            private$.storage$set_trial_state(
              self$study_id, trial_id, "pruned"
            )
          },
          error = function(e) {
            if (any(sapply(catch, function(cls) inherits(e, cls)))) {
              private$.storage$set_trial_state(
                self$study_id, trial_id, "failed"
              )
            } else {
              stop(e)
            }
          }
        )
        completed <- completed + 1L
      }
      invisible(self)
    }
  ),

  active = list(
    trials = function() {
      private$.storage$get_all_trials(self$study_id)
    },
    best_trial = function() {
      complete <- private$.storage$get_all_trials(
        self$study_id, states = "complete"
      )
      if (length(complete) == 0) return(NULL)
      vals <- sapply(complete, `[[`, "value")
      if (self$direction == "minimize") {
        complete[[which.min(vals)]]
      } else {
        complete[[which.max(vals)]]
      }
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
    storage_ref = function() private$.storage
  ),

  private = list(
    .sampler = NULL,
    .pruner  = NULL,
    .storage = NULL
  )
)
