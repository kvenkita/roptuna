#' Trial object passed to the objective function
#'
#' Created by the Study for each call to the objective. Do not instantiate
#' directly — always received as the argument to your objective function.
#'
#' @examples
#' # Trial objects are created automatically by study$optimize()
#' study <- create_study(direction = "minimize")
#' study$optimize(function(trial) {
#'   x <- trial$suggest_float("x", -5, 5)
#'   x^2
#' }, n_trials = 3)
#' @export
Trial <- R6::R6Class("Trial",
  public = list(
    #' @field number Zero-based trial index within the study.
    number = NULL,

    initialize = function(trial_id, study_id, storage, pruner, sampler = NULL,
                          study = NULL) {
      private$.trial_id <- trial_id
      private$.study_id <- study_id
      private$.storage  <- storage
      private$.pruner   <- pruner
      private$.sampler  <- sampler
      private$.study    <- study
      snap <- storage$get_trial(study_id, trial_id)
      self$number <- snap$number
    },

    #' @description Suggest a float hyperparameter.
    suggest_float = function(name, low, high, log = FALSE, step = NULL) {
      private$.suggest(name, float_distribution(low, high, log = log, step = step))
    },

    #' @description Suggest a float (alias for suggest_float, for compatibility).
    suggest_uniform = function(name, low, high) {
      self$suggest_float(name, low, high)
    },

    #' @description Suggest a float in log-uniform space.
    suggest_log_uniform = function(name, low, high) {
      self$suggest_float(name, low, high, log = TRUE)
    },

    #' @description Suggest an integer hyperparameter.
    #' @param name Parameter name.
    #' @param low Lower bound (inclusive).
    #' @param high Upper bound (inclusive).
    #' @param log If TRUE, sample in log space (both bounds must be positive).
    #' @param step Step size between valid integers (default 1).
    suggest_int = function(name, low, high, log = FALSE, step = 1L) {
      private$.suggest(name,
        int_distribution(as.integer(low), as.integer(high),
                         log = log, step = as.integer(step)))
    },

    #' @description Suggest a categorical hyperparameter.
    suggest_categorical = function(name, choices) {
      private$.suggest(name, categorical_distribution(choices))
    },

    #' @description Report an intermediate objective value (for pruning).
    report = function(value, step) {
      private$.storage$set_trial_intermediate_value(
        private$.study_id, private$.trial_id, as.integer(step), value
      )
    },

    #' @description Returns TRUE if this trial should be pruned.
    should_prune = function() {
      if (is.null(private$.pruner)) return(FALSE)
      snap <- private$.storage$get_trial(private$.study_id, private$.trial_id)
      private$.pruner$prune(private$.study, snap)
    },

    #' @description Set a user attribute on this trial.
    set_user_attr = function(key, value) {
      private$.storage$set_trial_user_attr(
        private$.study_id, private$.trial_id, key, value
      )
    }
  ),

  active = list(
    #' @field trial_id Internal trial identifier.
    trial_id = function() private$.trial_id,
    #' @field params Named list of all suggested parameter values so far.
    params = function() {
      private$.storage$get_trial(
        private$.study_id, private$.trial_id
      )$params
    },
    #' @field distributions Named list of distributions for suggested parameters.
    distributions = function() {
      private$.storage$get_trial(
        private$.study_id, private$.trial_id
      )$distributions
    },
    #' @field intermediate_values Named list of reported intermediate values.
    intermediate_values = function() {
      private$.storage$get_trial(
        private$.study_id, private$.trial_id
      )$intermediate_values
    },
    #' @field user_attrs Named list of user attributes set on this trial.
    user_attrs = function() {
      private$.storage$get_trial(
        private$.study_id, private$.trial_id
      )$user_attrs
    }
  ),

  private = list(
    .trial_id = NULL,
    .study_id = NULL,
    .storage  = NULL,
    .pruner   = NULL,
    .sampler  = NULL,
    .study    = NULL,

    .suggest = function(name, distribution) {
      snap <- private$.storage$get_trial(private$.study_id, private$.trial_id)
      if (!is.null(snap$params[[name]])) return(snap$params[[name]])

      if (!is.null(private$.sampler)) {
        value <- private$.sampler$sample_independent(
          private$.study, self, name, distribution
        )
      } else {
        value <- dist_sample_random(distribution)
      }

      private$.storage$set_trial_param(
        private$.study_id, private$.trial_id, name, distribution, value
      )
      value
    }
  )
)
