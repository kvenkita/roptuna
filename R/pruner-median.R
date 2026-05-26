#' Create a median pruner
#' @param n_startup_trials Minimum completed trials before pruning activates.
#' @param n_warmup_steps Steps to skip before checking pruning.
#' @param interval_steps Check every N steps.
#' @return A `MedianPruner` R6 object.
#' @examples
#' pruner <- median_pruner(n_startup_trials = 5L, n_warmup_steps = 2L)
#' study <- create_study(pruner = pruner)
#' @export
median_pruner <- function(n_startup_trials = 5L, n_warmup_steps = 0L,
                          interval_steps = 1L) {
  MedianPruner$new(as.integer(n_startup_trials),
                   as.integer(n_warmup_steps),
                   as.integer(interval_steps))
}

MedianPruner <- R6::R6Class("MedianPruner",
  public = list(
    initialize = function(n_startup_trials, n_warmup_steps, interval_steps) {
      private$.n_startup <- n_startup_trials
      private$.n_warmup  <- n_warmup_steps
      private$.interval  <- interval_steps
    },
    prune = function(study, trial_snap) {
      iv <- trial_snap$intermediate_values
      if (length(iv) == 0) return(FALSE)
      step <- max(as.integer(names(iv)))
      if (step < private$.n_warmup) return(FALSE)
      if ((step %% private$.interval) != 0) return(FALSE)

      completed <- study$storage_ref$get_all_trials(
        study$study_id, states = "complete")
      if (length(completed) < private$.n_startup) return(FALSE)

      step_key <- as.character(step)
      ref_vals <- unlist(Filter(Negate(is.null),
        lapply(completed, function(t) t$intermediate_values[[step_key]])))
      if (length(ref_vals) == 0) return(FALSE)

      current <- iv[[step_key]]
      med <- stats::median(ref_vals)
      if (study$direction == "minimize") current > med else current < med
    }
  ),
  private = list(.n_startup = NULL, .n_warmup = NULL, .interval = NULL)
)
