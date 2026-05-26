#' Create a successive halving (ASHA) pruner
#' @param min_resource Minimum steps before any pruning.
#' @param reduction_factor Halving factor eta (default 3).
#' @param min_early_stopping_rate Rung offset s_min.
#' @return A `SuccessiveHalvingPruner` R6 object.
#' @examples
#' pruner <- successive_halving_pruner(min_resource = 3L, reduction_factor = 3L)
#' study <- create_study(pruner = pruner)
#' @export
successive_halving_pruner <- function(min_resource = 1L, reduction_factor = 3L,
                                      min_early_stopping_rate = 0L) {
  SuccessiveHalvingPruner$new(as.integer(min_resource),
                               as.integer(reduction_factor),
                               as.integer(min_early_stopping_rate))
}

SuccessiveHalvingPruner <- R6::R6Class("SuccessiveHalvingPruner",
  public = list(
    initialize = function(min_resource, reduction_factor, min_early_stopping_rate) {
      private$.min_r <- min_resource
      private$.eta   <- reduction_factor
      private$.s_min <- min_early_stopping_rate
    },
    prune = function(study, trial_snap) {
      iv <- trial_snap$intermediate_values
      if (length(iv) == 0) return(FALSE)
      step <- max(as.integer(names(iv)))
      if (step < private$.min_r) return(FALSE)

      rungs <- private$.min_r * (private$.eta ^ (private$.s_min + 0:10))
      rungs <- rungs[rungs <= step]
      if (length(rungs) == 0) return(FALSE)
      rung_key <- as.character(as.integer(max(rungs)))

      completed <- study$storage_ref$get_all_trials(
        study$study_id, states = "complete")
      ref_vals <- unlist(Filter(Negate(is.null),
        lapply(completed, function(t) t$intermediate_values[[rung_key]])))
      if (length(ref_vals) == 0) return(FALSE)

      current <- iv[[as.character(step)]]
      top_k <- ceiling(length(ref_vals) / private$.eta)
      threshold <- sort(ref_vals,
        decreasing = (study$direction == "maximize"))[[top_k]]

      if (study$direction == "minimize") current > threshold else current < threshold
    }
  ),
  private = list(.min_r = NULL, .eta = NULL, .s_min = NULL)
)
