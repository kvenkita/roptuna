#' Create a patient pruner
#'
#' Wraps another pruner and only prunes when the wrapped pruner has been
#' recommending pruning AND there has been no improvement of at least
#' `min_delta` over the last `patience` reported steps.
#'
#' @param wrapped_pruner A pruner object (e.g. `median_pruner()`).
#' @param patience Number of recent steps with no improvement required before pruning.
#' @param min_delta Minimum improvement threshold (default 0).
#' @return A `PatientPruner` R6 object.
#' @examples
#' pruner <- patient_pruner(median_pruner(), patience = 3L)
#' study  <- create_study(pruner = pruner)
#' @export
patient_pruner <- function(wrapped_pruner, patience, min_delta = 0.0) {
  PatientPruner$new(wrapped_pruner, as.integer(patience), min_delta)
}

PatientPruner <- R6::R6Class("PatientPruner",
  public = list(
    initialize = function(wrapped_pruner, patience, min_delta) {
      private$.wrapped  <- wrapped_pruner
      private$.patience <- patience
      private$.delta    <- min_delta
    },
    prune = function(study, trial_snap) {
      if (!private$.wrapped$prune(study, trial_snap)) return(FALSE)

      iv <- trial_snap$intermediate_values
      if (length(iv) <= private$.patience) return(FALSE)

      steps  <- sort(as.integer(names(iv)))
      window <- tail(steps, private$.patience + 1L)
      vals   <- sapply(as.character(window), function(s) iv[[s]])

      ref    <- vals[[1]]
      recent <- vals[-1]
      improved <- if (study$direction == "minimize") {
        any((ref - recent) >= private$.delta)
      } else {
        any((recent - ref) >= private$.delta)
      }
      !improved
    }
  ),
  private = list(.wrapped = NULL, .patience = NULL, .delta = NULL)
)
