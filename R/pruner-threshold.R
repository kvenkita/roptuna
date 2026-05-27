#' Create a threshold pruner
#'
#' Prunes a trial immediately when its latest intermediate value falls outside
#' a fixed numeric interval. Useful when you know the acceptable range of
#' intermediate metrics (e.g., validation loss must stay below 2.0).
#'
#' @param lower If not NULL, prune when the intermediate value falls below this.
#' @param upper If not NULL, prune when the intermediate value rises above this.
#' @param n_warmup_steps Steps to skip at the start of each trial before checking.
#' @param interval_steps Check every N steps (default 1).
#' @return A `ThresholdPruner` R6 object.
#' @examples
#' pruner <- threshold_pruner(upper = 2.0)
#' study  <- create_study(pruner = pruner)
#' @export
threshold_pruner <- function(lower = NULL, upper = NULL,
                             n_warmup_steps = 0L, interval_steps = 1L) {
  if (is.null(lower) && is.null(upper))
    stop("At least one of lower or upper must be specified.")
  ThresholdPruner$new(lower, upper,
                      as.integer(n_warmup_steps),
                      as.integer(interval_steps))
}

ThresholdPruner <- R6::R6Class("ThresholdPruner",
  public = list(
    initialize = function(lower, upper, n_warmup_steps, interval_steps) {
      private$.lower    <- lower
      private$.upper    <- upper
      private$.n_warmup <- n_warmup_steps
      private$.interval <- interval_steps
    },
    prune = function(study, trial_snap) {
      iv <- trial_snap$intermediate_values
      if (length(iv) == 0) return(FALSE)
      step <- max(as.integer(names(iv)))
      if (step < private$.n_warmup) return(FALSE)
      if ((step %% private$.interval) != 0) return(FALSE)
      current <- iv[[as.character(step)]]
      if (!is.null(private$.lower) && current < private$.lower) return(TRUE)
      if (!is.null(private$.upper) && current > private$.upper) return(TRUE)
      FALSE
    }
  ),
  private = list(.lower = NULL, .upper = NULL, .n_warmup = NULL, .interval = NULL)
)
