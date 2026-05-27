#' Create a Hyperband pruner
#' @param min_resource Minimum steps before any pruning.
#' @param reduction_factor Halving factor (default 3).
#' @param min_early_stopping_rate Base rung offset (default 0).
#' @param n_brackets Number of SHA brackets (default 4).
#' @return A `HyperbandPruner` R6 object.
#' @export
hyperband_pruner <- function(min_resource = 1L, reduction_factor = 3L,
                             min_early_stopping_rate = 0L, n_brackets = 4L) {
  HyperbandPruner$new(as.integer(min_resource), as.integer(reduction_factor),
                      as.integer(min_early_stopping_rate), as.integer(n_brackets))
}

#' Hyperband pruner R6 class
#'
#' See [hyperband_pruner()] for the recommended constructor.
#' @export
HyperbandPruner <- R6::R6Class("HyperbandPruner",
  public = list(
    initialize = function(min_resource, reduction_factor, min_early_stopping_rate, n_brackets) {
      private$.brackets <- lapply(seq_len(n_brackets) - 1L, function(s) {
        SuccessiveHalvingPruner$new(min_resource, reduction_factor,
                                   min_early_stopping_rate + s)
      })
    },
    prune = function(study, trial_snap) {
      bracket_idx <- (trial_snap$number %% length(private$.brackets)) + 1L
      private$.brackets[[bracket_idx]]$prune(study, trial_snap)
    }
  ),
  private = list(.brackets = NULL)
)
