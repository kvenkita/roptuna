#' Create a Wilcoxon pruner
#' @param p_threshold P-value threshold below which to prune (default 0.1).
#' @param n_startup_trials Minimum completed trials before pruning activates (default 5).
#' @return A `WilcoxonPruner` R6 object.
#' @export
wilcoxon_pruner <- function(p_threshold = 0.1, n_startup_trials = 5L) {
  WilcoxonPruner$new(p_threshold, as.integer(n_startup_trials))
}

#' @export
WilcoxonPruner <- R6::R6Class("WilcoxonPruner",
  public = list(
    initialize = function(p_threshold, n_startup_trials) {
      private$.p_thresh  <- p_threshold
      private$.n_startup <- n_startup_trials
    },
    prune = function(study, trial_snap) {
      iv <- trial_snap$intermediate_values
      if (length(iv) == 0) return(FALSE)
      step     <- max(as.integer(names(iv)))
      step_key <- as.character(step)
      completed <- study$storage_ref$get_all_trials(study$study_id, states = "complete")
      if (length(completed) < private$.n_startup) return(FALSE)
      ref_vals <- unlist(Filter(Negate(is.null),
        lapply(completed, function(t) t$intermediate_values[[step_key]])))
      if (length(ref_vals) < 2L) return(FALSE)
      current <- iv[[step_key]]
      alt <- if (study$direction == "minimize") "greater" else "less"
      test <- tryCatch(
        stats::wilcox.test(current, ref_vals, alternative = alt, exact = FALSE),
        error = function(e) list(p.value = 1.0))
      test$p.value < private$.p_thresh
    }
  ),
  private = list(.p_thresh = NULL, .n_startup = NULL)
)
