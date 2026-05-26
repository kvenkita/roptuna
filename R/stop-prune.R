#' Signal that the current trial should be pruned
#'
#' Call inside an objective function when `trial$should_prune()` returns TRUE.
#' The study's optimize loop catches this condition and records the trial as pruned.
#'
#' @examples
#' \dontrun{
#' if (trial$should_prune()) stop_prune()
#' }
#' @export
stop_prune <- function() {
  cond <- structure(
    class = c("roptuna_trial_pruned", "error", "condition"),
    list(message = "Trial pruned", call = sys.call(-1))
  )
  stop(cond)
}
