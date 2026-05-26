#' Create a grid sampler
#' @param search_space Named list of choice vectors: `list(lr = c(0.01, 0.1), n = c(32L, 64L))`.
#' @export
grid_sampler <- function(search_space) GridSampler$new(search_space)

GridSampler <- R6::R6Class("GridSampler",
  public = list(
    initialize = function(search_space) {
      private$.grid <- do.call(expand.grid,
        c(search_space, list(stringsAsFactors = FALSE)))
      private$.used <- rep(FALSE, nrow(private$.grid))
    },
    sample_independent = function(study, trial, param_name, distribution) {
      dist_sample_random(distribution)
    },
    sample_relative = function(study, trial, distributions, search_space) {
      unused <- which(!private$.used)
      if (length(unused) == 0) {
        return(stats::setNames(
          lapply(names(distributions), function(n) dist_sample_random(distributions[[n]])),
          names(distributions)))
      }
      idx <- unused[[1]]
      private$.used[[idx]] <- TRUE
      as.list(private$.grid[idx, , drop = FALSE])
    },
    infer_relative_search_space = function(study, trial) {
      stats::setNames(
        lapply(names(private$.grid), function(n)
          categorical_distribution(unique(private$.grid[[n]]))),
        names(private$.grid))
    }
  ),
  private = list(.grid = NULL, .used = NULL)
)
