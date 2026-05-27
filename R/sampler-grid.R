#' Create a grid sampler
#' @param search_space Named list of choice vectors: `list(lr = c(0.01, 0.1), n = c(32L, 64L))`.
#' @return A `GridSampler` R6 object.
#' @examples
#' sampler <- grid_sampler(list(lr = c(0.01, 0.1), depth = c(3L, 5L)))
#' study <- create_study(sampler = sampler)
#' @export
grid_sampler <- function(search_space) GridSampler$new(search_space)

GridSampler <- R6::R6Class("GridSampler",
  public = list(
    initialize = function(search_space) {
      private$.grid <- do.call(expand.grid,
        c(search_space, list(stringsAsFactors = FALSE)))
    },
    sample_independent = function(study, trial, param_name, distribution) {
      dist_sample_random(distribution)
    },
    sample_relative = function(study, trial, distributions, search_space) {
      # Derive used rows from completed+running trials rather than in-memory state,
      # so this works correctly across SQLite restarts.
      all_trials <- study$storage_ref$get_all_trials(study$study_id)
      param_cols <- names(private$.grid)

      used_rows <- logical(nrow(private$.grid))
      for (t in all_trials) {
        if (length(t$params) == 0) next
        for (i in which(!used_rows)) {
          if (all(sapply(param_cols, function(col) {
            pval <- t$params[[col]]
            !is.null(pval) &&
              isTRUE(all.equal(pval, private$.grid[i, col, drop = TRUE]))
          }))) {
            used_rows[i] <- TRUE
            break
          }
        }
      }

      unused <- which(!used_rows)
      if (length(unused) == 0) {
        return(stats::setNames(
          lapply(names(distributions), function(n) dist_sample_random(distributions[[n]])),
          names(distributions)))
      }
      as.list(private$.grid[unused[[1]], , drop = FALSE])
    },
    infer_relative_search_space = function(study, trial) {
      stats::setNames(
        lapply(names(private$.grid), function(n)
          categorical_distribution(unique(private$.grid[[n]]))),
        names(private$.grid))
    }
  ),
  private = list(.grid = NULL)
)
