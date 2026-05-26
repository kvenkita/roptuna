#' Create a random sampler
#' @param seed Optional random seed.
#' @return A `RandomSampler` R6 object.
#' @examples
#' sampler <- random_sampler(seed = 42L)
#' study <- create_study(sampler = sampler)
#' @export
random_sampler <- function(seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  RandomSampler$new()
}

RandomSampler <- R6::R6Class("RandomSampler",
  public = list(
    sample_independent = function(study, trial, param_name, distribution) {
      dist_sample_random(distribution)
    },
    sample_relative = function(study, trial, distributions, search_space) {
      stats::setNames(
        lapply(names(distributions), function(n) dist_sample_random(distributions[[n]])),
        names(distributions)
      )
    },
    infer_relative_search_space = function(study, trial) list()
  )
)
