#' Create a float (continuous) distribution
#' @param low Lower bound (inclusive).
#' @param high Upper bound (inclusive).
#' @param log If TRUE, sample in log space. Both bounds must be positive.
#' @param step If not NULL, round samples to this step size.
#' @export
float_distribution <- function(low, high, log = FALSE, step = NULL) {
  if (low >= high) stop("low must be less than high")
  if (log && low <= 0) stop("float_distribution with log=TRUE requires positive bounds")
  structure(
    list(low = low, high = high, log = log, step = step),
    class = "roptuna_float_distribution"
  )
}

#' Create an integer distribution
#' @param low Lower bound (inclusive).
#' @param high Upper bound (inclusive).
#' @export
int_distribution <- function(low, high) {
  low <- as.integer(low); high <- as.integer(high)
  if (low >= high) stop("low must be less than high")
  structure(list(low = low, high = high), class = "roptuna_int_distribution")
}

#' Create a categorical distribution
#' @param choices Character or numeric vector of choices.
#' @export
categorical_distribution <- function(choices) {
  if (length(choices) < 1) stop("categorical_distribution requires at least one choice")
  structure(list(choices = choices), class = "roptuna_categorical_distribution")
}

dist_to_list <- function(d) {
  if (inherits(d, "roptuna_float_distribution")) {
    list(name = "FloatDistribution", low = d$low, high = d$high,
         log = d$log, step = d$step)
  } else if (inherits(d, "roptuna_int_distribution")) {
    list(name = "IntDistribution", low = d$low, high = d$high)
  } else {
    list(name = "CategoricalDistribution", choices = d$choices)
  }
}

dist_from_list <- function(x) {
  switch(x$name,
    FloatDistribution       = float_distribution(x$low, x$high,
                                                 log = isTRUE(x$log), step = x$step),
    IntDistribution         = int_distribution(x$low, x$high),
    CategoricalDistribution = categorical_distribution(x$choices)
  )
}

dist_sample_random <- function(d) {
  if (inherits(d, "roptuna_float_distribution")) {
    if (d$log) {
      exp(stats::runif(1, log(d$low), log(d$high)))
    } else if (!is.null(d$step)) {
      steps <- seq(d$low, d$high, by = d$step)
      sample(steps, 1)
    } else {
      stats::runif(1, d$low, d$high)
    }
  } else if (inherits(d, "roptuna_int_distribution")) {
    sample(d$low:d$high, 1)
  } else {
    sample(d$choices, 1)
  }
}
