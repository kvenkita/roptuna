#' Create a float (continuous) distribution
#' @param low Lower bound (inclusive).
#' @param high Upper bound (inclusive).
#' @param log If TRUE, sample in log space. Both bounds must be positive.
#' @param step If not NULL, round samples to this step size.
#' @return An S3 object of class `roptuna_float_distribution`.
#' @examples
#' float_distribution(0, 1)
#' float_distribution(1e-5, 1e-1, log = TRUE)
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
#' @param log If TRUE, sample in log space. Both bounds must be positive integers.
#' @param step Step size between valid integers (default 1).
#' @return An S3 object of class `roptuna_int_distribution`.
#' @examples
#' int_distribution(1L, 10L)
#' int_distribution(32L, 256L, step = 32L)
#' @export
int_distribution <- function(low, high, log = FALSE, step = 1L) {
  low  <- as.integer(low)
  high <- as.integer(high)
  step <- as.integer(step)
  if (low > high) stop("low must be less than or equal to high")
  if (log && low <= 0L) stop("int_distribution with log=TRUE requires positive bounds")
  if (step < 1L) stop("step must be >= 1")
  structure(list(low = low, high = high, log = log, step = step),
            class = "roptuna_int_distribution")
}

#' Create a categorical distribution
#' @param choices Character or numeric vector of choices.
#' @return An S3 object of class `roptuna_categorical_distribution`.
#' @examples
#' categorical_distribution(c("adam", "sgd", "rmsprop"))
#' @export
categorical_distribution <- function(choices) {
  if (length(choices) < 1) stop("categorical_distribution requires at least one choice")
  structure(list(choices = choices), class = "roptuna_categorical_distribution")
}

dist_to_list <- function(d) {
  if (inherits(d, "roptuna_float_distribution")) {
    dl <- list(name = "FloatDistribution", low = d$low, high = d$high, log = d$log)
    if (!is.null(d$step)) dl$step <- d$step
    dl
  } else if (inherits(d, "roptuna_int_distribution")) {
    list(name = "IntDistribution", low = d$low, high = d$high,
         log = d$log, step = d$step)
  } else {
    list(name = "CategoricalDistribution", choices = d$choices)
  }
}

dist_from_list <- function(x) {
  switch(x$name,
    FloatDistribution = float_distribution(x$low, x$high,
      log  = isTRUE(x$log),
      step = if (length(x$step) == 1) x$step else NULL),
    IntDistribution = int_distribution(x$low, x$high,
      log  = isTRUE(x$log),
      step = if (!is.null(x$step) && length(x$step) == 1) as.integer(x$step) else 1L),
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
    if (d$log) {
      val <- exp(stats::runif(1, log(d$low), log(d$high)))
      as.integer(max(d$low, min(d$high, round(val))))
    } else if (d$step > 1L) {
      steps <- seq(d$low, d$high, by = d$step)
      as.integer(sample(steps, 1))
    } else {
      if (d$low == d$high) return(d$low)
      sample.int(d$high - d$low + 1L, 1L) + d$low - 1L
    }
  } else {
    sample(d$choices, 1)
  }
}
