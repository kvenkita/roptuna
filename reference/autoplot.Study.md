# Plot an roptuna study

Plot an roptuna study

## Usage

``` r
# S3 method for class 'Study'
autoplot(object, type = "history", params = NULL, ...)
```

## Arguments

- object:

  A `Study` R6 object.

- type:

  One of `"history"`, `"parallel_coordinate"`, `"param_importance"`,
  `"intermediate_values"`, `"contour"`, `"slice"`, `"edf"`.

- params:

  For `"contour"`: character vector of exactly two parameter names. For
  `"slice"`: character vector of parameter names to include (default:
  all).

- ...:

  Unused.

## Value

A `ggplot2` object.

## Examples

``` r
study <- create_study(direction = "minimize", sampler = tpe_sampler(seed = 1L))
study$optimize(function(trial) {
  x <- trial$suggest_float("x", -5, 5)
  y <- trial$suggest_int("y", 1L, 3L)
  x^2 + y
}, n_trials = 10)
ggplot2::autoplot(study, type = "history")
```
