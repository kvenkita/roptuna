# Study R6 class for hyperparameter optimization

Study R6 class for hyperparameter optimization

Study R6 class for hyperparameter optimization

## Details

Manages the optimization loop. Create via
[`create_study()`](https://kvenkita.github.io/roptuna/reference/create_study.md).

## Methods

### Public methods

- [`Study$new()`](#method-Study-new)

- [`Study$optimize()`](#method-Study-optimize)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    Study$new(study_id, study_name, direction, sampler, pruner, storage)

------------------------------------------------------------------------

### Method [`optimize()`](https://rdrr.io/r/stats/optimize.html)

Run the objective function for n_trials iterations.

#### Usage

    Study$optimize(func, n_trials = 100, timeout = NULL, catch = character())

## Examples

``` r
study <- create_study(direction = "minimize")
study$optimize(function(trial) trial$suggest_float("x", -5, 5)^2, n_trials = 5)
study$best_value
#> [1] 0.1785757
```
