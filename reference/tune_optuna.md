# Run a tidymodels workflow using roptuna for hyperparameter search

Run a tidymodels workflow using roptuna for hyperparameter search

## Usage

``` r
tune_optuna(
  object,
  resamples,
  suggest_fn,
  n_trials = 20,
  direction = "minimize",
  sampler = tpe_sampler(),
  pruner = NULL,
  storage = NULL,
  study_name = NULL,
  metrics = NULL,
  control = NULL
)
```

## Arguments

- object:

  A `workflow` object with `tune::tune()` placeholders.

- resamples:

  An `rset` object (e.g. from `rsample::vfold_cv()`).

- suggest_fn:

  A function `function(trial) -> named list` mapping trial suggestions
  to parameter names. Parameter names must match those in the workflow's
  tuning parameters.

- n_trials:

  Number of trials to run.

- direction:

  "minimize" or "maximize".

- sampler:

  An roptuna sampler. Defaults to
  [`tpe_sampler()`](https://kvenkita.github.io/roptuna/reference/tpe_sampler.md).

- pruner:

  An roptuna pruner or NULL.

- storage:

  An roptuna storage backend or NULL (in-memory).

- study_name:

  Character study name.

- metrics:

  A `yardstick::metric_set`. Defaults to workflow's default metric.

- control:

  A `tune::control_resamples()` object.

## Value

An roptuna `Study` object. Use `study$best_params` to retrieve the best
hyperparameters.

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires tune, parsnip, rsample, workflows, yardstick packages
library(tune); library(parsnip); library(rsample); library(workflows)
wf <- workflow() |>
  add_model(decision_tree(cost_complexity = tune()) |> set_engine("rpart") |>
              set_mode("classification")) |>
  add_formula(Species ~ .)
folds <- vfold_cv(iris, v = 3)
study <- tune_optuna(wf, folds,
  suggest_fn = function(trial) list(cost_complexity = trial$suggest_float("cp", 0, 0.1)),
  n_trials = 5, direction = "minimize")
study$best_params
} # }
```
