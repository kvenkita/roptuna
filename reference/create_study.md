# Create a new optimization study

Create a new optimization study

## Usage

``` r
create_study(
  direction = "minimize",
  sampler = NULL,
  pruner = NULL,
  storage = NULL,
  study_name = NULL,
  load_if_exists = FALSE,
  directions = NULL
)
```

## Arguments

- direction:

  "minimize" or "maximize".

- sampler:

  A sampler object (e.g.
  [`tpe_sampler()`](https://kvenkita.github.io/roptuna/reference/tpe_sampler.md)).
  Defaults to
  [`random_sampler()`](https://kvenkita.github.io/roptuna/reference/random_sampler.md).

- pruner:

  A pruner object (e.g.
  [`median_pruner()`](https://kvenkita.github.io/roptuna/reference/median_pruner.md)).
  NULL disables pruning.

- storage:

  A storage backend. Defaults to `InMemoryStorage$new()`.

- study_name:

  Character name. Auto-generated if NULL.

- load_if_exists:

  For SQLite: if TRUE, load existing study with this name.

## Value

A `Study` R6 object.

## Examples

``` r
study <- create_study(direction = "minimize", sampler = tpe_sampler(seed = 1L))
study$optimize(function(trial) trial$suggest_float("x", -5, 5)^2, n_trials = 5)
study$best_value
#> [1] 0.5307613
```
