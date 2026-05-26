# Package index

## Study

- [`create_study()`](https://kvenkita.github.io/roptuna/reference/create_study.md)
  : Create a new optimization study
- [`Study`](https://kvenkita.github.io/roptuna/reference/Study.md) :
  Study R6 class for hyperparameter optimization
- [`Trial`](https://kvenkita.github.io/roptuna/reference/Trial.md) :
  Trial object passed to the objective function

## Samplers

- [`random_sampler()`](https://kvenkita.github.io/roptuna/reference/random_sampler.md)
  : Create a random sampler
- [`grid_sampler()`](https://kvenkita.github.io/roptuna/reference/grid_sampler.md)
  : Create a grid sampler
- [`tpe_sampler()`](https://kvenkita.github.io/roptuna/reference/tpe_sampler.md)
  : Create a TPE sampler

## Pruners

- [`median_pruner()`](https://kvenkita.github.io/roptuna/reference/median_pruner.md)
  : Create a median pruner
- [`successive_halving_pruner()`](https://kvenkita.github.io/roptuna/reference/successive_halving_pruner.md)
  : Create a successive halving (ASHA) pruner
- [`stop_prune()`](https://kvenkita.github.io/roptuna/reference/stop_prune.md)
  : Signal that the current trial should be pruned

## Storage

- [`sqlite_storage()`](https://kvenkita.github.io/roptuna/reference/sqlite_storage.md)
  : SQLite storage backend (schema matches Python Optuna)
- [`SqliteStorage`](https://kvenkita.github.io/roptuna/reference/SqliteStorage.md)
  : SQLite storage R6 class (schema matches Python Optuna)
- [`InMemoryStorage`](https://kvenkita.github.io/roptuna/reference/InMemoryStorage.md)
  : In-memory storage backend for roptuna studies

## Distributions

- [`float_distribution()`](https://kvenkita.github.io/roptuna/reference/float_distribution.md)
  : Create a float (continuous) distribution
- [`int_distribution()`](https://kvenkita.github.io/roptuna/reference/int_distribution.md)
  : Create an integer distribution
- [`categorical_distribution()`](https://kvenkita.github.io/roptuna/reference/categorical_distribution.md)
  : Create a categorical distribution

## Adapters

- [`tune_optuna()`](https://kvenkita.github.io/roptuna/reference/tune_optuna.md)
  : Run a tidymodels workflow using roptuna for hyperparameter search
- [`TunerOptuna`](https://kvenkita.github.io/roptuna/reference/TunerOptuna.md)
  : mlr3tuning adapter for roptuna

## Visualization

- [`autoplot(`*`<Study>`*`)`](https://kvenkita.github.io/roptuna/reference/autoplot.Study.md)
  : Plot an roptuna study
