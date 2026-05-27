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
- [`cmaes_sampler()`](https://kvenkita.github.io/roptuna/reference/cmaes_sampler.md)
  : Create a CMA-ES sampler for continuous optimization
- [`CmaEsSampler`](https://kvenkita.github.io/roptuna/reference/CmaEsSampler.md)
  : CMA-ES sampler R6 class
- [`nsgaii_sampler()`](https://kvenkita.github.io/roptuna/reference/nsgaii_sampler.md)
  : Create an NSGA-II sampler for multi-objective optimization
- [`NSGAIISampler`](https://kvenkita.github.io/roptuna/reference/NSGAIISampler.md)
  : NSGA-II sampler R6 class

## Pruners

- [`median_pruner()`](https://kvenkita.github.io/roptuna/reference/median_pruner.md)
  : Create a median pruner
- [`successive_halving_pruner()`](https://kvenkita.github.io/roptuna/reference/successive_halving_pruner.md)
  : Create a successive halving (ASHA) pruner
- [`hyperband_pruner()`](https://kvenkita.github.io/roptuna/reference/hyperband_pruner.md)
  : Create a Hyperband pruner
- [`HyperbandPruner`](https://kvenkita.github.io/roptuna/reference/HyperbandPruner.md)
  : Hyperband pruner R6 class
- [`wilcoxon_pruner()`](https://kvenkita.github.io/roptuna/reference/wilcoxon_pruner.md)
  : Create a Wilcoxon pruner
- [`WilcoxonPruner`](https://kvenkita.github.io/roptuna/reference/WilcoxonPruner.md)
  : Wilcoxon pruner R6 class
- [`percentile_pruner()`](https://kvenkita.github.io/roptuna/reference/percentile_pruner.md)
  : Create a percentile pruner
- [`threshold_pruner()`](https://kvenkita.github.io/roptuna/reference/threshold_pruner.md)
  : Create a threshold pruner
- [`patient_pruner()`](https://kvenkita.github.io/roptuna/reference/patient_pruner.md)
  : Create a patient pruner
- [`stop_prune()`](https://kvenkita.github.io/roptuna/reference/stop_prune.md)
  : Signal that the current trial should be pruned

## Storage

- [`sqlite_storage()`](https://kvenkita.github.io/roptuna/reference/sqlite_storage.md)
  : SQLite storage backend (schema matches Python Optuna)
- [`SqliteStorage`](https://kvenkita.github.io/roptuna/reference/SqliteStorage.md)
  : SQLite storage R6 class (schema matches Python Optuna)
- [`journal_storage()`](https://kvenkita.github.io/roptuna/reference/journal_storage.md)
  : Create a file-based journal storage backend
- [`JournalStorage`](https://kvenkita.github.io/roptuna/reference/JournalStorage.md)
  : Journal storage R6 class (file-based, append-only)
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
