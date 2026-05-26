# Getting Started with roptuna

## What is roptuna?

`roptuna` is an R port of the Python [Optuna](https://optuna.org/)
framework (Akiba et al. 2019). It automates hyperparameter search using
a define-by-run API: instead of defining a search space upfront, you
call `trial$suggest_*()` inside your objective function, and the sampler
adapts based on what has worked before.

## Define-by-run search spaces

The `trial` object passed to your objective exposes these methods:

- `trial$suggest_float(name, low, high, log = FALSE)` — continuous
  parameter
- `trial$suggest_int(name, low, high)` — integer parameter
- `trial$suggest_categorical(name, choices)` — categorical parameter
- `trial$suggest_log_uniform(name, low, high)` — log-uniform continuous

Parameters are named and idempotent: calling `suggest_float("lr", 0, 1)`
twice in the same trial always returns the same value.

## A complete example

``` r
library(roptuna)

objective <- function(trial) {
  x  <- trial$suggest_float("x", -5, 5)
  y  <- trial$suggest_int("y", 1L, 5L)
  x^2 + y
}

study <- create_study(direction = "minimize", sampler = tpe_sampler(seed = 42))
study$optimize(objective, n_trials = 50)

cat("Best value:", study$best_value, "\n")
cat("Best params:", "\n")
str(study$best_params)
```

## Pruning

Pruning stops unpromising trials early. Use `trial$report()` to report
intermediate values, check `trial$should_prune()`, and call
[`stop_prune()`](https://kvenkita.github.io/roptuna/reference/stop_prune.md)
to signal that the trial should be abandoned:

``` r
objective_with_pruning <- function(trial) {
  lr <- trial$suggest_float("lr", 1e-5, 1e-1, log = TRUE)

  for (epoch in 1:20) {
    loss <- simulate_training(lr, epoch)  # your training function
    trial$report(loss, step = epoch)
    if (trial$should_prune()) stop_prune()
  }
  loss
}

study <- create_study(
  direction = "minimize",
  sampler   = tpe_sampler(),
  pruner    = median_pruner(n_startup_trials = 5)
)
study$optimize(objective_with_pruning, n_trials = 30)
```

## Persistent studies with SQLite

Store trials in a file so you can resume later or share with colleagues:

``` r
study <- create_study(
  direction  = "minimize",
  storage    = sqlite_storage("my_study.sqlite"),
  study_name = "quadratic"
)
study$optimize(objective, n_trials = 20)

# Resume in a later session
study2 <- create_study(
  direction        = "minimize",
  storage          = sqlite_storage("my_study.sqlite"),
  study_name       = "quadratic",
  load_if_exists   = TRUE
)
study2$optimize(objective, n_trials = 20)  # adds 20 more trials
```

## Visualisation

``` r
autoplot(study, type = "history")           # best value over time
autoplot(study, type = "parallel_coordinate")  # parameter relationships
autoplot(study, type = "param_importance")  # which params matter most
```

## References

Akiba, T., Sano, S., Yanase, T., Ohta, T., & Koyama, M. (2019). Optuna:
A Next-generation Hyperparameter Optimization Framework. *KDD 2019*.
<https://doi.org/10.1145/3292500.3330701>
