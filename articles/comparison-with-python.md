# Comparison with Python Optuna

This vignette shows side-by-side R (roptuna) and Python (Optuna) code
for common tasks.

## Create a study

**Python:**

``` python
import optuna
study = optuna.create_study(direction="minimize")
```

**R:**

``` r

library(roptuna)
study <- create_study(direction = "minimize")
```

## Define and run an objective

**Python:**

``` python
def objective(trial):
    x = trial.suggest_float("x", -10, 10)
    y = trial.suggest_int("y", 1, 5)
    return (x - 2)**2 + y

study.optimize(objective, n_trials=50)
```

**R:**

``` r

objective <- function(trial) {
  x <- trial$suggest_float("x", -10, 10)
  y <- trial$suggest_int("y", 1L, 5L)
  (x - 2)^2 + y
}
study$optimize(objective, n_trials = 50)
```

## Retrieve best parameters

**Python:**

``` python
study.best_value
study.best_params
```

**R:**

``` r

study$best_value
study$best_params
```

## Categorical parameters

**Python:**

``` python
optimizer = trial.suggest_categorical("optimizer", ["adam", "sgd", "rmsprop"])
```

**R:**

``` r

optimizer <- trial$suggest_categorical("optimizer", c("adam", "sgd", "rmsprop"))
```

## SQLite persistence

**Python:**

``` python
study = optuna.create_study(
    storage="sqlite:///my_study.db",
    study_name="my_study"
)
```

**R:**

``` r

study <- create_study(
  storage    = sqlite_storage("my_study.sqlite"),
  study_name = "my_study"
)
```

roptuna’s SQLite schema matches Python Optuna’s schema exactly, enabling
cross-language study sharing.

## References

Python Optuna documentation: <https://optuna.readthedocs.io/>
