
# roptuna — Hyperparameter Optimization for R

<!-- badges: start -->

[![R-CMD-check](https://github.com/kvenkita/roptuna/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kvenkita/roptuna/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`roptuna` is an R implementation of the [Optuna](https://optuna.org/)
hyperparameter optimization framework (Akiba et al. 2019). It provides
define-by-run search spaces, Tree-structured Parzen Estimator (TPE)
sampling, trial pruning, and persistent study storage via SQLite.

## Installation

``` r
# install.packages("remotes")
remotes::install_github("kvenkita/roptuna")
```

## Quick start

``` r
library(roptuna)

objective <- function(trial) {
  x <- trial$suggest_float("x", -10, 10)
  y <- trial$suggest_int("y", 1L, 5L)
  (x - 2)^2 + y
}

study <- create_study(direction = "minimize", sampler = tpe_sampler())
study$optimize(objective, n_trials = 50)

study$best_value
study$best_params
autoplot(study, type = "history")
```

## Citation

Akiba, T., Sano, S., Yanase, T., Ohta, T., & Koyama, M. (2019). Optuna:
A Next-generation Hyperparameter Optimization Framework. *KDD 2019*.
<https://doi.org/10.1145/3292500.3330701>
