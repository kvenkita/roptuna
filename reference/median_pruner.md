# Create a median pruner

Create a median pruner

## Usage

``` r
median_pruner(n_startup_trials = 5L, n_warmup_steps = 0L, interval_steps = 1L)
```

## Arguments

- n_startup_trials:

  Minimum completed trials before pruning activates.

- n_warmup_steps:

  Steps to skip before checking pruning.

- interval_steps:

  Check every N steps.

## Value

A `MedianPruner` R6 object.

## Examples

``` r
pruner <- median_pruner(n_startup_trials = 5L, n_warmup_steps = 2L)
study <- create_study(pruner = pruner)
```
