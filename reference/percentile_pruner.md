# Create a percentile pruner

Prunes a trial if its latest intermediate value is worse than the given
percentile of historical intermediate values at the same step.

## Usage

``` r
percentile_pruner(
  percentile,
  n_startup_trials = 5L,
  n_warmup_steps = 0L,
  interval_steps = 1L
)
```

## Arguments

- percentile:

  Percentile threshold (0–100). Trials below this percentile (for
  minimize) or above it (for maximize) are pruned.

- n_startup_trials:

  Minimum completed trials required before pruning activates.

- n_warmup_steps:

  Steps to skip at the start of each trial.

- interval_steps:

  Check every N steps.

## Value

A `PercentilePruner` R6 object.

## Examples

``` r
pruner <- percentile_pruner(percentile = 75)
study  <- create_study(pruner = pruner)
```
