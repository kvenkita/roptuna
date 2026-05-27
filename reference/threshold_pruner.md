# Create a threshold pruner

Prunes a trial immediately when its latest intermediate value falls
outside a fixed numeric interval. Useful when you know the acceptable
range of intermediate metrics (e.g., validation loss must stay below
2.0).

## Usage

``` r
threshold_pruner(
  lower = NULL,
  upper = NULL,
  n_warmup_steps = 0L,
  interval_steps = 1L
)
```

## Arguments

- lower:

  If not NULL, prune when the intermediate value falls below this.

- upper:

  If not NULL, prune when the intermediate value rises above this.

- n_warmup_steps:

  Steps to skip at the start of each trial before checking.

- interval_steps:

  Check every N steps (default 1).

## Value

A `ThresholdPruner` R6 object.

## Examples

``` r
pruner <- threshold_pruner(upper = 2.0)
study  <- create_study(pruner = pruner)
```
