# Create a Hyperband pruner

Create a Hyperband pruner

## Usage

``` r
hyperband_pruner(
  min_resource = 1L,
  reduction_factor = 3L,
  min_early_stopping_rate = 0L,
  n_brackets = 4L
)
```

## Arguments

- min_resource:

  Minimum steps before any pruning.

- reduction_factor:

  Halving factor (default 3).

- min_early_stopping_rate:

  Base rung offset (default 0).

- n_brackets:

  Number of SHA brackets (default 4).

## Value

A `HyperbandPruner` R6 object.
