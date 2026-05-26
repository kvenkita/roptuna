# Create a successive halving (ASHA) pruner

Create a successive halving (ASHA) pruner

## Usage

``` r
successive_halving_pruner(
  min_resource = 1L,
  reduction_factor = 3L,
  min_early_stopping_rate = 0L
)
```

## Arguments

- min_resource:

  Minimum steps before any pruning.

- reduction_factor:

  Halving factor eta (default 3).

- min_early_stopping_rate:

  Rung offset s_min.

## Value

A `SuccessiveHalvingPruner` R6 object.

## Examples

``` r
pruner <- successive_halving_pruner(min_resource = 3L, reduction_factor = 3L)
study <- create_study(pruner = pruner)
```
