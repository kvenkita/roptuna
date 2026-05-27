# Create a patient pruner

Wraps another pruner and only prunes when the wrapped pruner has been
recommending pruning AND there has been no improvement of at least
`min_delta` over the last `patience` reported steps.

## Usage

``` r
patient_pruner(wrapped_pruner, patience, min_delta = 0)
```

## Arguments

- wrapped_pruner:

  A pruner object (e.g.
  [`median_pruner()`](https://kvenkita.github.io/roptuna/reference/median_pruner.md)).

- patience:

  Number of recent steps with no improvement required before pruning.

- min_delta:

  Minimum improvement threshold (default 0).

## Value

A `PatientPruner` R6 object.

## Examples

``` r
pruner <- patient_pruner(median_pruner(), patience = 3L)
study  <- create_study(pruner = pruner)
```
