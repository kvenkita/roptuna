# Create a Wilcoxon pruner

Create a Wilcoxon pruner

## Usage

``` r
wilcoxon_pruner(p_threshold = 0.1, n_startup_trials = 5L)
```

## Arguments

- p_threshold:

  P-value threshold below which to prune (default 0.1).

- n_startup_trials:

  Minimum completed trials before pruning activates (default 5).

## Value

A `WilcoxonPruner` R6 object.
