# Signal that the current trial should be pruned

Call inside an objective function when `trial$should_prune()` returns
TRUE. The study's optimize loop catches this condition and records the
trial as pruned.

## Usage

``` r
stop_prune()
```

## Examples

``` r
if (FALSE) { # \dontrun{
if (trial$should_prune()) stop_prune()
} # }
```
