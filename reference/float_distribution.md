# Create a float (continuous) distribution

Create a float (continuous) distribution

## Usage

``` r
float_distribution(low, high, log = FALSE, step = NULL)
```

## Arguments

- low:

  Lower bound (inclusive).

- high:

  Upper bound (inclusive).

- log:

  If TRUE, sample in log space. Both bounds must be positive.

- step:

  If not NULL, round samples to this step size.

## Value

An S3 object of class `roptuna_float_distribution`.

## Examples

``` r
float_distribution(0, 1)
#> $low
#> [1] 0
#> 
#> $high
#> [1] 1
#> 
#> $log
#> [1] FALSE
#> 
#> $step
#> NULL
#> 
#> attr(,"class")
#> [1] "roptuna_float_distribution"
float_distribution(1e-5, 1e-1, log = TRUE)
#> $low
#> [1] 1e-05
#> 
#> $high
#> [1] 0.1
#> 
#> $log
#> [1] TRUE
#> 
#> $step
#> NULL
#> 
#> attr(,"class")
#> [1] "roptuna_float_distribution"
```
