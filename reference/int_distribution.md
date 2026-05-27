# Create an integer distribution

Create an integer distribution

## Usage

``` r
int_distribution(low, high, log = FALSE, step = 1L)
```

## Arguments

- low:

  Lower bound (inclusive).

- high:

  Upper bound (inclusive).

- log:

  If TRUE, sample in log space. Both bounds must be positive integers.

- step:

  Step size between valid integers (default 1).

## Value

An S3 object of class `roptuna_int_distribution`.

## Examples

``` r
int_distribution(1L, 10L)
#> $low
#> [1] 1
#> 
#> $high
#> [1] 10
#> 
#> $log
#> [1] FALSE
#> 
#> $step
#> [1] 1
#> 
#> attr(,"class")
#> [1] "roptuna_int_distribution"
int_distribution(32L, 256L, step = 32L)
#> $low
#> [1] 32
#> 
#> $high
#> [1] 256
#> 
#> $log
#> [1] FALSE
#> 
#> $step
#> [1] 32
#> 
#> attr(,"class")
#> [1] "roptuna_int_distribution"
```
