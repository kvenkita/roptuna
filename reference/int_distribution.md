# Create an integer distribution

Create an integer distribution

## Usage

``` r
int_distribution(low, high)
```

## Arguments

- low:

  Lower bound (inclusive).

- high:

  Upper bound (inclusive).

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
#> attr(,"class")
#> [1] "roptuna_int_distribution"
```
