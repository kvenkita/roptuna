# Create a categorical distribution

Create a categorical distribution

## Usage

``` r
categorical_distribution(choices)
```

## Arguments

- choices:

  Character or numeric vector of choices.

## Value

An S3 object of class `roptuna_categorical_distribution`.

## Examples

``` r
categorical_distribution(c("adam", "sgd", "rmsprop"))
#> $choices
#> [1] "adam"    "sgd"     "rmsprop"
#> 
#> attr(,"class")
#> [1] "roptuna_categorical_distribution"
```
