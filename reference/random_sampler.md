# Create a random sampler

Create a random sampler

## Usage

``` r
random_sampler(seed = NULL)
```

## Arguments

- seed:

  Optional random seed.

## Value

A `RandomSampler` R6 object.

## Examples

``` r
sampler <- random_sampler(seed = 42L)
study <- create_study(sampler = sampler)
```
