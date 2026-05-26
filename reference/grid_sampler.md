# Create a grid sampler

Create a grid sampler

## Usage

``` r
grid_sampler(search_space)
```

## Arguments

- search_space:

  Named list of choice vectors:
  `list(lr = c(0.01, 0.1), n = c(32L, 64L))`.

## Value

A `GridSampler` R6 object.

## Examples

``` r
sampler <- grid_sampler(list(lr = c(0.01, 0.1), depth = c(3L, 5L)))
study <- create_study(sampler = sampler)
```
