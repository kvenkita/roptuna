# Create a CMA-ES sampler for continuous optimization

Create a CMA-ES sampler for continuous optimization

## Usage

``` r
cmaes_sampler(n_startup_trials = 1L, sigma0 = NULL, seed = NULL)
```

## Arguments

- n_startup_trials:

  Random trials before CMA-ES activates (default 1).

- sigma0:

  Initial step size (default 0.3 \* (high - low) per param).

- seed:

  Random seed (NULL for random).

## Value

A `CmaEsSampler` R6 object.
