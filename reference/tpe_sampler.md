# Create a TPE sampler

Create a TPE sampler

## Usage

``` r
tpe_sampler(
  n_startup_trials = 10L,
  gamma = 0.25,
  n_ei_candidates = 24L,
  seed = NULL
)
```

## Arguments

- n_startup_trials:

  Random trials before TPE activates.

- gamma:

  Fraction of trials treated as "good" (default 0.25).

- n_ei_candidates:

  Candidates sampled per parameter.

- seed:

  Random seed.

## Value

A `TpeSampler` R6 object.

## Examples

``` r
sampler <- tpe_sampler(n_startup_trials = 5L, seed = 42L)
study <- create_study(sampler = sampler)
```
