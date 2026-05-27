# Create an NSGA-II sampler for multi-objective optimization

Create an NSGA-II sampler for multi-objective optimization

## Usage

``` r
nsgaii_sampler(
  population_size = 50L,
  eta_c = 20,
  eta_m = 20,
  mutation_prob = NULL,
  seed = NULL
)
```

## Arguments

- population_size:

  Size of parent population (default 50).

- eta_c:

  SBX crossover parameter (default 20).

- eta_m:

  Polynomial mutation parameter (default 20).

- mutation_prob:

  Per-param mutation probability (default 1/n_params).

- seed:

  Random seed (NULL for random).

## Value

An `NSGAIISampler` R6 object.
