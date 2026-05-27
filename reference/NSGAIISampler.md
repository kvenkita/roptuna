# NSGA-II sampler R6 class

See
[`nsgaii_sampler()`](https://kvenkita.github.io/roptuna/reference/nsgaii_sampler.md)
for the recommended constructor.

## Methods

### Public methods

- [`NSGAIISampler$new()`](#method-NSGAIISampler-new)

- [`NSGAIISampler$infer_relative_search_space()`](#method-NSGAIISampler-infer_relative_search_space)

- [`NSGAIISampler$sample_relative()`](#method-NSGAIISampler-sample_relative)

- [`NSGAIISampler$sample_independent()`](#method-NSGAIISampler-sample_independent)

- [`NSGAIISampler$clone()`](#method-NSGAIISampler-clone)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    NSGAIISampler$new(population_size, eta_c, eta_m, mutation_prob, seed)

------------------------------------------------------------------------

### Method `infer_relative_search_space()`

#### Usage

    NSGAIISampler$infer_relative_search_space(study, trial)

------------------------------------------------------------------------

### Method `sample_relative()`

#### Usage

    NSGAIISampler$sample_relative(study, trial, search_space, all_params)

------------------------------------------------------------------------

### Method `sample_independent()`

#### Usage

    NSGAIISampler$sample_independent(study, trial, param_name, dist)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    NSGAIISampler$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
