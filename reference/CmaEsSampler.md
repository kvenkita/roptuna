# CMA-ES sampler R6 class

See
[`cmaes_sampler()`](https://kvenkita.github.io/roptuna/reference/cmaes_sampler.md)
for the recommended constructor.

## Methods

### Public methods

- [`CmaEsSampler$new()`](#method-CmaEsSampler-new)

- [`CmaEsSampler$infer_relative_search_space()`](#method-CmaEsSampler-infer_relative_search_space)

- [`CmaEsSampler$sample_relative()`](#method-CmaEsSampler-sample_relative)

- [`CmaEsSampler$sample_independent()`](#method-CmaEsSampler-sample_independent)

- [`CmaEsSampler$clone()`](#method-CmaEsSampler-clone)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    CmaEsSampler$new(n_startup_trials, sigma0, seed)

------------------------------------------------------------------------

### Method `infer_relative_search_space()`

#### Usage

    CmaEsSampler$infer_relative_search_space(study, trial)

------------------------------------------------------------------------

### Method `sample_relative()`

#### Usage

    CmaEsSampler$sample_relative(study, trial, search_space, all_params)

------------------------------------------------------------------------

### Method `sample_independent()`

#### Usage

    CmaEsSampler$sample_independent(study, trial, param_name, dist)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    CmaEsSampler$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
