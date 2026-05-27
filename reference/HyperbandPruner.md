# Hyperband pruner R6 class

See
[`hyperband_pruner()`](https://kvenkita.github.io/roptuna/reference/hyperband_pruner.md)
for the recommended constructor.

## Methods

### Public methods

- [`HyperbandPruner$new()`](#method-HyperbandPruner-new)

- [`HyperbandPruner$prune()`](#method-HyperbandPruner-prune)

- [`HyperbandPruner$clone()`](#method-HyperbandPruner-clone)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    HyperbandPruner$new(
      min_resource,
      reduction_factor,
      min_early_stopping_rate,
      n_brackets
    )

------------------------------------------------------------------------

### Method `prune()`

#### Usage

    HyperbandPruner$prune(study, trial_snap)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    HyperbandPruner$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
