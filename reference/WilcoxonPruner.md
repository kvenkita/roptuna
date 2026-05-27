# Wilcoxon pruner R6 class

See
[`wilcoxon_pruner()`](https://kvenkita.github.io/roptuna/reference/wilcoxon_pruner.md)
for the recommended constructor.

## Methods

### Public methods

- [`WilcoxonPruner$new()`](#method-WilcoxonPruner-new)

- [`WilcoxonPruner$prune()`](#method-WilcoxonPruner-prune)

- [`WilcoxonPruner$clone()`](#method-WilcoxonPruner-clone)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    WilcoxonPruner$new(p_threshold, n_startup_trials)

------------------------------------------------------------------------

### Method `prune()`

#### Usage

    WilcoxonPruner$prune(study, trial_snap)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    WilcoxonPruner$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
