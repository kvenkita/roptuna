# mlr3tuning adapter for roptuna

mlr3tuning adapter for roptuna

mlr3tuning adapter for roptuna

## Details

A class that uses roptuna to search hyperparameters inside an mlr3tuning
instance. Use with
[`mlr3tuning::ti()`](https://mlr3tuning.mlr-org.com/reference/ti.html)
and `tuner$optimize(instance)`.

## Methods

### Public methods

- [`TunerOptuna$new()`](#method-TunerOptuna-new)

- [`TunerOptuna$optimize()`](#method-TunerOptuna-optimize)

- [`TunerOptuna$clone()`](#method-TunerOptuna-clone)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    TunerOptuna$new(sampler = tpe_sampler(), pruner = NULL)

#### Arguments

- `sampler`:

  roptuna sampler (default: tpe_sampler()).

- `pruner`:

  roptuna pruner or NULL.

------------------------------------------------------------------------

### Method [`optimize()`](https://rdrr.io/r/stats/optimize.html)

Run optimization on an mlr3tuning TuningInstance.

#### Usage

    TunerOptuna$optimize(inst)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TunerOptuna$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires mlr3, mlr3tuning, paradox packages
library(mlr3); library(mlr3tuning); library(paradox)
tuner <- TunerOptuna$new(sampler = tpe_sampler(seed = 1L))
} # }
```
