# Trial object passed to the objective function

Trial object passed to the objective function

Trial object passed to the objective function

## Details

Created by the Study for each call to the objective. Do not instantiate
directly — always received as the argument to your objective function.

## Public fields

- `number`:

  Zero-based trial index within the study.

## Active bindings

- `params`:

  Named list of all suggested parameter values so far.

## Methods

### Public methods

- [`Trial$new()`](#method-Trial-new)

- [`Trial$suggest_float()`](#method-Trial-suggest_float)

- [`Trial$suggest_log_uniform()`](#method-Trial-suggest_log_uniform)

- [`Trial$suggest_int()`](#method-Trial-suggest_int)

- [`Trial$suggest_categorical()`](#method-Trial-suggest_categorical)

- [`Trial$report()`](#method-Trial-report)

- [`Trial$should_prune()`](#method-Trial-should_prune)

- [`Trial$set_user_attr()`](#method-Trial-set_user_attr)

- [`Trial$clone()`](#method-Trial-clone)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    Trial$new(trial_id, study_id, storage, pruner, sampler = NULL, study = NULL)

------------------------------------------------------------------------

### Method `suggest_float()`

Suggest a float hyperparameter.

#### Usage

    Trial$suggest_float(name, low, high, log = FALSE, step = NULL)

------------------------------------------------------------------------

### Method `suggest_log_uniform()`

Suggest a float in log-uniform space.

#### Usage

    Trial$suggest_log_uniform(name, low, high)

------------------------------------------------------------------------

### Method `suggest_int()`

Suggest an integer hyperparameter.

#### Usage

    Trial$suggest_int(name, low, high)

------------------------------------------------------------------------

### Method `suggest_categorical()`

Suggest a categorical hyperparameter.

#### Usage

    Trial$suggest_categorical(name, choices)

------------------------------------------------------------------------

### Method `report()`

Report an intermediate objective value (for pruning).

#### Usage

    Trial$report(value, step)

------------------------------------------------------------------------

### Method `should_prune()`

Returns TRUE if this trial should be pruned.

#### Usage

    Trial$should_prune()

------------------------------------------------------------------------

### Method `set_user_attr()`

Set a user attribute on this trial.

#### Usage

    Trial$set_user_attr(key, value)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Trial$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Trial objects are created automatically by study$optimize()
study <- create_study(direction = "minimize")
study$optimize(function(trial) {
  x <- trial$suggest_float("x", -5, 5)
  x^2
}, n_trials = 3)
```
