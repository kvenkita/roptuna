# Study R6 class for hyperparameter optimization

Study R6 class for hyperparameter optimization

Study R6 class for hyperparameter optimization

## Details

Manages the optimization loop. Create via
[`create_study()`](https://kvenkita.github.io/roptuna/reference/create_study.md).

## Methods

### Public methods

- [`Study$new()`](#method-Study-new)

- [`Study$optimize()`](#method-Study-optimize)

- [`Study$ask()`](#method-Study-ask)

- [`Study$tell()`](#method-Study-tell)

- [`Study$stop()`](#method-Study-stop)

- [`Study$set_user_attr()`](#method-Study-set_user_attr)

- [`Study$enqueue_trial()`](#method-Study-enqueue_trial)

- [`Study$add_trial()`](#method-Study-add_trial)

- [`Study$add_trials()`](#method-Study-add_trials)

- [`Study$trials_dataframe()`](#method-Study-trials_dataframe)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    Study$new(
      study_id,
      study_name,
      direction,
      sampler,
      pruner,
      storage,
      directions = NULL
    )

------------------------------------------------------------------------

### Method [`optimize()`](https://rdrr.io/r/stats/optimize.html)

Run the objective function for n_trials iterations.

#### Usage

    Study$optimize(
      func,
      n_trials = 100,
      timeout = NULL,
      catch = character(),
      callbacks = NULL,
      n_jobs = 1L
    )

#### Arguments

- `func`:

  Objective function taking a Trial and returning a numeric scalar.

- `n_trials`:

  Number of trials to run.

- `timeout`:

  Wall-clock timeout in seconds (NULL = no limit).

- `catch`:

  Character vector of additional condition classes to catch as failed
  trials.

- `callbacks`:

  List of functions `function(study, trial_snapshot)` called after each
  trial.

- `n_jobs`:

  Number of parallel workers (\>1 requires `parallel` package).

------------------------------------------------------------------------

### Method `ask()`

Create a trial without evaluating the objective (decoupled API). Call
tell() with the returned trial to record the result.

#### Usage

    Study$ask()

#### Returns

A `Trial` R6 object ready for parameter suggestion.

------------------------------------------------------------------------

### Method `tell()`

Record the result of a trial created by ask().

#### Usage

    Study$tell(trial, values, state = "complete")

#### Arguments

- `trial`:

  A `Trial` object returned by ask().

- `values`:

  Numeric scalar or vector (multi-obj), or NULL for failed/pruned.

- `state`:

  "complete", "failed", or "pruned".

------------------------------------------------------------------------

### Method [`stop()`](https://rdrr.io/r/base/stop.html)

Signal the optimize loop to stop after the current trial.

#### Usage

    Study$stop()

------------------------------------------------------------------------

### Method `set_user_attr()`

Set a user attribute on the study.

#### Usage

    Study$set_user_attr(key, value)

------------------------------------------------------------------------

### Method `enqueue_trial()`

Queue a trial with fixed parameter values. The next call to optimize()
will use these values instead of sampling.

#### Usage

    Study$enqueue_trial(params)

#### Arguments

- `params`:

  Named list of parameter name -\> value.

------------------------------------------------------------------------

### Method `add_trial()`

Add a completed (or failed/pruned) trial directly to the study.

#### Usage

    Study$add_trial(trial_info)

#### Arguments

- `trial_info`:

  Named list with: `params` (named list), `value` (numeric), and
  optionally `state` ("complete"), `distributions` (named list),
  `user_attrs` (named list), `intermediate_values` (named numeric).

------------------------------------------------------------------------

### Method `add_trials()`

Add multiple trials at once. See `add_trial()`.

#### Usage

    Study$add_trials(trial_list)

#### Arguments

- `trial_list`:

  List of trial_info lists.

------------------------------------------------------------------------

### Method `trials_dataframe()`

Return all trials as a data frame with one row per trial. Columns:
number, value, state, datetime_start, datetime_complete, plus
`params_<name>` for every hyperparameter and `user_attrs_<name>` for
every user attribute.

#### Usage

    Study$trials_dataframe()

## Examples

``` r
study <- create_study(direction = "minimize")
study$optimize(function(trial) trial$suggest_float("x", -5, 5)^2, n_trials = 5)
study$best_value
#> [1] 1.015276
```
