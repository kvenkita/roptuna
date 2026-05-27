# SQLite storage R6 class (schema matches Python Optuna)

See
[`sqlite_storage()`](https://kvenkita.github.io/roptuna/reference/sqlite_storage.md)
for the recommended constructor.

## Methods

### Public methods

- [`SqliteStorage$new()`](#method-SqliteStorage-new)

- [`SqliteStorage$create_study()`](#method-SqliteStorage-create_study)

- [`SqliteStorage$get_study_directions()`](#method-SqliteStorage-get_study_directions)

- [`SqliteStorage$get_study()`](#method-SqliteStorage-get_study)

- [`SqliteStorage$set_study_user_attr()`](#method-SqliteStorage-set_study_user_attr)

- [`SqliteStorage$get_study_user_attrs()`](#method-SqliteStorage-get_study_user_attrs)

- [`SqliteStorage$find_study()`](#method-SqliteStorage-find_study)

- [`SqliteStorage$create_trial()`](#method-SqliteStorage-create_trial)

- [`SqliteStorage$set_trial_state()`](#method-SqliteStorage-set_trial_state)

- [`SqliteStorage$set_trial_param()`](#method-SqliteStorage-set_trial_param)

- [`SqliteStorage$set_trial_values()`](#method-SqliteStorage-set_trial_values)

- [`SqliteStorage$set_trial_intermediate_value()`](#method-SqliteStorage-set_trial_intermediate_value)

- [`SqliteStorage$set_trial_user_attr()`](#method-SqliteStorage-set_trial_user_attr)

- [`SqliteStorage$get_trial()`](#method-SqliteStorage-get_trial)

- [`SqliteStorage$get_all_trials()`](#method-SqliteStorage-get_all_trials)

- [`SqliteStorage$clone()`](#method-SqliteStorage-clone)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    SqliteStorage$new(path)

------------------------------------------------------------------------

### Method [`create_study()`](https://kvenkita.github.io/roptuna/reference/create_study.md)

#### Usage

    SqliteStorage$create_study(study_name, direction, directions = NULL)

------------------------------------------------------------------------

### Method `get_study_directions()`

#### Usage

    SqliteStorage$get_study_directions(study_id)

------------------------------------------------------------------------

### Method `get_study()`

#### Usage

    SqliteStorage$get_study(study_id)

------------------------------------------------------------------------

### Method `set_study_user_attr()`

#### Usage

    SqliteStorage$set_study_user_attr(study_id, key, value)

------------------------------------------------------------------------

### Method `get_study_user_attrs()`

#### Usage

    SqliteStorage$get_study_user_attrs(study_id)

------------------------------------------------------------------------

### Method `find_study()`

#### Usage

    SqliteStorage$find_study(study_name)

------------------------------------------------------------------------

### Method `create_trial()`

#### Usage

    SqliteStorage$create_trial(study_id)

------------------------------------------------------------------------

### Method `set_trial_state()`

#### Usage

    SqliteStorage$set_trial_state(
      study_id,
      trial_id,
      state,
      value = NULL,
      datetime_complete = NULL
    )

------------------------------------------------------------------------

### Method `set_trial_param()`

#### Usage

    SqliteStorage$set_trial_param(
      study_id,
      trial_id,
      param_name,
      distribution,
      value
    )

------------------------------------------------------------------------

### Method `set_trial_values()`

#### Usage

    SqliteStorage$set_trial_values(study_id, trial_id, values)

------------------------------------------------------------------------

### Method `set_trial_intermediate_value()`

#### Usage

    SqliteStorage$set_trial_intermediate_value(study_id, trial_id, step, value)

------------------------------------------------------------------------

### Method `set_trial_user_attr()`

#### Usage

    SqliteStorage$set_trial_user_attr(study_id, trial_id, key_name, value)

------------------------------------------------------------------------

### Method `get_trial()`

#### Usage

    SqliteStorage$get_trial(study_id, trial_id)

------------------------------------------------------------------------

### Method `get_all_trials()`

#### Usage

    SqliteStorage$get_all_trials(study_id, states = NULL)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SqliteStorage$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
storage <- SqliteStorage$new(tempfile(fileext = ".sqlite"))
} # }
```
