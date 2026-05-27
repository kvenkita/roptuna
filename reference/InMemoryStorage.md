# In-memory storage backend for roptuna studies

In-memory storage backend for roptuna studies

In-memory storage backend for roptuna studies

## Methods

### Public methods

- [`InMemoryStorage$new()`](#method-InMemoryStorage-new)

- [`InMemoryStorage$create_study()`](#method-InMemoryStorage-create_study)

- [`InMemoryStorage$get_study()`](#method-InMemoryStorage-get_study)

- [`InMemoryStorage$get_study_directions()`](#method-InMemoryStorage-get_study_directions)

- [`InMemoryStorage$set_study_user_attr()`](#method-InMemoryStorage-set_study_user_attr)

- [`InMemoryStorage$get_study_user_attrs()`](#method-InMemoryStorage-get_study_user_attrs)

- [`InMemoryStorage$create_trial()`](#method-InMemoryStorage-create_trial)

- [`InMemoryStorage$set_trial_state()`](#method-InMemoryStorage-set_trial_state)

- [`InMemoryStorage$set_trial_values()`](#method-InMemoryStorage-set_trial_values)

- [`InMemoryStorage$set_trial_param()`](#method-InMemoryStorage-set_trial_param)

- [`InMemoryStorage$set_trial_intermediate_value()`](#method-InMemoryStorage-set_trial_intermediate_value)

- [`InMemoryStorage$set_trial_user_attr()`](#method-InMemoryStorage-set_trial_user_attr)

- [`InMemoryStorage$get_trial()`](#method-InMemoryStorage-get_trial)

- [`InMemoryStorage$get_all_trials()`](#method-InMemoryStorage-get_all_trials)

- [`InMemoryStorage$find_study()`](#method-InMemoryStorage-find_study)

- [`InMemoryStorage$clone()`](#method-InMemoryStorage-clone)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    InMemoryStorage$new()

------------------------------------------------------------------------

### Method [`create_study()`](https://kvenkita.github.io/roptuna/reference/create_study.md)

#### Usage

    InMemoryStorage$create_study(study_name, direction, directions = NULL)

------------------------------------------------------------------------

### Method `get_study()`

#### Usage

    InMemoryStorage$get_study(study_id)

------------------------------------------------------------------------

### Method `get_study_directions()`

#### Usage

    InMemoryStorage$get_study_directions(study_id)

------------------------------------------------------------------------

### Method `set_study_user_attr()`

#### Usage

    InMemoryStorage$set_study_user_attr(study_id, key, value)

------------------------------------------------------------------------

### Method `get_study_user_attrs()`

#### Usage

    InMemoryStorage$get_study_user_attrs(study_id)

------------------------------------------------------------------------

### Method `create_trial()`

#### Usage

    InMemoryStorage$create_trial(study_id)

------------------------------------------------------------------------

### Method `set_trial_state()`

#### Usage

    InMemoryStorage$set_trial_state(
      study_id,
      trial_id,
      state,
      value = NULL,
      datetime_complete = NULL
    )

------------------------------------------------------------------------

### Method `set_trial_values()`

#### Usage

    InMemoryStorage$set_trial_values(study_id, trial_id, values)

------------------------------------------------------------------------

### Method `set_trial_param()`

#### Usage

    InMemoryStorage$set_trial_param(
      study_id,
      trial_id,
      param_name,
      distribution,
      value
    )

------------------------------------------------------------------------

### Method `set_trial_intermediate_value()`

#### Usage

    InMemoryStorage$set_trial_intermediate_value(study_id, trial_id, step, value)

------------------------------------------------------------------------

### Method `set_trial_user_attr()`

#### Usage

    InMemoryStorage$set_trial_user_attr(study_id, trial_id, key_name, value)

------------------------------------------------------------------------

### Method `get_trial()`

#### Usage

    InMemoryStorage$get_trial(study_id, trial_id)

------------------------------------------------------------------------

### Method `get_all_trials()`

#### Usage

    InMemoryStorage$get_all_trials(study_id, states = NULL)

------------------------------------------------------------------------

### Method `find_study()`

#### Usage

    InMemoryStorage$find_study(study_name)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    InMemoryStorage$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
storage <- InMemoryStorage$new()
```
