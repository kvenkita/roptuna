# Journal storage R6 class (file-based, append-only)

See
[`journal_storage()`](https://kvenkita.github.io/roptuna/reference/journal_storage.md)
for the recommended constructor.

## Methods

### Public methods

- [`JournalStorage$new()`](#method-JournalStorage-new)

- [`JournalStorage$create_study()`](#method-JournalStorage-create_study)

- [`JournalStorage$get_study()`](#method-JournalStorage-get_study)

- [`JournalStorage$get_study_directions()`](#method-JournalStorage-get_study_directions)

- [`JournalStorage$set_study_user_attr()`](#method-JournalStorage-set_study_user_attr)

- [`JournalStorage$get_study_user_attrs()`](#method-JournalStorage-get_study_user_attrs)

- [`JournalStorage$find_study()`](#method-JournalStorage-find_study)

- [`JournalStorage$create_trial()`](#method-JournalStorage-create_trial)

- [`JournalStorage$set_trial_state()`](#method-JournalStorage-set_trial_state)

- [`JournalStorage$set_trial_values()`](#method-JournalStorage-set_trial_values)

- [`JournalStorage$set_trial_param()`](#method-JournalStorage-set_trial_param)

- [`JournalStorage$set_trial_intermediate_value()`](#method-JournalStorage-set_trial_intermediate_value)

- [`JournalStorage$set_trial_user_attr()`](#method-JournalStorage-set_trial_user_attr)

- [`JournalStorage$get_trial()`](#method-JournalStorage-get_trial)

- [`JournalStorage$get_all_trials()`](#method-JournalStorage-get_all_trials)

- [`JournalStorage$clone()`](#method-JournalStorage-clone)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    JournalStorage$new(path)

------------------------------------------------------------------------

### Method [`create_study()`](https://kvenkita.github.io/roptuna/reference/create_study.md)

#### Usage

    JournalStorage$create_study(study_name, direction, directions = NULL)

------------------------------------------------------------------------

### Method `get_study()`

#### Usage

    JournalStorage$get_study(study_id)

------------------------------------------------------------------------

### Method `get_study_directions()`

#### Usage

    JournalStorage$get_study_directions(study_id)

------------------------------------------------------------------------

### Method `set_study_user_attr()`

#### Usage

    JournalStorage$set_study_user_attr(study_id, key, value)

------------------------------------------------------------------------

### Method `get_study_user_attrs()`

#### Usage

    JournalStorage$get_study_user_attrs(study_id)

------------------------------------------------------------------------

### Method `find_study()`

#### Usage

    JournalStorage$find_study(study_name)

------------------------------------------------------------------------

### Method `create_trial()`

#### Usage

    JournalStorage$create_trial(study_id)

------------------------------------------------------------------------

### Method `set_trial_state()`

#### Usage

    JournalStorage$set_trial_state(
      study_id,
      trial_id,
      state,
      value = NULL,
      datetime_complete = NULL
    )

------------------------------------------------------------------------

### Method `set_trial_values()`

#### Usage

    JournalStorage$set_trial_values(study_id, trial_id, values)

------------------------------------------------------------------------

### Method `set_trial_param()`

#### Usage

    JournalStorage$set_trial_param(
      study_id,
      trial_id,
      param_name,
      distribution,
      value
    )

------------------------------------------------------------------------

### Method `set_trial_intermediate_value()`

#### Usage

    JournalStorage$set_trial_intermediate_value(study_id, trial_id, step, value)

------------------------------------------------------------------------

### Method `set_trial_user_attr()`

#### Usage

    JournalStorage$set_trial_user_attr(study_id, trial_id, key_name, value)

------------------------------------------------------------------------

### Method `get_trial()`

#### Usage

    JournalStorage$get_trial(study_id, trial_id)

------------------------------------------------------------------------

### Method `get_all_trials()`

#### Usage

    JournalStorage$get_all_trials(study_id, states = NULL)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    JournalStorage$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
