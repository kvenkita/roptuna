#' Create a file-based journal storage backend
#' @param path Path to the journal file (.jsonl).
#' @return A `JournalStorage` R6 object.
#' @examples
#' \dontrun{
#' storage <- journal_storage(tempfile(fileext = ".jsonl"))
#' study   <- create_study(storage = storage, study_name = "my_study")
#' }
#' @export
journal_storage <- function(path) JournalStorage$new(path)

#' Journal storage R6 class (file-based, append-only)
#'
#' See [journal_storage()] for the recommended constructor.
#' @export
JournalStorage <- R6::R6Class("JournalStorage",
  public = list(
    initialize = function(path) {
      private$.path <- path
      private$.mem  <- InMemoryStorage$new()
      if (file.exists(path)) private$.replay()
    },

    create_study = function(study_name, direction, directions = NULL) {
      sid <- private$.mem$create_study(study_name, direction, directions = directions)
      private$.append_op(list(
        op         = "create_study",
        study_name = study_name,
        direction  = direction,
        directions = if (is.null(directions)) NA else as.list(directions)
      ))
      sid
    },

    get_study = function(study_id) private$.mem$get_study(study_id),

    get_study_directions = function(study_id) private$.mem$get_study_directions(study_id),

    set_study_user_attr = function(study_id, key, value) {
      private$.mem$set_study_user_attr(study_id, key, value)
      private$.append_op(list(
        op = "set_study_user_attr", study_id = study_id, key = key, value = value))
    },

    get_study_user_attrs = function(study_id) private$.mem$get_study_user_attrs(study_id),

    find_study = function(study_name) private$.mem$find_study(study_name),

    create_trial = function(study_id) {
      tid <- private$.mem$create_trial(study_id)
      private$.append_op(list(op = "create_trial", study_id = study_id))
      tid
    },

    set_trial_state = function(study_id, trial_id, state,
                               value = NULL, datetime_complete = NULL) {
      private$.mem$set_trial_state(study_id, trial_id, state, value, datetime_complete)
      private$.append_op(list(
        op = "set_trial_state", study_id = study_id, trial_id = trial_id,
        state = state, value = if (is.null(value)) NA else value))
    },

    set_trial_values = function(study_id, trial_id, values) {
      private$.mem$set_trial_values(study_id, trial_id, values)
      private$.append_op(list(
        op = "set_trial_values", study_id = study_id, trial_id = trial_id,
        values = as.list(values)))
    },

    set_trial_param = function(study_id, trial_id, param_name, distribution, value) {
      private$.mem$set_trial_param(study_id, trial_id, param_name, distribution, value)
      private$.append_op(list(
        op = "set_trial_param", study_id = study_id, trial_id = trial_id,
        param_name = param_name,
        distribution = dist_to_list(distribution),
        value = value))
    },

    set_trial_intermediate_value = function(study_id, trial_id, step, value) {
      private$.mem$set_trial_intermediate_value(study_id, trial_id, step, value)
      private$.append_op(list(
        op = "set_trial_intermediate_value", study_id = study_id, trial_id = trial_id,
        step = as.integer(step), value = value))
    },

    set_trial_user_attr = function(study_id, trial_id, key_name, value) {
      private$.mem$set_trial_user_attr(study_id, trial_id, key_name, value)
      private$.append_op(list(
        op = "set_trial_user_attr", study_id = study_id, trial_id = trial_id,
        key = key_name, value = value))
    },

    get_trial = function(study_id, trial_id) private$.mem$get_trial(study_id, trial_id),

    get_all_trials = function(study_id, states = NULL) {
      private$.mem$get_all_trials(study_id, states)
    }
  ),

  private = list(
    .path = NULL,
    .mem  = NULL,

    .append_op = function(op_list) {
      line <- jsonlite::toJSON(op_list, auto_unbox = TRUE, null = "null")
      cat(line, "\n", file = private$.path, append = TRUE, sep = "")
    },

    .replay = function() {
      lines <- readLines(private$.path, warn = FALSE)
      lines <- lines[nzchar(trimws(lines))]
      for (line in lines) {
        op <- tryCatch(jsonlite::fromJSON(line, simplifyVector = FALSE),
                       error = function(e) NULL)
        if (is.null(op)) next
        switch(op$op,
          create_study = {
            dirs <- if (identical(op$directions, NA) ||
                        (length(op$directions) == 1 && is.na(unlist(op$directions))))
                      NULL else as.character(unlist(op$directions))
            private$.mem$create_study(op$study_name, op$direction, directions = dirs)
          },
          create_trial = {
            private$.mem$create_trial(op$study_id)
          },
          set_trial_state = {
            val <- if (is.null(op$value) ||
                       (length(op$value) == 1 && is.na(unlist(op$value))))
                     NULL else unlist(op$value)
            private$.mem$set_trial_state(op$study_id, op$trial_id, op$state, value = val)
          },
          set_trial_values = {
            private$.mem$set_trial_values(op$study_id, op$trial_id, unlist(op$values))
          },
          set_trial_param = {
            dist <- dist_from_list(op$distribution)
            val  <- unlist(op$value)
            if (inherits(dist, "roptuna_int_distribution")) val <- as.integer(val)
            private$.mem$set_trial_param(
              op$study_id, op$trial_id, op$param_name, dist, val)
          },
          set_trial_intermediate_value = {
            private$.mem$set_trial_intermediate_value(
              op$study_id, op$trial_id, as.integer(op$step), unlist(op$value))
          },
          set_trial_user_attr = {
            private$.mem$set_trial_user_attr(
              op$study_id, op$trial_id, op$key, unlist(op$value))
          },
          set_study_user_attr = {
            private$.mem$set_study_user_attr(op$study_id, op$key, unlist(op$value))
          }
        )
      }
    }
  )
)
