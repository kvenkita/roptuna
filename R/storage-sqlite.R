#' SQLite storage backend (schema matches Python Optuna)
#' @param path Path to SQLite file.
#' @export
sqlite_storage <- function(path) SqliteStorage$new(path)

#' @export
SqliteStorage <- R6::R6Class("SqliteStorage",
  public = list(
    initialize = function(path) {
      private$.path <- path
      con <- private$.con(); on.exit(DBI::dbDisconnect(con))
      private$.init_schema(con)
    },
    create_study = function(study_name, direction) {
      con <- private$.con(); on.exit(DBI::dbDisconnect(con))
      DBI::dbExecute(con,
        "INSERT INTO studies (study_name, direction) VALUES (?, ?)",
        list(study_name, direction))
      as.integer(DBI::dbGetQuery(con, "SELECT last_insert_rowid()")[[1]])
    },
    get_study = function(study_id) {
      con <- private$.con(); on.exit(DBI::dbDisconnect(con))
      as.list(DBI::dbGetQuery(con,
        "SELECT * FROM studies WHERE study_id = ?", list(study_id)))
    },
    create_trial = function(study_id) {
      con <- private$.con(); on.exit(DBI::dbDisconnect(con))
      n <- DBI::dbGetQuery(con,
        "SELECT COUNT(*) FROM trials WHERE study_id = ?", list(study_id))[[1]]
      DBI::dbExecute(con,
        "INSERT INTO trials (number, study_id, state, datetime_start) VALUES (?, ?, 'running', ?)",
        list(n, study_id, format(Sys.time())))
      as.integer(DBI::dbGetQuery(con, "SELECT last_insert_rowid()")[[1]])
    },
    set_trial_state = function(study_id, trial_id, state,
                               value = NULL, datetime_complete = NULL) {
      con <- private$.con(); on.exit(DBI::dbDisconnect(con))
      if (state %in% c("complete", "pruned", "failed")) {
        DBI::dbExecute(con,
          "UPDATE trials SET state=?, value=?, datetime_complete=? WHERE trial_id=?",
          list(state, value, format(datetime_complete %||% Sys.time()), trial_id))
      } else {
        DBI::dbExecute(con,
          "UPDATE trials SET state=? WHERE trial_id=?",
          list(state, trial_id))
      }
    },
    set_trial_param = function(study_id, trial_id, param_name, distribution, value) {
      con <- private$.con(); on.exit(DBI::dbDisconnect(con))
      dj <- jsonlite::toJSON(dist_to_list(distribution), auto_unbox = TRUE)
      DBI::dbExecute(con,
        "INSERT OR REPLACE INTO trial_params
         (trial_id, param_name, internal_value, distribution_json) VALUES (?,?,?,?)",
        list(trial_id, param_name, value, dj))
    },
    set_trial_intermediate_value = function(study_id, trial_id, step, value) {
      con <- private$.con(); on.exit(DBI::dbDisconnect(con))
      DBI::dbExecute(con,
        "INSERT OR REPLACE INTO trial_intermediate_values
         (trial_id, step, intermediate_value) VALUES (?,?,?)",
        list(trial_id, as.integer(step), value))
    },
    set_trial_user_attr = function(study_id, trial_id, key_name, value) {
      con <- private$.con(); on.exit(DBI::dbDisconnect(con))
      DBI::dbExecute(con,
        "INSERT OR REPLACE INTO trial_user_attributes (trial_id, key, value_json) VALUES (?,?,?)",
        list(trial_id, key_name, jsonlite::toJSON(value, auto_unbox = TRUE)))
    },
    get_trial = function(study_id, trial_id) {
      con <- private$.con(); on.exit(DBI::dbDisconnect(con))
      private$.load_trial(con, study_id, trial_id)
    },
    get_all_trials = function(study_id, states = NULL) {
      con <- private$.con(); on.exit(DBI::dbDisconnect(con))
      if (is.null(states)) {
        rows <- DBI::dbGetQuery(con,
          "SELECT trial_id FROM trials WHERE study_id = ?", list(study_id))
      } else {
        ph <- paste(rep("?", length(states)), collapse = ",")
        rows <- DBI::dbGetQuery(con,
          paste0("SELECT trial_id FROM trials WHERE study_id=? AND state IN (", ph, ")"),
          c(list(study_id), as.list(states)))
      }
      lapply(rows$trial_id, function(tid) private$.load_trial(con, study_id, tid))
    }
  ),
  private = list(
    .path = NULL,
    .con  = function() DBI::dbConnect(RSQLite::SQLite(), private$.path),
    .init_schema = function(con) {
      DBI::dbExecute(con, "PRAGMA journal_mode=WAL")
      DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS studies (
        study_id INTEGER PRIMARY KEY AUTOINCREMENT,
        study_name TEXT UNIQUE NOT NULL, direction TEXT NOT NULL)")
      DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS trials (
        trial_id INTEGER PRIMARY KEY AUTOINCREMENT,
        number INTEGER NOT NULL, study_id INTEGER NOT NULL,
        state TEXT NOT NULL, value REAL,
        datetime_start TEXT, datetime_complete TEXT)")
      DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS trial_params (
        param_id INTEGER PRIMARY KEY AUTOINCREMENT,
        trial_id INTEGER NOT NULL, param_name TEXT NOT NULL,
        internal_value REAL NOT NULL, distribution_json TEXT NOT NULL,
        UNIQUE(trial_id, param_name))")
      DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS trial_intermediate_values (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        trial_id INTEGER NOT NULL, step INTEGER NOT NULL,
        intermediate_value REAL NOT NULL, UNIQUE(trial_id, step))")
      DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS trial_user_attributes (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        trial_id INTEGER NOT NULL, key TEXT NOT NULL,
        value_json TEXT NOT NULL, UNIQUE(trial_id, key))")
    },
    .load_trial = function(con, study_id, trial_id) {
      tr <- DBI::dbGetQuery(con,
        "SELECT * FROM trials WHERE trial_id=?", list(trial_id))
      pr <- DBI::dbGetQuery(con,
        "SELECT * FROM trial_params WHERE trial_id=?", list(trial_id))
      iv <- DBI::dbGetQuery(con,
        "SELECT * FROM trial_intermediate_values WHERE trial_id=?", list(trial_id))
      ua <- DBI::dbGetQuery(con,
        "SELECT * FROM trial_user_attributes WHERE trial_id=?", list(trial_id))
      list(
        trial_id  = trial_id, number = tr$number, study_id = study_id,
        state = tr$state, value = tr$value,
        params = if (nrow(pr) > 0) stats::setNames(as.list(pr$internal_value), pr$param_name) else list(),
        distributions = if (nrow(pr) > 0)
          stats::setNames(lapply(pr$distribution_json, function(j)
            dist_from_list(jsonlite::fromJSON(j))), pr$param_name) else list(),
        intermediate_values = if (nrow(iv) > 0)
          stats::setNames(iv$intermediate_value, as.character(iv$step)) else
          stats::setNames(numeric(0), character(0)),
        user_attrs = if (nrow(ua) > 0)
          stats::setNames(lapply(ua$value_json, jsonlite::fromJSON), ua$key) else list(),
        datetime_start = tr$datetime_start,
        datetime_complete = tr$datetime_complete
      )
    }
  )
)
