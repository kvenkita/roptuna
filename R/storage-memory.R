#' In-memory storage backend for roptuna studies
#' @examples
#' storage <- InMemoryStorage$new()
#' @export
InMemoryStorage <- R6::R6Class("InMemoryStorage",
  public = list(
    initialize = function() {
      private$.studies          <- list()
      private$.trials           <- list()
      private$.study_counter    <- 0L
      private$.trial_counters   <- list()
      private$.study_user_attrs <- list()
    },

    create_study = function(study_name, direction, directions = NULL) {
      private$.study_counter <- private$.study_counter + 1L
      sid <- private$.study_counter
      private$.studies[[as.character(sid)]] <- list(
        study_id   = sid,
        study_name = study_name,
        direction  = direction,
        directions = directions
      )
      private$.trial_counters[[as.character(sid)]]   <- 0L
      private$.study_user_attrs[[as.character(sid)]] <- list()
      sid
    },

    get_study = function(study_id) {
      s <- private$.studies[[as.character(study_id)]]
      s$user_attrs <- private$.study_user_attrs[[as.character(study_id)]] %||% list()
      s
    },

    get_study_directions = function(study_id) {
      private$.studies[[as.character(study_id)]]$directions
    },

    set_study_user_attr = function(study_id, key, value) {
      private$.study_user_attrs[[as.character(study_id)]][[key]] <- value
    },

    get_study_user_attrs = function(study_id) {
      private$.study_user_attrs[[as.character(study_id)]] %||% list()
    },

    create_trial = function(study_id) {
      sid_key <- as.character(study_id)
      cnt <- private$.trial_counters[[sid_key]]
      tid <- cnt + 1L
      private$.trial_counters[[sid_key]] <- tid
      key <- paste(study_id, tid, sep = "_")
      private$.trials[[key]] <- list(
        trial_id            = tid,
        number              = tid - 1L,
        study_id            = study_id,
        state               = "running",
        value               = NULL,
        values              = NULL,
        params              = list(),
        distributions       = list(),
        intermediate_values = stats::setNames(numeric(0), character(0)),
        user_attrs          = list(),
        datetime_start      = Sys.time(),
        datetime_complete   = NULL
      )
      tid
    },

    set_trial_state = function(study_id, trial_id, state,
                               value = NULL, datetime_complete = NULL) {
      key <- paste(study_id, trial_id, sep = "_")
      private$.trials[[key]]$state <- state
      if (!is.null(value)) private$.trials[[key]]$value <- value
      if (state %in% c("complete", "pruned", "failed")) {
        private$.trials[[key]]$datetime_complete <-
          datetime_complete %||% Sys.time()
      }
    },

    set_trial_values = function(study_id, trial_id, values) {
      key <- paste(study_id, trial_id, sep = "_")
      private$.trials[[key]]$values <- as.numeric(values)
    },

    set_trial_param = function(study_id, trial_id, param_name, distribution, value) {
      key <- paste(study_id, trial_id, sep = "_")
      private$.trials[[key]]$params[[param_name]]        <- value
      private$.trials[[key]]$distributions[[param_name]] <- distribution
    },

    set_trial_intermediate_value = function(study_id, trial_id, step, value) {
      key <- paste(study_id, trial_id, sep = "_")
      private$.trials[[key]]$intermediate_values[[as.character(step)]] <- value
    },

    set_trial_user_attr = function(study_id, trial_id, key_name, value) {
      key <- paste(study_id, trial_id, sep = "_")
      private$.trials[[key]]$user_attrs[[key_name]] <- value
    },

    get_trial = function(study_id, trial_id) {
      private$.trials[[paste(study_id, trial_id, sep = "_")]]
    },

    get_all_trials = function(study_id, states = NULL) {
      prefix <- paste0("^", study_id, "_")
      keys   <- grep(prefix, names(private$.trials), value = TRUE)
      trials <- private$.trials[keys]
      if (!is.null(states)) {
        trials <- Filter(function(t) t$state %in% states, trials)
      }
      unname(trials)
    },

    find_study = function(study_name) {
      for (sid_key in names(private$.studies)) {
        if (private$.studies[[sid_key]]$study_name == study_name)
          return(private$.studies[[sid_key]]$study_id)
      }
      NULL
    }
  ),
  private = list(
    .studies          = NULL,
    .trials           = NULL,
    .study_counter    = NULL,
    .trial_counters   = NULL,
    .study_user_attrs = NULL
  )
)
