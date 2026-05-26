#' mlr3tuning adapter for roptuna
#'
#' A class that uses roptuna to search hyperparameters inside an mlr3tuning
#' instance. Use with `mlr3tuning::ti()` and `tuner$optimize(instance)`.
#'
#' @examples
#' \dontrun{
#' # Requires mlr3, mlr3tuning, paradox packages
#' library(mlr3); library(mlr3tuning); library(paradox)
#' tuner <- TunerOptuna$new(sampler = tpe_sampler(seed = 1L))
#' }
#' @export
TunerOptuna <- R6::R6Class("TunerOptuna",
  public = list(
    #' @param sampler roptuna sampler (default: tpe_sampler()).
    #' @param pruner roptuna pruner or NULL.
    initialize = function(sampler = tpe_sampler(), pruner = NULL) {
      if (!requireNamespace("mlr3tuning", quietly = TRUE))
        stop("Package 'mlr3tuning' required for TunerOptuna.")
      if (!requireNamespace("paradox",    quietly = TRUE))
        stop("Package 'paradox' required for TunerOptuna.")
      private$.sampler <- sampler
      private$.pruner  <- pruner
    },

    #' @description Run optimization on an mlr3tuning TuningInstance.
    optimize = function(inst) {
      if (!requireNamespace("mlr3tuning", quietly = TRUE))
        stop("Package 'mlr3tuning' required.")
      if (!requireNamespace("data.table", quietly = TRUE))
        stop("Package 'data.table' required.")

      search_space <- inst$search_space
      param_ids    <- search_space$ids()

      direction <- if (identical(inst$objective$codomain$tags[[1]], "maximize"))
        "maximize" else "minimize"

      study <- create_study(
        direction = direction,
        sampler   = private$.sampler,
        pruner    = private$.pruner
      )

      n_evals <- inst$terminator$param_set$values$n_evals %||% 20L

      objective_fn <- function(trial) {
        params <- list()
        for (pid in param_ids) {
          p <- search_space$params[[pid]]
          if (inherits(p, "ParamDbl")) {
            params[[pid]] <- trial$suggest_float(pid, p$lower, p$upper)
          } else if (inherits(p, "ParamInt")) {
            params[[pid]] <- trial$suggest_int(pid, as.integer(p$lower),
                                               as.integer(p$upper))
          } else if (inherits(p, "ParamFct")) {
            params[[pid]] <- trial$suggest_categorical(pid, p$levels)
          } else if (inherits(p, "ParamLgl")) {
            params[[pid]] <- as.logical(
              trial$suggest_categorical(pid, c("TRUE", "FALSE")))
          }
        }

        xdt <- data.table::as.data.table(params)
        inst$eval_batch(xdt)

        arch <- inst$archive$data
        arch[[inst$objective$codomain$target_ids]][[nrow(arch)]]
      }

      study$optimize(objective_fn, n_trials = n_evals)
      invisible(inst)
    }
  ),
  private = list(.sampler = NULL, .pruner = NULL)
)
