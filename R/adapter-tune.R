#' Run a tidymodels workflow using roptuna for hyperparameter search
#'
#' @param object A `workflow` object with `tune::tune()` placeholders.
#' @param resamples An `rset` object (e.g. from `rsample::vfold_cv()`).
#' @param suggest_fn A function `function(trial) -> named list` mapping trial
#'   suggestions to parameter names. Parameter names must match those in the
#'   workflow's tuning parameters.
#' @param n_trials Number of trials to run.
#' @param direction "minimize" or "maximize".
#' @param sampler An roptuna sampler. Defaults to `tpe_sampler()`.
#' @param pruner An roptuna pruner or NULL.
#' @param storage An roptuna storage backend or NULL (in-memory).
#' @param study_name Character study name.
#' @param metrics A `yardstick::metric_set`. Defaults to workflow's default metric.
#' @param control A `tune::control_resamples()` object.
#' @return An roptuna `Study` object. Use `study$best_params` to retrieve
#'   the best hyperparameters.
#' @examples
#' \dontrun{
#' # Requires tune, parsnip, rsample, workflows, yardstick packages
#' library(tune); library(parsnip); library(rsample); library(workflows)
#' wf <- workflow() |>
#'   add_model(decision_tree(cost_complexity = tune()) |> set_engine("rpart") |>
#'               set_mode("classification")) |>
#'   add_formula(Species ~ .)
#' folds <- vfold_cv(iris, v = 3)
#' study <- tune_optuna(wf, folds,
#'   suggest_fn = function(trial) list(cost_complexity = trial$suggest_float("cp", 0, 0.1)),
#'   n_trials = 5, direction = "minimize")
#' study$best_params
#' }
#' @export
tune_optuna <- function(object, resamples, suggest_fn,
                        n_trials = 20,
                        direction = "minimize",
                        sampler = tpe_sampler(),
                        pruner = NULL,
                        storage = NULL,
                        study_name = NULL,
                        metrics = NULL,
                        control = NULL) {
  if (!requireNamespace("tune",      quietly = TRUE)) stop("Package 'tune' required.")
  if (!requireNamespace("workflows", quietly = TRUE)) stop("Package 'workflows' required.")

  if (is.null(control))
    control <- tune::control_resamples(save_pred = FALSE, verbose = FALSE)

  study <- create_study(
    direction  = direction,
    sampler    = sampler,
    pruner     = pruner,
    storage    = storage,
    study_name = study_name
  )

  objective <- function(trial) {
    params <- suggest_fn(trial)
    params_tbl <- tibble::as_tibble_row(params)

    updated_wf <- tryCatch(
      tune::finalize_workflow(object, params_tbl),
      error = function(e) stop("tune::finalize_workflow failed: ", conditionMessage(e))
    )

    result <- tune::fit_resamples(
      updated_wf,
      resamples = resamples,
      metrics   = metrics,
      control   = control
    )

    metric_summary <- tune::collect_metrics(result)
    metric_summary$mean[[1]]
  }

  study$optimize(objective, n_trials = n_trials, catch = "error")
  study
}
