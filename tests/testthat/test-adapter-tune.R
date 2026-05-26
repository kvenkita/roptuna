skip_if_not_installed("tune")
skip_if_not_installed("parsnip")
skip_if_not_installed("rsample")
skip_if_not_installed("workflows")
skip_if_not_installed("yardstick")
skip_if_not_installed("dials")

test_that("tune_optuna runs n_trials and returns a study", {
  set.seed(1)
  data <- data.frame(
    x1 = rnorm(100), x2 = rnorm(100),
    y  = factor(sample(c("a", "b"), 100, replace = TRUE))
  )
  folds <- rsample::vfold_cv(data, v = 3)

  spec <- parsnip::decision_tree(
    cost_complexity = tune::tune(),
    tree_depth      = tune::tune()
  ) |> parsnip::set_engine("rpart") |> parsnip::set_mode("classification")

  wf <- workflows::workflow() |>
    workflows::add_model(spec) |>
    workflows::add_formula(y ~ x1 + x2)

  suggest_fn <- function(trial) {
    list(
      cost_complexity = trial$suggest_float("cost_complexity", 1e-4, 1e-1, log = TRUE),
      tree_depth      = trial$suggest_int("tree_depth", 1L, 10L)
    )
  }

  study <- tune_optuna(wf, folds, suggest_fn = suggest_fn,
                       n_trials = 5, direction = "minimize",
                       metrics = yardstick::metric_set(yardstick::mn_log_loss))
  expect_true(inherits(study, "Study"))
  expect_equal(length(study$trials), 5)
  expect_true(all(sapply(study$trials, `[[`, "state") %in% c("complete", "failed")))
})
