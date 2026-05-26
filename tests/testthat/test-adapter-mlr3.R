skip_if_not_installed("mlr3")
skip_if_not_installed("mlr3tuning")
skip_if_not_installed("paradox")
skip_if_not_installed("data.table")

test_that("TunerOptuna completes tuning and selects best", {
  library(mlr3)
  library(mlr3tuning)
  library(paradox)

  task    <- mlr3::tsk("iris")
  learner <- mlr3::lrn("classif.rpart",
                       cp = paradox::to_tune(1e-4, 1e-1),
                       maxdepth = paradox::to_tune(1L, 10L))

  instance <- mlr3tuning::ti(
    task        = task,
    learner     = learner,
    resampling  = mlr3::rsmp("holdout"),
    measures    = mlr3::msr("classif.ce"),
    terminator  = mlr3tuning::trm("evals", n_evals = 6)
  )

  tuner <- TunerOptuna$new()
  tuner$optimize(instance)

  expect_true(nrow(instance$archive$data) >= 6)
  expect_false(is.na(instance$result_learner_param_vals$cp))
})
