make_study_30 <- function() {
  set.seed(7)
  study <- create_study("minimize", sampler = tpe_sampler(seed = 7))
  study$optimize(function(trial) {
    x <- trial$suggest_float("x", -5, 5)
    y <- trial$suggest_int("y", 1L, 5L)
    x^2 + y
  }, n_trials = 30)
  study
}

test_that("autoplot history returns a ggplot", {
  study <- make_study_30()
  p <- autoplot(study, type = "history")
  expect_s3_class(p, "ggplot")
})

test_that("autoplot parallel_coordinate returns a ggplot", {
  study <- make_study_30()
  p <- autoplot(study, type = "parallel_coordinate")
  expect_s3_class(p, "ggplot")
})

test_that("autoplot param_importance returns a ggplot", {
  study <- make_study_30()
  p <- autoplot(study, type = "param_importance")
  expect_s3_class(p, "ggplot")
})

test_that("autoplot errors on unknown type", {
  study <- make_study_30()
  expect_error(autoplot(study, type = "banana"), "should be one of")
})
