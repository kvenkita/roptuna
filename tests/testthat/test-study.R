test_that("create_study returns a Study object", {
  study <- create_study(direction = "minimize")
  expect_true(inherits(study, "Study"))
})

test_that("study$optimize runs n_trials and records results", {
  study <- create_study(direction = "minimize")
  study$optimize(function(trial) {
    x <- trial$suggest_float("x", -5, 5)
    x^2
  }, n_trials = 5)
  expect_equal(length(study$trials), 5)
  expect_true(all(sapply(study$trials, `[[`, "state") == "complete"))
})

test_that("study$best_value returns minimum for minimize direction", {
  study <- create_study(direction = "minimize")
  study$optimize(function(trial) {
    trial$suggest_categorical("v", c(1, 2, 3))
  }, n_trials = 3)
  vals <- sapply(study$trials, `[[`, "value")
  expect_equal(study$best_value, min(vals))
})

test_that("study$best_params returns params of best trial", {
  study <- create_study(direction = "minimize")
  study$optimize(function(trial) {
    x <- trial$suggest_int("x", 1L, 10L)
    as.numeric(x)
  }, n_trials = 10)
  best <- study$best_trial
  expect_equal(study$best_params$x, best$params$x)
})

test_that("optimize catches stop_prune() and marks trial as pruned", {
  study <- create_study(direction = "minimize")
  study$optimize(function(trial) {
    trial$report(0.9, step = 1L)
    stop_prune()
  }, n_trials = 3)
  states <- sapply(study$trials, `[[`, "state")
  expect_true(all(states == "pruned"))
  expect_equal(study$best_value, Inf)
})

test_that("optimize with direction=maximize picks largest value", {
  study <- create_study(direction = "maximize")
  study$optimize(function(trial) {
    trial$suggest_categorical("v", c(1.0, 2.0, 3.0))
  }, n_trials = 6)
  vals <- sapply(study$trials, `[[`, "value")
  expect_equal(study$best_value, max(vals))
})
