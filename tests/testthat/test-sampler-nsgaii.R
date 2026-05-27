test_that("nsgaii_sampler falls back to random before enough mo trials", {
  study <- create_study(directions = c("minimize", "maximize"),
                        sampler = nsgaii_sampler(seed = 42L))
  study$optimize(function(trial) {
    x <- trial$suggest_float("x", 0, 1)
    c(x^2, 1 - x)
  }, n_trials = 5)
  expect_equal(study$n_trials, 5L)
})

test_that("nsgaii_sampler produces float params within bounds", {
  study <- create_study(directions = c("minimize", "maximize"),
                        sampler = nsgaii_sampler(seed = 1L))
  study$optimize(function(trial) {
    x <- trial$suggest_float("x", -2, 2)
    y <- trial$suggest_float("y", 0, 1)
    c(x^2 + y, 1 - y)
  }, n_trials = 20)
  xs <- sapply(study$trials, function(t) t$params$x)
  ys <- sapply(study$trials, function(t) t$params$y)
  expect_true(all(xs >= -2 & xs <= 2))
  expect_true(all(ys >= 0 & ys <= 1))
})

test_that("nsgaii_sampler best_trials is non-empty Pareto front", {
  study <- create_study(directions = c("minimize", "maximize"),
                        sampler = nsgaii_sampler(seed = 7L))
  study$optimize(function(trial) {
    x <- trial$suggest_float("x", 0, 1)
    c(x, 1 - x)
  }, n_trials = 15)
  bt <- study$best_trials
  expect_gte(length(bt), 1L)
})

test_that("nsgaii_sampler handles categorical params", {
  study <- create_study(directions = c("minimize", "maximize"),
                        sampler = nsgaii_sampler(seed = 3L))
  study$optimize(function(trial) {
    lr <- trial$suggest_categorical("lr", c(0.01, 0.001, 0.0001))
    c(as.numeric(lr), 1 - as.numeric(lr))
  }, n_trials = 15)
  lrs <- sapply(study$trials, function(t) t$params$lr)
  expect_true(all(lrs %in% c(0.01, 0.001, 0.0001)))
})
