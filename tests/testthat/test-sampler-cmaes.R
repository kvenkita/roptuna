test_that("cmaes_sampler optimizes a simple quadratic", {
  study <- create_study("minimize", sampler = cmaes_sampler(seed = 1L))
  study$optimize(function(trial) {
    x <- trial$suggest_float("x", -5, 5)
    y <- trial$suggest_float("y", -5, 5)
    x^2 + y^2
  }, n_trials = 80)
  expect_lte(study$best_value, 1.0)
})

test_that("cmaes_sampler produces float params within bounds", {
  study <- create_study("minimize", sampler = cmaes_sampler(seed = 2L))
  study$optimize(function(trial) {
    x <- trial$suggest_float("x", 1, 3)
    (x - 2)^2
  }, n_trials = 30)
  xs <- sapply(study$trials, function(t) t$params$x)
  expect_true(all(xs >= 1 & xs <= 3))
})

test_that("cmaes_sampler falls back to random for int params", {
  study <- create_study("minimize", sampler = cmaes_sampler(seed = 3L))
  study$optimize(function(trial) {
    k <- trial$suggest_int("k", 1L, 10L)
    x <- trial$suggest_float("x", 0, 1)
    (k - 5)^2 + x^2
  }, n_trials = 20)
  ks <- sapply(study$trials, function(t) t$params$k)
  expect_true(all(ks >= 1L & ks <= 10L))
})

test_that("cmaes_sampler is reproducible with seed", {
  make_study <- function() {
    s <- create_study("minimize", sampler = cmaes_sampler(seed = 99L))
    s$optimize(function(trial) trial$suggest_float("x", -1, 1)^2, n_trials = 10)
    sapply(s$trials, function(t) t$params$x)
  }
  xs1 <- make_study()
  xs2 <- make_study()
  expect_equal(xs1, xs2)
})
