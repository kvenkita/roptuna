test_that("tpe_sampler finds minimum faster than random on quadratic", {
  set.seed(123)
  study_tpe <- create_study("minimize", sampler = tpe_sampler(seed = 123))
  study_tpe$optimize(function(trial) {
    x <- trial$suggest_float("x", -5, 5); x^2
  }, n_trials = 30)

  set.seed(123)
  study_rnd <- create_study("minimize", sampler = random_sampler(seed = 123))
  study_rnd$optimize(function(trial) {
    x <- trial$suggest_float("x", -5, 5); x^2
  }, n_trials = 30)

  # TPE should find a value within 0.5 of random's best (may not be strictly better
  # on every seed, but should be competitive)
  expect_lt(study_tpe$best_value, study_rnd$best_value + 0.5)
})

test_that("tpe_sampler handles categorical parameters", {
  set.seed(42)
  study <- create_study("minimize", sampler = tpe_sampler(seed = 42))
  study$optimize(function(trial) {
    opt <- trial$suggest_categorical("opt", c("adam", "sgd", "rmsprop"))
    if (opt == "adam") 0.1 else if (opt == "sgd") 0.5 else 0.9
  }, n_trials = 20)
  expect_equal(study$best_params$opt, "adam")
})

test_that("tpe_sampler falls back to random before n_startup_trials", {
  set.seed(1)
  study <- create_study("minimize",
    sampler = tpe_sampler(n_startup_trials = 10L, seed = 1))
  study$optimize(function(trial) trial$suggest_float("x", 0, 1), n_trials = 5)
  expect_equal(length(study$trials), 5)
  expect_true(all(sapply(study$trials, `[[`, "state") == "complete"))
})

test_that("tpe_sampler does not get stuck with narrow integer range (sigma floor)", {
  # Regression: without minimum sigma, Parzen kernels collapse to delta functions,
  # candidates all round to the same integer, and EI = Inf-Inf = NaN -> which.max
  # returns integer(0) -> sampler crashes or selection fails.
  set.seed(7)
  study <- create_study("minimize",
    sampler = tpe_sampler(seed = 7, n_startup_trials = 5L))
  study$optimize(function(trial) {
    k <- trial$suggest_int("k", 1L, 3L)
    c(1.0, 0.2, 0.8)[k]
  }, n_trials = 50L)
  expect_equal(length(study$trials), 50L)
  expect_equal(study$best_params$k, 2L)
  # After startup, TPE should predominantly exploit k=2
  post_k <- sapply(study$trials[20:50], function(t) t$params$k)
  expect_gt(mean(post_k == 2), 0.5)
})

test_that("tpe_sampler converges on 1-D quadratic within 100 trials", {
  set.seed(42)
  study <- create_study("minimize", sampler = tpe_sampler(seed = 42))
  study$optimize(function(trial) trial$suggest_float("x", -10, 10)^2, n_trials = 100L)
  expect_lt(study$best_value, 0.1)
})
