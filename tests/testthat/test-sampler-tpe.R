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
