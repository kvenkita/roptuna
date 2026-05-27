test_that("optimize with n_jobs=1 behaves like sequential", {
  study1 <- create_study("minimize", sampler = random_sampler(seed = 1L))
  study1$optimize(function(trial) trial$suggest_float("x", 0, 1)^2, n_trials = 10)

  study2 <- create_study("minimize", sampler = random_sampler(seed = 1L))
  study2$optimize(function(trial) trial$suggest_float("x", 0, 1)^2, n_trials = 10,
                  n_jobs = 1L)

  expect_equal(study1$n_trials, study2$n_trials)
})

test_that("optimize with n_jobs=2 completes n_trials trials", {
  study <- create_study("minimize")
  study$optimize(function(trial) {
    x <- trial$suggest_float("x", 0, 1)
    x^2
  }, n_trials = 6, n_jobs = 2L)
  expect_equal(study$n_trials, 6L)
})

test_that("optimize n_jobs=2 produces params within bounds", {
  study <- create_study("minimize", sampler = random_sampler(seed = 5L))
  study$optimize(function(trial) {
    x <- trial$suggest_float("x", -2, 2)
    k <- trial$suggest_categorical("k", c("a", "b", "c"))
    x^2
  }, n_trials = 8, n_jobs = 2L)
  xs <- sapply(study$trials, function(t) t$params$x)
  ks <- sapply(study$trials, function(t) t$params$k)
  expect_true(all(xs >= -2 & xs <= 2))
  expect_true(all(ks %in% c("a", "b", "c")))
})

test_that("optimize n_jobs=2 works with SQLite storage", {
  withr::with_tempfile("db", fileext = ".sqlite", code = {
    study <- create_study("minimize",
      storage    = sqlite_storage(db),
      study_name = "par_test")
    study$optimize(function(trial) {
      trial$suggest_float("x", 0, 1)^2
    }, n_trials = 6, n_jobs = 2L)
    expect_equal(study$n_trials, 6L)
  })
})
