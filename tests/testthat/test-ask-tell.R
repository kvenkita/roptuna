test_that("ask() returns a Trial object", {
  study <- create_study("minimize")
  trial <- study$ask()
  expect_true(inherits(trial, "Trial"))
})

test_that("tell() records result and sets state to complete", {
  study <- create_study("minimize")
  trial <- study$ask()
  x     <- trial$suggest_float("x", 0, 1)
  study$tell(trial, x^2)
  expect_equal(study$n_trials, 1L)
  expect_equal(study$trials[[1]]$state, "complete")
  expect_equal(study$trials[[1]]$value, x^2)
})

test_that("ask/tell loop produces correct number of trials", {
  study <- create_study("minimize", sampler = random_sampler(seed = 1L))
  for (i in 1:5) {
    tr <- study$ask()
    x  <- tr$suggest_float("x", -1, 1)
    study$tell(tr, x^2)
  }
  expect_equal(study$n_trials, 5L)
  states <- sapply(study$trials, `[[`, "state")
  expect_true(all(states == "complete"))
})

test_that("tell() with state=failed records failed state", {
  study <- create_study("minimize")
  trial <- study$ask()
  study$tell(trial, NULL, state = "failed")
  expect_equal(study$trials[[1]]$state, "failed")
})

test_that("tell() with multi-objective values stores values vector", {
  study <- create_study(directions = c("minimize", "maximize"))
  trial <- study$ask()
  x     <- trial$suggest_float("x", 0, 1)
  study$tell(trial, c(x, 1 - x))
  expect_equal(study$n_trials, 1L)
  t <- study$trials[[1]]
  expect_equal(length(t$values), 2L)
})

test_that("ask() pre-fills queued params", {
  study <- create_study("minimize")
  study$enqueue_trial(list(x = 0.777))
  trial <- study$ask()
  x     <- trial$suggest_float("x", 0, 1)
  expect_equal(x, 0.777)
  study$tell(trial, x^2)
  expect_equal(study$best_value, 0.777^2)
})
