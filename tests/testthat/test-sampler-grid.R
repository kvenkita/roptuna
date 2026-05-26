test_that("grid_sampler covers all combinations", {
  set.seed(42)
  study <- create_study("minimize",
    sampler = grid_sampler(list(x = c(1.0, 2.0), y = c("a", "b"))))
  study$optimize(function(trial) {
    x <- trial$suggest_categorical("x", c(1.0, 2.0))
    y <- trial$suggest_categorical("y", c("a", "b"))
    as.numeric(x) + nchar(y)
  }, n_trials = 4)
  combos <- sapply(study$trials, function(t) paste(t$params$x, t$params$y))
  expect_equal(length(unique(combos)), 4)
})

test_that("grid_sampler falls back to random after exhaustion", {
  study <- create_study("minimize",
    sampler = grid_sampler(list(x = c(1.0, 2.0))))
  study$optimize(function(trial) {
    trial$suggest_categorical("x", c(1.0, 2.0))
  }, n_trials = 5)
  expect_equal(length(study$trials), 5)
})
