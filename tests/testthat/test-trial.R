test_that("trial$suggest_float records param and returns value in bounds", {
  s   <- InMemoryStorage$new()
  sid <- s$create_study("s", "minimize")
  tid <- s$create_trial(sid)
  trial <- Trial$new(trial_id = tid, study_id = sid, storage = s, pruner = NULL)
  v <- trial$suggest_float("lr", 1e-4, 1e-1)
  expect_gte(v, 1e-4)
  expect_lte(v, 1e-1)
  snap <- s$get_trial(sid, tid)
  expect_equal(snap$params$lr, v)
})

test_that("trial$suggest_float is idempotent on second call", {
  s   <- InMemoryStorage$new()
  sid <- s$create_study("s", "minimize")
  tid <- s$create_trial(sid)
  trial <- Trial$new(trial_id = tid, study_id = sid, storage = s, pruner = NULL)
  v1 <- trial$suggest_float("lr", 0, 1)
  v2 <- trial$suggest_float("lr", 0, 1)
  expect_equal(v1, v2)
})

test_that("trial$suggest_int returns integer in bounds", {
  s   <- InMemoryStorage$new()
  sid <- s$create_study("s", "minimize")
  tid <- s$create_trial(sid)
  trial <- Trial$new(trial_id = tid, study_id = sid, storage = s, pruner = NULL)
  v <- trial$suggest_int("n_layers", 1L, 5L)
  expect_true(v %in% 1:5)
  expect_true(v == as.integer(v))
})

test_that("trial$suggest_categorical returns one of the choices", {
  s   <- InMemoryStorage$new()
  sid <- s$create_study("s", "minimize")
  tid <- s$create_trial(sid)
  trial <- Trial$new(trial_id = tid, study_id = sid, storage = s, pruner = NULL)
  v <- trial$suggest_categorical("optimizer", c("adam", "sgd", "rmsprop"))
  expect_true(v %in% c("adam", "sgd", "rmsprop"))
})

test_that("trial$suggest_log_uniform returns value in log-space bounds", {
  s   <- InMemoryStorage$new()
  sid <- s$create_study("s", "minimize")
  tid <- s$create_trial(sid)
  trial <- Trial$new(trial_id = tid, study_id = sid, storage = s, pruner = NULL)
  v <- trial$suggest_log_uniform("lr", 1e-5, 1e-1)
  expect_gte(v, 1e-5)
  expect_lte(v, 1e-1)
})

test_that("trial$report stores intermediate value", {
  s   <- InMemoryStorage$new()
  sid <- s$create_study("s", "minimize")
  tid <- s$create_trial(sid)
  trial <- Trial$new(trial_id = tid, study_id = sid, storage = s, pruner = NULL)
  trial$report(0.5, step = 1L)
  snap <- s$get_trial(sid, tid)
  expect_equal(snap$intermediate_values[["1"]], 0.5)
})

test_that("stop_prune() raises roptuna_trial_pruned condition", {
  expect_condition(stop_prune(), class = "roptuna_trial_pruned")
})

test_that("trial$params active binding returns named list of suggestions", {
  s   <- InMemoryStorage$new()
  sid <- s$create_study("s", "minimize")
  tid <- s$create_trial(sid)
  trial <- Trial$new(trial_id = tid, study_id = sid, storage = s, pruner = NULL)
  trial$suggest_float("x", 0, 1)
  trial$suggest_int("n", 1L, 5L)
  p <- trial$params
  expect_true("x" %in% names(p))
  expect_true("n" %in% names(p))
})
