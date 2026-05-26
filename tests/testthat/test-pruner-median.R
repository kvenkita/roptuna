make_study_with_iv <- function(values, step = 1L) {
  storage <- InMemoryStorage$new()
  study <- create_study("minimize", storage = storage)
  sid <- study$study_id
  for (v in values) {
    tid <- storage$create_trial(sid)
    storage$set_trial_intermediate_value(sid, tid, step, v)
    storage$set_trial_state(sid, tid, "complete", value = v)
  }
  list(storage = storage, sid = sid, study = study)
}

test_that("median_pruner skips before n_startup_trials", {
  ctx <- make_study_with_iv(c(0.1, 0.2, 0.3))
  tid <- ctx$storage$create_trial(ctx$sid)
  ctx$storage$set_trial_intermediate_value(ctx$sid, tid, 1L, 0.9)
  snap <- ctx$storage$get_trial(ctx$sid, tid)
  expect_false(median_pruner(n_startup_trials = 5)$prune(ctx$study, snap))
})

test_that("median_pruner prunes when worse than median", {
  ctx <- make_study_with_iv(c(0.1, 0.2, 0.3, 0.4, 0.5))
  tid <- ctx$storage$create_trial(ctx$sid)
  ctx$storage$set_trial_intermediate_value(ctx$sid, tid, 1L, 0.9)
  snap <- ctx$storage$get_trial(ctx$sid, tid)
  expect_true(median_pruner(n_startup_trials = 3)$prune(ctx$study, snap))
})

test_that("median_pruner keeps when better than median", {
  ctx <- make_study_with_iv(c(0.1, 0.2, 0.3, 0.4, 0.5))
  tid <- ctx$storage$create_trial(ctx$sid)
  ctx$storage$set_trial_intermediate_value(ctx$sid, tid, 1L, 0.05)
  snap <- ctx$storage$get_trial(ctx$sid, tid)
  expect_false(median_pruner(n_startup_trials = 3)$prune(ctx$study, snap))
})
