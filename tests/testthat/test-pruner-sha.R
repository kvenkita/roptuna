test_that("successive_halving_pruner skips before min_resource", {
  storage <- InMemoryStorage$new()
  study <- create_study("minimize", storage = storage)
  sid <- study$study_id
  tid <- storage$create_trial(sid)
  storage$set_trial_intermediate_value(sid, tid, 1L, 0.9)
  snap <- storage$get_trial(sid, tid)
  expect_false(successive_halving_pruner(min_resource = 5L)$prune(study, snap))
})

test_that("successive_halving_pruner does not prune when no reference values", {
  storage <- InMemoryStorage$new()
  study <- create_study("minimize", storage = storage)
  sid <- study$study_id
  tid <- storage$create_trial(sid)
  storage$set_trial_intermediate_value(sid, tid, 5L, 0.9)
  snap <- storage$get_trial(sid, tid)
  expect_false(successive_halving_pruner(min_resource = 1L)$prune(study, snap))
})
