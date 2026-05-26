test_that("random_sampler samples float in bounds", {
  s <- random_sampler()
  storage <- InMemoryStorage$new()
  sid <- storage$create_study("s", "minimize")
  study <- create_study("minimize", sampler = s, storage = storage)
  tid <- storage$create_trial(sid)
  trial <- Trial$new(tid, sid, storage, pruner = NULL, sampler = s, study = study)
  v <- s$sample_independent(study, trial, "x", float_distribution(0, 1))
  expect_gte(v, 0); expect_lte(v, 1)
})

test_that("random_sampler samples int in bounds", {
  s <- random_sampler()
  storage <- InMemoryStorage$new()
  sid <- storage$create_study("s", "minimize")
  study <- create_study("minimize", sampler = s, storage = storage)
  tid <- storage$create_trial(sid)
  trial <- Trial$new(tid, sid, storage, pruner = NULL, sampler = s, study = study)
  v <- s$sample_independent(study, trial, "n", int_distribution(1L, 10L))
  expect_true(v %in% 1:10)
})

test_that("random_sampler samples categorical from choices", {
  s <- random_sampler()
  storage <- InMemoryStorage$new()
  sid <- storage$create_study("s", "minimize")
  study <- create_study("minimize", sampler = s, storage = storage)
  tid <- storage$create_trial(sid)
  trial <- Trial$new(tid, sid, storage, pruner = NULL, sampler = s, study = study)
  v <- s$sample_independent(study, trial, "opt",
                            categorical_distribution(c("a", "b")))
  expect_true(v %in% c("a", "b"))
})
