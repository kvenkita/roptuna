test_that("SqliteStorage round-trips a study and trials", {
  withr::with_tempfile("db", fileext = ".sqlite", code = {
    s   <- SqliteStorage$new(db)
    sid <- s$create_study("my_study", "minimize")
    expect_equal(sid, 1L)
    info <- s$get_study(sid)
    expect_equal(info$direction, "minimize")
    tid <- s$create_trial(sid)
    s$set_trial_param(sid, tid, "lr", float_distribution(0, 1), 0.01)
    s$set_trial_state(sid, tid, "complete", value = 0.5)
    trials <- s$get_all_trials(sid, states = "complete")
    expect_equal(length(trials), 1)
    expect_equal(trials[[1]]$params$lr, 0.01)
    expect_equal(trials[[1]]$value, 0.5)
  })
})

test_that("SqliteStorage persists across connections", {
  withr::with_tempfile("db", fileext = ".sqlite", code = {
    s1 <- SqliteStorage$new(db)
    sid <- s1$create_study("p", "maximize")
    tid <- s1$create_trial(sid)
    s1$set_trial_state(sid, tid, "complete", value = 1.23)
    rm(s1)
    s2 <- SqliteStorage$new(db)
    expect_equal(s2$get_all_trials(sid)[[1]]$value, 1.23)
  })
})

test_that("create_study with sqlite_storage works end-to-end", {
  withr::with_tempfile("db", fileext = ".sqlite", code = {
    study <- create_study("minimize",
                          sampler = random_sampler(seed = 7),
                          storage = sqlite_storage(db))
    study$optimize(function(trial) {
      x <- trial$suggest_float("x", -2, 2)
      x^2
    }, n_trials = 5)
    expect_equal(length(study$trials), 5)
    expect_true(study$best_value < 4.0)
  })
})
