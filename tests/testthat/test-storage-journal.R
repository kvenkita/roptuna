test_that("journal_storage creates and loads study", {
  withr::with_tempfile("jf", fileext = ".jsonl", code = {
    s1  <- JournalStorage$new(jf)
    sid <- s1$create_study("test", "minimize")
    expect_equal(sid, 1L)
    s2  <- JournalStorage$new(jf)
    expect_equal(s2$find_study("test"), 1L)
  })
})

test_that("journal_storage replays trials on reload", {
  withr::with_tempfile("jf", fileext = ".jsonl", code = {
    s1  <- JournalStorage$new(jf)
    sid <- s1$create_study("replay", "minimize")
    tid <- s1$create_trial(sid)
    s1$set_trial_param(sid, tid, "x", float_distribution(-1, 1), 0.5)
    s1$set_trial_state(sid, tid, "complete", value = 0.25)
    s2     <- JournalStorage$new(jf)
    trials <- s2$get_all_trials(sid)
    expect_equal(length(trials), 1L)
    expect_equal(trials[[1]]$params$x, 0.5)
    expect_equal(trials[[1]]$value, 0.25)
  })
})

test_that("journal_storage works with create_study via study interface", {
  withr::with_tempfile("jf", fileext = ".jsonl", code = {
    s1 <- create_study("minimize", storage = journal_storage(jf), study_name = "jtest")
    s1$optimize(function(trial) trial$suggest_float("x", 0, 1)^2, n_trials = 5)
    s2 <- create_study("minimize", storage = journal_storage(jf),
                       study_name = "jtest", load_if_exists = TRUE)
    expect_equal(s2$n_trials, 5L)
  })
})

test_that("journal_storage replays study user attrs", {
  withr::with_tempfile("jf", fileext = ".jsonl", code = {
    s1  <- JournalStorage$new(jf)
    sid <- s1$create_study("ua", "minimize")
    s1$set_study_user_attr(sid, "key", "val")
    s2  <- JournalStorage$new(jf)
    expect_equal(s2$get_study_user_attrs(sid)$key, "val")
  })
})

test_that("journal_storage supports multi-objective values", {
  withr::with_tempfile("jf", fileext = ".jsonl", code = {
    s1  <- JournalStorage$new(jf)
    sid <- s1$create_study("mo", "minimize", directions = c("minimize", "maximize"))
    tid <- s1$create_trial(sid)
    s1$set_trial_values(sid, tid, c(0.1, 0.9))
    s1$set_trial_state(sid, tid, "complete", value = 0.1)
    s2  <- JournalStorage$new(jf)
    tr  <- s2$get_trial(sid, tid)
    expect_equal(tr$values, c(0.1, 0.9))
  })
})

test_that("journal_storage replays categorical params correctly", {
  withr::with_tempfile("jf", fileext = ".jsonl", code = {
    s1  <- JournalStorage$new(jf)
    sid <- s1$create_study("cat", "minimize")
    tid <- s1$create_trial(sid)
    s1$set_trial_param(sid, tid, "c", categorical_distribution(c("a", "b", "c")), "b")
    s1$set_trial_state(sid, tid, "complete", value = 1.0)
    s2  <- JournalStorage$new(jf)
    tr  <- s2$get_trial(sid, tid)
    expect_equal(tr$params$c, "b")
  })
})
