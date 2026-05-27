# ── Storage layer tests for multi-objective ───────────────────────────────────

test_that("InMemoryStorage create_study stores directions", {
  s   <- InMemoryStorage$new()
  sid <- s$create_study("mo", "minimize", directions = c("minimize", "maximize"))
  dirs <- s$get_study_directions(sid)
  expect_equal(dirs, c("minimize", "maximize"))
})

test_that("InMemoryStorage set/get trial_values round-trips", {
  s   <- InMemoryStorage$new()
  sid <- s$create_study("mo", "minimize", directions = c("minimize", "maximize"))
  tid <- s$create_trial(sid)
  s$set_trial_values(sid, tid, c(0.1, 0.9))
  tr  <- s$get_trial(sid, tid)
  expect_equal(tr$values, c(0.1, 0.9))
})

test_that("SqliteStorage create_study stores directions", {
  withr::with_tempfile("db", fileext = ".sqlite", code = {
    s   <- SqliteStorage$new(db)
    sid <- s$create_study("mo", "minimize", directions = c("minimize", "maximize"))
    dirs <- s$get_study_directions(sid)
    expect_equal(dirs, c("minimize", "maximize"))
  })
})

test_that("SqliteStorage set/get trial_values round-trips", {
  withr::with_tempfile("db", fileext = ".sqlite", code = {
    s   <- SqliteStorage$new(db)
    sid <- s$create_study("mo", "minimize", directions = c("minimize", "maximize"))
    tid <- s$create_trial(sid)
    s$set_trial_values(sid, tid, c(0.2, 0.8))
    tr  <- s$get_trial(sid, tid)
    expect_equal(tr$values, c(0.2, 0.8))
  })
})

test_that("SqliteStorage backward compat: old DB without directions_json column", {
  withr::with_tempfile("db", fileext = ".sqlite", code = {
    con <- DBI::dbConnect(RSQLite::SQLite(), db)
    DBI::dbExecute(con, "CREATE TABLE studies (
      study_id INTEGER PRIMARY KEY AUTOINCREMENT,
      study_name TEXT UNIQUE NOT NULL, direction TEXT NOT NULL)")
    DBI::dbExecute(con, "INSERT INTO studies (study_name, direction) VALUES ('old', 'minimize')")
    DBI::dbDisconnect(con)
    s <- SqliteStorage$new(db)
    expect_null(s$get_study_directions(1L))
  })
})

# ── Study multi-objective support ─────────────────────────────────────────────

test_that("create_study with directions creates multi-objective study", {
  study <- create_study(directions = c("minimize", "maximize"))
  expect_equal(study$directions, c("minimize", "maximize"))
})

test_that("optimize with vector return stores values", {
  study <- create_study(directions = c("minimize", "maximize"))
  study$optimize(function(trial) {
    x <- trial$suggest_float("x", 0, 1)
    c(x, 1 - x)
  }, n_trials = 5)
  expect_equal(study$n_trials, 5L)
  t <- study$trials[[1]]
  expect_equal(length(t$values), 2L)
  expect_equal(t$values[[1]] + t$values[[2]], 1, tolerance = 1e-10)
})

test_that("best_trials returns Pareto non-dominated set", {
  study <- create_study(directions = c("minimize", "minimize"))
  study$add_trials(list(
    list(params = list(x = 1), values = c(1.0, 3.0)),
    list(params = list(x = 2), values = c(2.0, 2.0)),
    list(params = list(x = 3), values = c(3.0, 1.0)),
    list(params = list(x = 4), values = c(1.5, 1.5))
  ))
  bt <- study$best_trials
  bt_vals <- lapply(bt, function(t) t$values)
  val_matrix <- do.call(rbind, bt_vals)
  # (2.0, 2.0) is dominated by (1.5, 1.5): both objectives worse
  expect_false(any(val_matrix[, 1] == 2.0 & val_matrix[, 2] == 2.0))
  expect_gte(length(bt), 1L)
})

test_that("add_trial with values stores multi-objective result", {
  study <- create_study(directions = c("minimize", "maximize"))
  study$add_trial(list(params = list(x = 0.3), values = c(0.3, 0.7)))
  t <- study$trials[[1]]
  expect_equal(t$values, c(0.3, 0.7))
})

test_that("autoplot pareto_front produces gg object", {
  study <- create_study(directions = c("minimize", "maximize"))
  study$optimize(function(trial) {
    x <- trial$suggest_float("x", 0, 1)
    c(x^2, 1 - x)
  }, n_trials = 8)
  p <- ggplot2::autoplot(study, type = "pareto_front")
  expect_s3_class(p, "gg")
})
