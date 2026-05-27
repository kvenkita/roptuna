# ── int_distribution: step and log ───────────────────────────────────────────

test_that("int_distribution supports step parameter", {
  d <- int_distribution(0L, 100L, step = 10L)
  expect_equal(d$step, 10L)
  set.seed(1)
  vals <- replicate(50, dist_sample_random(d))
  expect_true(all(vals %% 10 == 0))
  expect_true(all(vals >= 0 & vals <= 100))
})

test_that("int_distribution supports log parameter", {
  d <- int_distribution(1L, 1000L, log = TRUE)
  expect_true(d$log)
  set.seed(1)
  vals <- replicate(30, dist_sample_random(d))
  expect_true(all(is.integer(vals)))
  expect_true(all(vals >= 1L & vals <= 1000L))
})

test_that("int_distribution serializes step and log via dist_to_list/dist_from_list", {
  d  <- int_distribution(2L, 64L, step = 2L)
  d2 <- dist_from_list(dist_to_list(d))
  expect_equal(d2$step, 2L)
  expect_equal(d2$low, 2L)
  expect_equal(d2$high, 64L)
})

test_that("suggest_int respects step parameter", {
  study <- create_study("minimize")
  study$optimize(function(trial) {
    n <- trial$suggest_int("n", 32L, 256L, step = 32L)
    as.numeric(n)
  }, n_trials = 20)
  ns <- sapply(study$trials, function(t) t$params$n)
  expect_true(all(ns %% 32 == 0))
})

test_that("suggest_int respects log parameter", {
  study <- create_study("minimize")
  study$optimize(function(trial) {
    n <- trial$suggest_int("n", 1L, 512L, log = TRUE)
    as.numeric(n)
  }, n_trials = 20)
  ns <- sapply(study$trials, function(t) t$params$n)
  expect_true(all(ns >= 1L & ns <= 512L))
})

# ── trial new methods and active bindings ─────────────────────────────────────

test_that("trial$distributions returns named distribution list", {
  study <- create_study("minimize")
  study$optimize(function(trial) {
    trial$suggest_float("lr", 1e-4, 1e-1)
    trial$suggest_int("k", 1L, 5L)
    0
  }, n_trials = 1)
  t <- study$trials[[1]]
  expect_true("lr" %in% names(t$distributions))
  expect_true("k" %in% names(t$distributions))
  expect_s3_class(t$distributions$lr, "roptuna_float_distribution")
  expect_s3_class(t$distributions$k, "roptuna_int_distribution")
})

test_that("trial$suggest_uniform is an alias for suggest_float", {
  s   <- InMemoryStorage$new()
  sid <- s$create_study("s", "minimize")
  tid <- s$create_trial(sid)
  tr  <- Trial$new(trial_id = tid, study_id = sid, storage = s, pruner = NULL)
  v   <- tr$suggest_uniform("lr", 0, 1)
  expect_gte(v, 0); expect_lte(v, 1)
})

# ── study$n_trials ────────────────────────────────────────────────────────────

test_that("study$n_trials reflects total trial count including non-complete states", {
  study <- create_study("minimize")
  study$optimize(function(trial) {
    trial$report(0.9, step = 1L)
    stop_prune()
  }, n_trials = 4)
  expect_equal(study$n_trials, 4L)
})

# ── study$set_user_attr / user_attrs ─────────────────────────────────────────

test_that("study$set_user_attr persists and user_attrs retrieves", {
  study <- create_study("minimize")
  study$set_user_attr("dataset", "iris")
  study$set_user_attr("version", 2L)
  ua <- study$user_attrs
  expect_equal(ua$dataset, "iris")
  expect_equal(ua$version, 2L)
})

test_that("study user_attrs persist across SQLite reconnection", {
  withr::with_tempfile("db", fileext = ".sqlite", code = {
    s1 <- create_study("minimize", storage = sqlite_storage(db),
                       study_name = "my_study")
    s1$set_user_attr("tag", "experiment-1")
    s2 <- create_study("minimize", storage = sqlite_storage(db),
                       study_name = "my_study", load_if_exists = TRUE)
    expect_equal(s2$user_attrs$tag, "experiment-1")
  })
})

# ── study$trials_dataframe ────────────────────────────────────────────────────

test_that("trials_dataframe returns a data.frame with expected columns", {
  study <- create_study("minimize")
  study$optimize(function(trial) {
    x <- trial$suggest_float("x", 0, 1)
    k <- trial$suggest_int("k", 1L, 3L)
    x + k
  }, n_trials = 5)
  df <- study$trials_dataframe()
  expect_true(is.data.frame(df))
  expect_true("number" %in% names(df))
  expect_true("value" %in% names(df))
  expect_true("state" %in% names(df))
  expect_true("params_x" %in% names(df))
  expect_true("params_k" %in% names(df))
  expect_equal(nrow(df), 5L)
})

test_that("trials_dataframe includes user_attrs columns", {
  study <- create_study("minimize")
  study$optimize(function(trial) {
    trial$set_user_attr("run_id", trial$number)
    trial$suggest_float("x", 0, 1)
  }, n_trials = 3)
  df <- study$trials_dataframe()
  expect_true("user_attrs_run_id" %in% names(df))
})

# ── study$enqueue_trial ───────────────────────────────────────────────────────

test_that("enqueue_trial forces specific parameter values in next trial", {
  study <- create_study("minimize")
  study$enqueue_trial(list(x = 0.123))
  study$optimize(function(trial) trial$suggest_float("x", 0, 1), n_trials = 1)
  expect_equal(study$trials[[1]]$params$x, 0.123)
})

test_that("multiple enqueued trials are consumed in order", {
  study <- create_study("minimize")
  study$enqueue_trial(list(x = 0.1))
  study$enqueue_trial(list(x = 0.9))
  study$optimize(function(trial) trial$suggest_float("x", 0, 1), n_trials = 3)
  xs <- sapply(study$trials[1:2], function(t) t$params$x)
  expect_equal(xs, c(0.1, 0.9))
})

# ── study$add_trial / add_trials ──────────────────────────────────────────────

test_that("add_trial injects a completed trial with params and value", {
  study <- create_study("minimize")
  study$add_trial(list(params = list(x = 0.5), value = 0.25))
  expect_equal(study$n_trials, 1L)
  t <- study$trials[[1]]
  expect_equal(t$params$x, 0.5)
  expect_equal(t$value, 0.25)
  expect_equal(t$state, "complete")
})

test_that("add_trials injects multiple trials at once", {
  study <- create_study("minimize")
  study$add_trials(list(
    list(params = list(x = 0.1), value = 0.01),
    list(params = list(x = 0.5), value = 0.25),
    list(params = list(x = 0.9), value = 0.81)
  ))
  expect_equal(study$n_trials, 3L)
  expect_equal(study$best_value, 0.01)
})

# ── study$stop ────────────────────────────────────────────────────────────────

test_that("study$stop() halts optimize after current trial", {
  count <- 0L
  study <- create_study("minimize")
  study$optimize(function(trial) {
    count <<- count + 1L
    if (count == 3L) study$stop()
    trial$suggest_float("x", 0, 1)
  }, n_trials = 100)
  expect_equal(study$n_trials, 3L)
})

# ── optimize callbacks ────────────────────────────────────────────────────────

test_that("callbacks are called after each trial with study and trial snapshot", {
  collected_values <- numeric(0)
  cb <- function(study, trial_snap) {
    if (!is.null(trial_snap$value))
      collected_values <<- c(collected_values, trial_snap$value)
  }
  study <- create_study("minimize")
  study$optimize(
    function(trial) trial$suggest_float("x", 0, 1)^2,
    n_trials = 5, callbacks = list(cb))
  expect_equal(length(collected_values), 5L)
  expect_true(all(collected_values >= 0))
})

# ── GridSampler: SQLite persistence ──────────────────────────────────────────

test_that("GridSampler covers all cells across SQLite sessions", {
  withr::with_tempfile("db", fileext = ".sqlite", code = {
    # First session: run 2 trials
    s1 <- create_study("minimize",
      sampler = grid_sampler(list(x = c(1.0, 2.0), y = c("a", "b"))),
      storage = sqlite_storage(db), study_name = "grid_test")
    s1$optimize(function(trial) {
      x <- trial$suggest_categorical("x", c(1.0, 2.0))
      y <- trial$suggest_categorical("y", c("a", "b"))
      as.numeric(x)
    }, n_trials = 2)

    # Second session with a fresh GridSampler (no in-memory state)
    s2 <- create_study("minimize",
      sampler = grid_sampler(list(x = c(1.0, 2.0), y = c("a", "b"))),
      storage = sqlite_storage(db), study_name = "grid_test",
      load_if_exists = TRUE)
    s2$optimize(function(trial) {
      x <- trial$suggest_categorical("x", c(1.0, 2.0))
      y <- trial$suggest_categorical("y", c("a", "b"))
      as.numeric(x)
    }, n_trials = 2)

    all_trials <- s2$trials
    combos <- sapply(all_trials, function(t) paste(t$params$x, t$params$y))
    expect_equal(length(unique(combos)), 4L)
  })
})

# ── New pruners ───────────────────────────────────────────────────────────────

test_that("threshold_pruner prunes when value exceeds upper bound", {
  study <- create_study("minimize", pruner = threshold_pruner(upper = 0.5))
  study$optimize(function(trial) {
    trial$report(1.0, step = 1L)  # above threshold
    if (trial$should_prune()) stop_prune()
    0.0
  }, n_trials = 5)
  states <- sapply(study$trials, `[[`, "state")
  expect_true(all(states == "pruned"))
})

test_that("threshold_pruner does not prune when value is within bounds", {
  study <- create_study("minimize", pruner = threshold_pruner(upper = 2.0))
  study$optimize(function(trial) {
    trial$report(0.5, step = 1L)  # within threshold
    if (trial$should_prune()) stop_prune()
    0.5
  }, n_trials = 3)
  states <- sapply(study$trials, `[[`, "state")
  expect_true(all(states == "complete"))
})

test_that("threshold_pruner: lower bound prunes below threshold", {
  study <- create_study("maximize", pruner = threshold_pruner(lower = 0.3))
  study$optimize(function(trial) {
    trial$report(0.1, step = 1L)  # below lower bound
    if (trial$should_prune()) stop_prune()
    0.1
  }, n_trials = 3)
  states <- sapply(study$trials, `[[`, "state")
  expect_true(all(states == "pruned"))
})

test_that("percentile_pruner activates after n_startup_trials", {
  study <- create_study("minimize",
    pruner = percentile_pruner(75, n_startup_trials = 3L))
  count <- 0L
  study$optimize(function(trial) {
    count <<- count + 1L
    trial$report(if (count > 3L) 100.0 else 0.5, step = 1L)
    if (trial$should_prune()) stop_prune()
    0.5
  }, n_trials = 8)
  # First 3 complete (startup), at least some later ones should be pruned
  states <- sapply(study$trials, `[[`, "state")
  expect_true(any(states == "pruned"))
})

test_that("patient_pruner wraps median pruner with patience", {
  pruner <- patient_pruner(median_pruner(n_startup_trials = 3L), patience = 2L)
  study  <- create_study("minimize", pruner = pruner)
  # All trials report 1.0 (bad), so eventually pruning kicks in
  study$optimize(function(trial) {
    for (step in 1:5) trial$report(1.0, step = step)
    if (trial$should_prune()) stop_prune()
    1.0
  }, n_trials = 10)
  expect_true(study$n_trials > 0)  # didn't crash
})

# ── New plot types ────────────────────────────────────────────────────────────

test_that("autoplot.Study type=intermediate_values works with intermediate data", {
  study <- create_study("minimize", pruner = median_pruner(n_startup_trials = 1L))
  study$optimize(function(trial) {
    for (step in 1:3) trial$report(stats::runif(1), step = step)
    stats::runif(1)
  }, n_trials = 5)
  p <- ggplot2::autoplot(study, type = "intermediate_values")
  expect_s3_class(p, "gg")
})

test_that("autoplot.Study type=edf works on completed trials", {
  study <- create_study("minimize")
  study$optimize(function(trial) trial$suggest_float("x", 0, 1)^2, n_trials = 8)
  p <- ggplot2::autoplot(study, type = "edf")
  expect_s3_class(p, "gg")
})

test_that("autoplot.Study type=slice works", {
  study <- create_study("minimize")
  study$optimize(function(trial) {
    x <- trial$suggest_float("x", -2, 2)
    y <- trial$suggest_float("y", -2, 2)
    x^2 + y^2
  }, n_trials = 10)
  p <- ggplot2::autoplot(study, type = "slice")
  expect_s3_class(p, "gg")
})

test_that("autoplot.Study type=contour works with two params", {
  study <- create_study("minimize")
  study$optimize(function(trial) {
    x <- trial$suggest_float("x", -2, 2)
    y <- trial$suggest_float("y", -2, 2)
    x^2 + y^2
  }, n_trials = 10)
  p <- ggplot2::autoplot(study, type = "contour", params = c("x", "y"))
  expect_s3_class(p, "gg")
})
