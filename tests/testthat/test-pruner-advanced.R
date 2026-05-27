# ── HyperbandPruner ───────────────────────────────────────────────────────────

test_that("hyperband_pruner constructor works and prune returns logical", {
  pruner <- hyperband_pruner(min_resource = 1L, reduction_factor = 3L, n_brackets = 3L)
  study  <- create_study("minimize", pruner = pruner)
  study$optimize(function(trial) {
    for (step in 1:5) trial$report(stats::runif(1), step = step)
    if (trial$should_prune()) stop_prune()
    stats::runif(1)
  }, n_trials = 10)
  expect_true(study$n_trials > 0)
})

test_that("hyperband_pruner prunes high-loss trials eventually", {
  pruner <- hyperband_pruner(min_resource = 1L, reduction_factor = 2L, n_brackets = 2L)
  study  <- create_study("minimize", pruner = pruner)
  count  <- 0L
  study$optimize(function(trial) {
    count <<- count + 1L
    val <- if (count > 4L) 1000.0 else 0.5
    trial$report(val, step = 1L)
    if (trial$should_prune()) stop_prune()
    val
  }, n_trials = 10)
  states <- sapply(study$trials, `[[`, "state")
  expect_true(any(states == "pruned"))
})

# ── WilcoxonPruner ────────────────────────────────────────────────────────────

test_that("wilcoxon_pruner does not prune during startup", {
  study <- create_study("minimize",
    pruner = wilcoxon_pruner(p_threshold = 0.05, n_startup_trials = 5L))
  study$optimize(function(trial) {
    trial$report(stats::runif(1), step = 1L)
    if (trial$should_prune()) stop_prune()
    stats::runif(1)
  }, n_trials = 4)
  states <- sapply(study$trials, `[[`, "state")
  expect_true(all(states == "complete"))
})

test_that("wilcoxon_pruner prunes significantly worse trials", {
  study <- create_study("minimize",
    pruner = wilcoxon_pruner(p_threshold = 0.15, n_startup_trials = 3L))
  count <- 0L
  study$optimize(function(trial) {
    count <<- count + 1L
    val <- if (count <= 3L) 0.1 else 100.0
    trial$report(val, step = 1L)
    if (trial$should_prune()) stop_prune()
    val
  }, n_trials = 8)
  states <- sapply(study$trials, `[[`, "state")
  expect_true(any(states == "pruned"))
})
