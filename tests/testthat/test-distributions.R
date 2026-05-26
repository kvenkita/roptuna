test_that("float_distribution stores bounds and flags", {
  d <- float_distribution(0, 1)
  expect_equal(d$low, 0)
  expect_equal(d$high, 1)
  expect_false(d$log)
  expect_null(d$step)
  expect_s3_class(d, "roptuna_float_distribution")
})

test_that("float_distribution validates low < high", {
  expect_error(float_distribution(1, 0), "low must be less than high")
})

test_that("float_distribution log=TRUE requires positive bounds", {
  expect_error(float_distribution(-1, 1, log = TRUE), "positive")
})

test_that("int_distribution stores bounds", {
  d <- int_distribution(1L, 10L)
  expect_equal(d$low, 1L)
  expect_equal(d$high, 10L)
  expect_s3_class(d, "roptuna_int_distribution")
})

test_that("int_distribution allows low == high (degenerate single-value range)", {
  d <- int_distribution(5L, 5L)
  expect_equal(d$low, 5L)
  expect_equal(d$high, 5L)
  expect_equal(dist_sample_random(d), 5L)
})

test_that("dist_to_list omits NULL step; dist_from_list tolerates missing step", {
  d <- float_distribution(-1, 1)
  lst <- dist_to_list(d)
  expect_null(lst$step)           # NULL step should not be serialized
  d2 <- dist_from_list(lst)
  expect_null(d2$step)            # round-trip preserves NULL
})

test_that("dist_from_list handles JSON-roundtripped NULL step (empty list)", {
  # jsonlite serialises NULL as {} which comes back as list(); must not crash
  lst <- list(name = "FloatDistribution", low = -5, high = 5,
              log = FALSE, step = list())   # simulates {} from JSON
  d <- dist_from_list(lst)
  expect_null(d$step)
  expect_true(is.numeric(dist_sample_random(d)))  # no crash
})

test_that("categorical_distribution stores choices", {
  d <- categorical_distribution(c("a", "b", "c"))
  expect_equal(d$choices, c("a", "b", "c"))
  expect_s3_class(d, "roptuna_categorical_distribution")
})

test_that("categorical_distribution requires at least one choice", {
  expect_error(categorical_distribution(character(0)), "at least one")
})

test_that("dist_sample_random returns float in bounds", {
  set.seed(1)
  d <- float_distribution(0, 1)
  v <- dist_sample_random(d)
  expect_gte(v, 0); expect_lte(v, 1)
})

test_that("dist_to_list and dist_from_list round-trip float", {
  d <- float_distribution(0.1, 1.0, log = TRUE)
  lst <- dist_to_list(d)
  d2 <- dist_from_list(lst)
  expect_equal(d2$low, d$low)
  expect_equal(d2$high, d$high)
  expect_equal(d2$log, d$log)
})

test_that("dist_to_list and dist_from_list round-trip categorical", {
  d <- categorical_distribution(c("a", "b"))
  d2 <- dist_from_list(dist_to_list(d))
  expect_equal(d2$choices, c("a", "b"))
})
