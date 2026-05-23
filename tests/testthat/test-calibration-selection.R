test_that("test.conditions handling allows outcome-only calibration tests", {
  expect_identical(
    .normalize_test_conditions(NULL, c("A", "B"), test.outcome = TRUE),
    character(0)
  )

  expect_identical(
    .normalize_test_conditions(c(" A ", "A", "B"), c("A", "B"), test.outcome = FALSE),
    c("A", "B")
  )

  expect_error(
    .normalize_test_conditions(NULL, c("A", "B"), test.outcome = FALSE),
    "`test.conditions` can be NULL only when `test.outcome = TRUE`.",
    fixed = TRUE
  )

  expect_error(
    .normalize_test_conditions("Y", c("A", "B"), test.outcome = TRUE),
    "All `test.conditions` must be contained in `conditions`.",
    fixed = TRUE
  )
})

test_that("calibration specs can include the outcome only when requested", {
  spec <- qcaert_schema_calib6_spec()
  spec$Y <- list(
    raw = "Y_raw",
    type = "fuzzy",
    method = "direct",
    thresholds = c(10, 20, 30, 70, 80, 90),
    calibrate = list(logistic = FALSE)
  )

  normalized <- .normalize_calib_specs(
    conditions = c("A", "B"),
    outcome = "Y",
    calib_spec = spec,
    test.outcome = TRUE
  )

  expect_identical(names(normalized), c("A", "B", "Y"))
  expect_identical(normalized$Y$raw, "Y_raw")
  expect_identical(normalized$Y$method, "direct")

  expect_error(
    .normalize_calib_specs(
      conditions = c("A", "B"),
      outcome = "Y",
      calib_spec = qcaert_schema_calib6_spec(),
      test.outcome = TRUE
    ),
    "`calib_spec` must contain exactly one entry for each condition and the outcome when `test.outcome = TRUE`.",
    fixed = TRUE
  )

  expect_error(
    .normalize_calib_specs(
      conditions = c("A", "B"),
      outcome = "Y",
      calib_spec = spec
    ),
    "`calib_spec` must contain exactly one entry for each condition.",
    fixed = TRUE
  )

  expect_error(
    .normalize_calib_specs(
      conditions = c("A", "B"),
      outcome = "Y",
      calib_spec = NULL,
      test.outcome = FALSE
    ),
    "`calib_spec` must be supplied.",
    fixed = TRUE
  )
})

test_that("calibration context separates condition specs from outcome spec", {
  raw <- qcaert_schema_raw()
  raw$Y_raw <- raw$A_raw
  spec <- qcaert_schema_calib6_spec()
  spec$Y <- list(
    raw = "Y_raw",
    type = "fuzzy",
    method = "direct",
    thresholds = c(10, 20, 30, 70, 80, 90),
    calibrate = list(logistic = FALSE)
  )

  context <- .prepare_calib_context(
    conditions = c("A", "B"),
    outcome = "Y",
    calib_spec = spec,
    test.outcome = TRUE,
    raw.data = raw,
    unit_step = 1,
    unit_step_divisor = NULL,
    anchors_to_test = NULL,
    caller = "test"
  )

  expect_identical(names(context$calib_specs), c("A", "B", "Y"))
  expect_identical(names(context$condition_specs), c("A", "B"))
  expect_identical(context$outcome_spec$raw, "Y_raw")
  expect_identical(names(context$thresholds), c("A", "B"))
  expect_identical(names(context$thresholds_targets), c("A", "B", "Y"))
})
