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
    tested_sets = "Y",
    raw.data = raw,
    unit_step = c(1, 2, 3),
    unit_step_divisor = NULL,
    anchors_to_test = NULL,
    caller = "test"
  )

  expect_identical(names(context$calib_specs), c("A", "B", "Y"))
  expect_identical(names(context$condition_specs), c("A", "B"))
  expect_identical(context$outcome_spec$raw, "Y_raw")
  expect_identical(names(context$thresholds), c("A", "B"))
  expect_identical(names(context$thresholds_targets), c("A", "B", "Y"))
  expect_identical(context$unit_step_targets, c(A = 1, B = 2, Y = 3))
})

test_that("automatic step sizing skips untested crisp calibrations at the raw-data limit", {
  skip_if_not_installed("QCA")

  raw <- data.frame(A_raw = 1:8, coverage = c(0.2, 0.4, 0.8, 1, 0.35, 0.7, 1, 0.9))
  data <- data.frame(
    A = as.integer(raw$A_raw >= 4.5),
    B = as.integer(raw$coverage == 1),
    Y = as.integer(raw$A_raw >= 4.5)
  )
  args <- list(
    raw.data = raw, calib.data = data, outcome = "Y", conditions = c("A", "B"),
    calib_spec = list(
      A = list(raw = "A_raw", type = "crisp", thresholds = 4.5),
      B = list(raw = "coverage", type = "crisp", thresholds = 1)
    ),
    test.conditions = "A", solution = "conservative", incl.cut = 0.75, progress = FALSE
  )
  calibration <- do.call(calib.test, c(args, list(max_steps = 1)))
  expect_identical(calibration$baseline$status, "ok")
  expect_identical(unique(calibration$results$set), "A")
  expect_equal(calibration$results$step_unit, rep(0.35, 2))
  baseline <- calibration$baseline$by_solution_type$conservative$r_con$tt$initial.data
  expect_equal(baseline$B, data$B)

  alternative <- do.call(altset.test, c(args, list(
    n_draws = 1, seed = 3, calib_max_steps = 1, ncut_max_steps = 1, incl_max_steps = 1
  )))
  draw <- alternative$by_draw[[1L]]$result
  expect_identical(names(draw$draw_meta$calibration), "A")
  expect_identical(draw$data$B, data$B)
})
