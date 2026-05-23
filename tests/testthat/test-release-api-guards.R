test_that("calibration robustness functions expose only calib_spec for calibration inputs", {
  fns <- c("calib.test", "altset.test", "loo.test", "subsample.test")
  blocked <- c(
    "thresholds",
    "type",
    "raw_conditions",
    "raw.conditions",
    "raw_condition",
    "raw.condition",
    "raw_outcome",
    "raw.outcome",
    "calibrate_args",
    "calibrate.args",
    "calib_args",
    "calib.args"
  )

  for (fn in fns) {
    args <- names(formals(get(fn, envir = asNamespace("qcaERT"))))
    expect_true("calib_spec" %in% args, info = fn)
    expect_false(any(blocked %in% args), info = fn)
  }
})

test_that("top-level calibration inputs are rejected through dots", {
  expect_error(
    .reject_calibration_inputs_in_dots(
      list(thresholds = list(A = c(1, 2, 3))),
      "calib.test"
    ),
    "Calibration inputs must be supplied through `calib_spec`.",
    fixed = TRUE
  )

  expect_error(
    .reject_calibration_inputs_in_dots(
      list(calibrate_args = list(logistic = FALSE)),
      "altset.test"
    ),
    "Calibration inputs must be supplied through `calib_spec`.",
    fixed = TRUE
  )
})

test_that("public calibration functions reject top-level calibration inputs before analysis", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_direct6()
  msg <- "Calibration inputs must be supplied through `calib_spec`."

  expect_error(
    calib.test(
      raw.data = fixture$raw,
      calib.data = fixture$calib,
      outcome = fixture$outcome,
      conditions = fixture$conditions,
      calib_spec = fixture$calib_spec,
      test.conditions = fixture$test.conditions,
      solution = "conservative",
      thresholds = list(A = c(10, 20, 30)),
      progress = FALSE
    ),
    msg,
    fixed = TRUE
  )

  expect_error(
    altset.test(
      raw.data = fixture$raw,
      calib.data = fixture$calib,
      outcome = fixture$outcome,
      conditions = fixture$conditions,
      calib_spec = fixture$calib_spec,
      test.conditions = fixture$test.conditions,
      solution = "conservative",
      n_draws = 1,
      type = "fuzzy",
      progress = FALSE
    ),
    msg,
    fixed = TRUE
  )

  expect_error(
    loo.test(
      data = fixture$calib,
      outcome = fixture$outcome,
      conditions = fixture$conditions,
      cases = 1,
      calib = "fixed",
      solution = "conservative",
      calibrate_args = list(logistic = FALSE),
      progress = FALSE
    ),
    msg,
    fixed = TRUE
  )

  expect_error(
    subsample.test(
      data = fixture$calib,
      outcome = fixture$outcome,
      conditions = fixture$conditions,
      calib = "fixed",
      sample_n = 8,
      reps = 1,
      solution = "conservative",
      raw_conditions = c(A = "A_raw"),
      progress = FALSE
    ),
    msg,
    fixed = TRUE
  )
})

test_that("old exclusion spellings are rejected through dots", {
  expect_error(
    .reject_exclusion_controls_in_dots(list(exclude_spec = list(type = 2)), "incl.test"),
    "Use `exclude_static`",
    fixed = TRUE
  )

  expect_error(
    .reject_exclusion_controls_in_dots(list(exclude = 1:2), "ncut.test"),
    "Use `exclude_static`",
    fixed = TRUE
  )

  expect_error(
    .reject_exclusion_controls_in_dots(list(omit = 1:2), "theory.test"),
    "Use `exclude_static`",
    fixed = TRUE
  )
})

test_that("plot solution selectors are reserved for solution_type", {
  skip_if_not_installed("QCA")
  skip_if_not_installed("ggplot2")

  fixture <- qcaert_fixture_direct6()
  out <- suppressWarnings(incl.test(
    data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    incl.cut = 0.75,
    step = 0.1,
    max_steps = 1,
    n.cut = 1,
    solution = "all",
    dir.exp = fixture$dir.exp,
    progress = FALSE
  ))

  expect_error(
    plot(out, solution = "conservative"),
    "Use `solution_type`",
    fixed = TRUE
  )
  expect_error(
    plot(out, monitored_solutions = "conservative"),
    "Use `solution_type`",
    fixed = TRUE
  )
})
