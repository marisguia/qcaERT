test_that("boundary tests report staged baseline QCA failures", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_direct6()

  incl_truth_table <- qcaert_expect_no_warning(incl.test(
    data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    incl.cut = 0.75,
    step = 0.1,
    max_steps = 1,
    n.cut = 1,
    solution = "conservative",
    sort.by = "bad",
    progress = FALSE
  ))
  expect_true(all(incl_truth_table$diagnostics$stop_reason == "baseline_truth_table_build_error"))
  expect_true(all(incl_truth_table$diagnostics$error_source == "truthTable"))

  incl_minimize <- qcaert_expect_no_warning(incl.test(
    data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    incl.cut = 0.75,
    step = 0.1,
    max_steps = 1,
    n.cut = 1,
    solution = "conservative",
    show.cases = "bad",
    progress = FALSE
  ))
  expect_true(all(incl_minimize$diagnostics$stop_reason == "baseline_requested_minimize_error"))
  expect_true(all(incl_minimize$diagnostics$error_source == "conservative"))

  ncut_truth_table <- qcaert_expect_no_warning(ncut.test(
    data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    n.cut = 1,
    step = 1,
    max_steps = 1,
    incl.cut = 0.75,
    solution = "conservative",
    sort.by = "bad",
    progress = FALSE
  ))
  expect_true(all(ncut_truth_table$diagnostics$stop_reason == "baseline_truth_table_build_error"))
  expect_true(all(ncut_truth_table$diagnostics$error_source == "truthTable"))
})

test_that("calib.test reports staged baseline QCA failures and missing selected solutions", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_direct6()

  missing_solution <- qcaert_expect_no_warning(calib.test(
    raw.data = fixture$raw,
    calib.data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    calib_spec = fixture$calib_spec,
    test.conditions = fixture$test.conditions[1L],
    anchors_to_test = "E1",
    unit_step = 1,
    max_steps = 1,
    incl.cut = 0.75,
    n.cut = 1,
    solution = "conservative",
    which_M = 99,
    progress = FALSE
  ))
  expect_identical(missing_solution$baseline$status, "baseline_error")
  expect_true(all(missing_solution$diagnostics$stop_reason == "baseline_selected_solution_missing"))
  expect_true(all(missing_solution$diagnostics$error_source == "conservative"))
  expect_true(all(missing_solution$results$reason == "baseline_selected_solution_missing"))

  truth_table_failure <- qcaert_expect_no_warning(calib.test(
    raw.data = fixture$raw,
    calib.data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    calib_spec = fixture$calib_spec,
    test.conditions = fixture$test.conditions[1L],
    anchors_to_test = "E1",
    unit_step = 1,
    max_steps = 1,
    incl.cut = 0.75,
    n.cut = 1,
    solution = "conservative",
    sort.by = "bad",
    progress = FALSE
  ))
  expect_true(all(truth_table_failure$diagnostics$stop_reason == "baseline_truth_table_build_error"))
  expect_true(all(truth_table_failure$diagnostics$error_source == "truthTable"))

  minimize_failure <- qcaert_expect_no_warning(calib.test(
    raw.data = fixture$raw,
    calib.data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    calib_spec = fixture$calib_spec,
    test.conditions = fixture$test.conditions[1L],
    anchors_to_test = "E1",
    unit_step = 1,
    max_steps = 1,
    incl.cut = 0.75,
    n.cut = 1,
    solution = "conservative",
    show.cases = "bad",
    progress = FALSE
  ))
  expect_true(all(minimize_failure$diagnostics$stop_reason == "baseline_requested_minimize_error"))
  expect_true(all(minimize_failure$diagnostics$error_source == "conservative"))

  exclude_failure <- qcaert_expect_no_warning(calib.test(
    raw.data = fixture$raw,
    calib.data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    calib_spec = fixture$calib_spec,
    test.conditions = fixture$test.conditions[1L],
    anchors_to_test = "E1",
    unit_step = 1,
    max_steps = 1,
    incl.cut = 0.75,
    n.cut = 1,
    solution = "parsimonious",
    exclude_mode = "recompute",
    exclude_recompute = list(type = 2, bogus = TRUE),
    progress = FALSE
  ))
  expect_true(all(exclude_failure$diagnostics$stop_reason == "baseline_exclude_recompute_error"))
  expect_true(all(exclude_failure$diagnostics$error_source == "exclude"))
})

test_that("calibration robustness functions reject independently reordered rows", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_direct6()
  case_ids <- paste0("case_", seq_len(nrow(fixture$raw)))
  raw <- fixture$raw
  calib <- fixture$calib
  rownames(raw) <- case_ids
  rownames(calib) <- case_ids
  raw_reordered <- raw[rev(seq_len(nrow(raw))), , drop = FALSE]

  alignment_error <- paste0(
    "`raw.data` and `calib.data` must have identical unique row names ",
    "or case identifiers in the same order."
  )

  expect_error(
    calib.test(
      raw.data = raw_reordered,
      calib.data = calib,
      outcome = fixture$outcome,
      conditions = fixture$conditions,
      calib_spec = fixture$calib_spec,
      progress = FALSE
    ),
    alignment_error,
    fixed = TRUE
  )

  expect_error(
    altset.test(
      raw.data = raw_reordered,
      calib.data = calib,
      outcome = fixture$outcome,
      conditions = fixture$conditions,
      calib_spec = fixture$calib_spec,
      progress = FALSE
    ),
    alignment_error,
    fixed = TRUE
  )
})

test_that("cluster.test rejects data reordered after truth-table construction", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_cluster()
  data <- fixture$data
  rownames(data) <- paste0("case_", seq_len(nrow(data)))
  tt <- qcaert_truth_table(
    data = data,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    incl.cut = 0.75,
    n.cut = 1
  )

  expect_error(
    cluster.test(
      data = data[rev(seq_len(nrow(data))), , drop = FALSE],
      tt = tt,
      cluster_id = fixture$cluster_id,
      unit_id = fixture$unit_id,
      progress = FALSE
    ),
    paste0(
      "`data` and `tt$recoded.data` must have identical unique row names ",
      "or case identifiers in the same order."
    ),
    fixed = TRUE
  )
})

test_that("cluster.test rejects duplicate unit-cluster observations", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_cluster()
  duplicate_data <- fixture$data
  duplicate_data[[fixture$unit_id]][2L] <- duplicate_data[[fixture$unit_id]][1L]

  expect_error(
    cluster.test(
      data = duplicate_data,
      tt = fixture$truth_table,
      cluster_id = fixture$cluster_id,
      unit_id = fixture$unit_id,
      progress = FALSE
    ),
    paste0(
      "`unit_id` and `cluster_id` must identify at most one row per ",
      "unit-cluster combination; duplicate combinations are not aggregated."
    ),
    fixed = TRUE
  )
})

test_that("plot methods explain zero-row filters", {
  skip_if_not_installed("QCA")
  skip_if_not_installed("ggplot2")

  fixture <- qcaert_fixture_direct6()

  incl_out <- qcaert_expect_no_warning(incl.test(
    data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    incl.cut = 0.75,
    step = 0.1,
    max_steps = 1,
    n.cut = 1,
    solution = "conservative",
    progress = FALSE
  ))
  expect_error(
    plot(incl_out, stop_reason = "not_a_recorded_reason"),
    "No rows left to plot after filtering.",
    fixed = TRUE
  )

  calib_out <- qcaert_expect_no_warning(calib.test(
    raw.data = fixture$raw,
    calib.data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    calib_spec = fixture$calib_spec,
    test.conditions = fixture$test.conditions[1L],
    unit_step = 1,
    max_steps = 1,
    incl.cut = 0.75,
    n.cut = 1,
    solution = "conservative",
    progress = FALSE
  ))
  expect_error(
    plot(calib_out, sets = "missing_set"),
    "No rows left to plot after filtering.",
    fixed = TRUE
  )
})

test_that("calib.test returns completed steps when recalibration fails", {
  skip_if_not_installed("QCA")

  raw <- data.frame(A_raw = 0:6)
  data <- data.frame(
    A = QCA::calibrate(raw$A_raw, thresholds = c(2, 3, 5), logistic = FALSE),
    Y = as.integer(raw$A_raw >= 4)
  )
  spec <- list(A = list(
    raw = "A_raw", type = "fuzzy", thresholds = c(E = 2, C = 3, I = 5),
    calibrate = list(logistic = FALSE)
  ))
  calibrate <- .calibrate_from_spec
  local_mocked_bindings(
    .calibrate_from_spec = function(raw_x, spec) {
      if (spec$thresholds[["E"]] == 0) stop("Calibration failed at E = 0.")
      calibrate(raw_x, spec)
    },
    .package = "qcaERT"
  )

  args <- list(
    raw.data = raw, calib.data = data, outcome = "Y", conditions = "A",
    calib_spec = spec, anchors_to_test = "E", unit_step = 1, incl.cut = 0.75,
    solution = "conservative", progress = FALSE
  )
  one_step <- do.call(calib.test, c(args, list(max_steps = 1)))
  expect_identical(one_step$results$reason[one_step$results$direction == "lower"], "run_budget_exhausted")

  two_steps <- do.call(calib.test, c(args, list(max_steps = 2)))
  lower <- two_steps$results[two_steps$results$direction == "lower", ]
  expect_equal(lower$last_safe, 1)
  expect_equal(lower$first_failing, 0)
  expect_identical(lower$steps, 1L)
  expect_identical(lower$reason, "calibration_error")
  expect_identical(two_steps$diagnostics$error_message[two_steps$diagnostics$direction == "lower"],
    "Calibration failed at E = 0.")

  trace <- two_steps$by_set$A$steps$E_lower$trace
  expect_equal(trace$value, c(1, 0))
  expect_identical(trace$status, c("ok", "calibration_error"))
  expect_identical(trace$changed, c(FALSE, NA))
  expect_true("upper" %in% two_steps$results$direction)
})
