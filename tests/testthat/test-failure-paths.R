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
