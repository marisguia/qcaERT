test_that("incl.test solution = all keeps independent solution_type paths", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_direct6()

  all <- qcaert_expect_no_warning(incl.test(
    data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    incl.cut = 0.75,
    step = 0.1,
    max_steps = 2,
    n.cut = 1,
    solution = "all",
    dir.exp = fixture$dir.exp,
    progress = FALSE
  ))

  con <- qcaert_expect_no_warning(incl.test(
    data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    incl.cut = 0.75,
    step = 0.1,
    max_steps = 2,
    n.cut = 1,
    solution = "conservative",
    progress = FALSE
  ))

  expect_false("solution" %in% names(all$results))
  expect_false("reason" %in% names(all$results))
  expect_setequal(all$diagnostics$solution_type, all$settings$monitored_solutions)
  expect_equal(all$results$con_last_safe, con$results$last_safe)
  expect_equal(all$results$con_first_failing, con$results$first_failing)
  expect_equal(all$results$con_steps, con$results$steps)
  expect_true(is.matrix(all$bounds))

  long <- qcaert_expect_no_warning(incl.test(
    data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    incl.cut = 0.75,
    step = 0.1,
    max_steps = 2,
    n.cut = 1,
    solution = "all",
    dir.exp = fixture$dir.exp,
    result_shape = "long",
    progress = FALSE
  ))

  expect_true("solution_type" %in% names(long$results))
  expect_false("solution" %in% names(long$results))
  expect_equal(nrow(long$results), nrow(con$results) * length(long$settings$monitored_solutions))
  expect_equal(long$results$last_safe[long$results$solution_type == "conservative"], con$results$last_safe)
  expect_equal(long$results$first_failing[long$results$solution_type == "conservative"], con$results$first_failing)
  expect_equal(long$results$steps[long$results$solution_type == "conservative"], con$results$steps)
})

test_that("ncut.test solution = all keeps independent solution_type paths", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_direct6()

  all <- qcaert_expect_no_warning(ncut.test(
    data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    n.cut = 1,
    step = 1,
    max_steps = 2,
    incl.cut = 0.75,
    solution = "all",
    dir.exp = fixture$dir.exp,
    progress = FALSE
  ))

  con <- qcaert_expect_no_warning(ncut.test(
    data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    n.cut = 1,
    step = 1,
    max_steps = 2,
    incl.cut = 0.75,
    solution = "conservative",
    progress = FALSE
  ))

  expect_false("solution" %in% names(all$results))
  expect_false("reason" %in% names(all$results))
  expect_setequal(all$diagnostics$solution_type, all$settings$monitored_solutions)
  expect_equal(all$results$con_last_safe, con$results$last_safe)
  expect_equal(all$results$con_first_failing, con$results$first_failing)
  expect_equal(all$results$con_steps, con$results$steps)
  expect_true(is.matrix(all$bounds))

  long <- qcaert_expect_no_warning(ncut.test(
    data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    n.cut = 1,
    step = 1,
    max_steps = 2,
    incl.cut = 0.75,
    solution = "all",
    dir.exp = fixture$dir.exp,
    result_shape = "long",
    progress = FALSE
  ))

  expect_true("solution_type" %in% names(long$results))
  expect_false("solution" %in% names(long$results))
  expect_equal(nrow(long$results), nrow(con$results) * length(long$settings$monitored_solutions))
  expect_equal(long$results$last_safe[long$results$solution_type == "conservative"], con$results$last_safe)
  expect_equal(long$results$first_failing[long$results$solution_type == "conservative"], con$results$first_failing)
  expect_equal(long$results$steps[long$results$solution_type == "conservative"], con$results$steps)
})

test_that("calib.test solution = all keeps independent solution_type paths", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_direct6()

  all <- qcaert_expect_no_warning(calib.test(
    raw.data = fixture$raw,
    calib.data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    calib_spec = fixture$calib_spec,
    test.conditions = fixture$test.conditions,
    unit_step = 1,
    max_steps = 1,
    incl.cut = 0.75,
    n.cut = 1,
    solution = "all",
    dir.exp = fixture$dir.exp,
    progress = FALSE
  ))

  con <- qcaert_expect_no_warning(calib.test(
    raw.data = fixture$raw,
    calib.data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    calib_spec = fixture$calib_spec,
    test.conditions = fixture$test.conditions,
    unit_step = 1,
    max_steps = 1,
    incl.cut = 0.75,
    n.cut = 1,
    solution = "conservative",
    progress = FALSE
  ))

  expect_false("solution" %in% names(all$results))
  expect_false("reason" %in% names(all$results))
  expect_setequal(all$diagnostics$solution_type, all$settings$monitored_solutions)
  expect_equal(nrow(all$diagnostics), nrow(con$diagnostics) * length(all$settings$monitored_solutions))
  expect_equal(all$results$con_last_safe, con$results$last_safe)
  expect_equal(all$results$con_first_failing, con$results$first_failing)
  expect_equal(all$results$con_steps, con$results$steps)
  expect_true(is.list(all$bounds[[fixture$test.conditions[1L]]]))

  long <- qcaert_expect_no_warning(calib.test(
    raw.data = fixture$raw,
    calib.data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    calib_spec = fixture$calib_spec,
    test.conditions = fixture$test.conditions,
    unit_step = 1,
    max_steps = 1,
    incl.cut = 0.75,
    n.cut = 1,
    solution = "all",
    dir.exp = fixture$dir.exp,
    result_shape = "long",
    progress = FALSE
  ))

  expect_true("solution_type" %in% names(long$results))
  expect_false("solution" %in% names(long$results))
  expect_equal(nrow(long$results), nrow(con$results) * length(long$settings$monitored_solutions))
  expect_equal(long$results$last_safe[long$results$solution_type == "conservative"], con$results$last_safe)
  expect_equal(long$results$first_failing[long$results$solution_type == "conservative"], con$results$first_failing)
  expect_equal(long$results$steps[long$results$solution_type == "conservative"], con$results$steps)
})

test_that("calib.test exposes intermediate branch selection through i_mode", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_direct6()

  out <- qcaert_expect_no_warning(calib.test(
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
    solution = "intermediate",
    dir.exp = fixture$dir.exp,
    i_mode = "C1P1",
    progress = FALSE
  ))

  expect_identical(out$settings$i_mode, "C1P1")
  expect_identical(out$settings$monitored_solutions, "intermediate")
  expect_true(all(grepl("^C1P1:", out$baseline$sig$intermediate)))
})
