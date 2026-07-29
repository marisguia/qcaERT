test_that("altset.test can perturb only the outcome calibration", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_outcome_calibration()
  out <- qcaert_expect_no_warning(altset.test(
    raw.data = fixture$raw,
    calib.data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    calib_spec = fixture$calib_spec,
    test.conditions = NULL,
    test.outcome = TRUE,
    anchors_to_test = c("E1", "C1", "I1"),
    unit_step = 1,
    calib_max_steps = 1,
    incl.cut = 0.75,
    incl_step = 0.1,
    incl_max_steps = 1,
    n.cut = 1,
    ncut_step = 1,
    ncut_max_steps = 1,
    n_draws = 2,
    seed = 101,
    solution = "conservative",
    progress = FALSE
  ))

  expect_qcaert_result_structure(
    out,
    class = "altset_test",
    diagnostics = c("draw", "status", "changed_sets", "changed_roles", "n_changed_sets"),
    results = c("draw", "status", "changed_sets", "changed_roles", "n_changed_sets"),
    settings = c("outcome", "conditions", "test.conditions", "test.outcome", "calib_spec"),
    top = c("summary", "baseline", "by_draw", "settings")
  )

  expect_identical(out$settings$test.conditions, character(0))
  expect_true(out$settings$test.outcome)

  draw_sets <- lapply(out$diagnostics$calibration, names)
  expect_true(all(vapply(draw_sets, identical, logical(1), fixture$outcome)))

  draw_roles <- unlist(lapply(out$diagnostics$calibration, function(x) {
    vapply(x, function(y) y$role, character(1))
  }), use.names = FALSE)
  expect_identical(unique(draw_roles), "outcome")
})

test_that("altset.test can perturb selected conditions and the outcome together", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_outcome_calibration()
  out <- qcaert_expect_no_warning(altset.test(
    raw.data = fixture$raw,
    calib.data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    calib_spec = fixture$calib_spec,
    test.conditions = "A",
    test.outcome = TRUE,
    anchors_to_test = c("E1", "C1", "I1"),
    unit_step = 1,
    calib_max_steps = 1,
    incl.cut = 0.75,
    incl_step = 0.1,
    incl_max_steps = 1,
    n.cut = 1,
    ncut_step = 1,
    ncut_max_steps = 1,
    n_draws = 2,
    seed = 202,
    solution = "conservative",
    progress = FALSE
  ))

  draw_sets <- lapply(out$diagnostics$calibration, names)
  expect_true(all(vapply(draw_sets, identical, logical(1), c("A", fixture$outcome))))

  draw_roles <- lapply(out$diagnostics$calibration, function(x) {
    vapply(x, function(y) y$role, character(1))
  })
  expect_true(all(vapply(draw_roles, identical, logical(1), c(A = "condition", Y = "outcome"))))
})

test_that("altset.test requires outcome calibration specs for outcome testing", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_direct6()

  expect_error(
    altset.test(
      raw.data = fixture$raw,
      calib.data = fixture$calib,
      outcome = fixture$outcome,
      conditions = fixture$conditions,
      calib_spec = fixture$calib_spec,
      test.conditions = NULL,
      test.outcome = TRUE,
      unit_step = 1,
      calib_max_steps = 1,
      incl.cut = 0.75,
      incl_step = 0.1,
      incl_max_steps = 1,
      n.cut = 1,
      ncut_step = 1,
      ncut_max_steps = 1,
      n_draws = 2,
      seed = 303,
      solution = "conservative",
      progress = FALSE
    ),
    "`calib_spec` must contain exactly one entry for each condition and the outcome when `test.outcome = TRUE`.",
    fixed = TRUE
  )
})

test_that("altset.test rejects mismatched baseline memberships for tested sets", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_direct6()
  fixture$calib$A <- 1 - fixture$calib$A

  expect_error(
    altset.test(
      raw.data = fixture$raw,
      calib.data = fixture$calib,
      outcome = fixture$outcome,
      conditions = fixture$conditions,
      calib_spec = fixture$calib_spec,
      test.conditions = "A",
      anchors_to_test = "E1",
      unit_step = 1,
      calib_max_steps = 1,
      incl.cut = 0.75,
      incl_step = 0.1,
      incl_max_steps = 1,
      n.cut = 1,
      ncut_step = 1,
      ncut_max_steps = 1,
      n_draws = 1,
      solution = "conservative",
      exclude_mode = "none",
      progress = FALSE
    ),
    paste0(
      "`calib.data[['A']]` does not match the memberships reconstructed from ",
      "`raw.data[['A_raw']]` and `calib_spec[['A']]`."
    ),
    fixed = TRUE
  )
})

test_that("altset.test rejects mismatched baseline outcome memberships", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_outcome_calibration()
  fixture$calib$Y <- 1 - fixture$calib$Y

  expect_error(
    altset.test(
      raw.data = fixture$raw,
      calib.data = fixture$calib,
      outcome = fixture$outcome,
      conditions = fixture$conditions,
      calib_spec = fixture$calib_spec,
      test.conditions = NULL,
      test.outcome = TRUE,
      anchors_to_test = "E1",
      unit_step = 1,
      calib_max_steps = 1,
      incl.cut = 0.75,
      incl_step = 0.1,
      incl_max_steps = 1,
      n.cut = 1,
      ncut_step = 1,
      ncut_max_steps = 1,
      n_draws = 1,
      solution = "conservative",
      exclude_mode = "none",
      progress = FALSE
    ),
    paste0(
      "`calib.data[['Y']]` does not match the memberships reconstructed from ",
      "`raw.data[['Y_raw']]` and `calib_spec[['Y']]`."
    ),
    fixed = TRUE
  )
})

test_that("altset.test does not validate untested condition memberships", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_direct6()
  fixture$calib$B <- 1 - fixture$calib$B

  out <- qcaert_expect_no_warning(altset.test(
    raw.data = fixture$raw,
    calib.data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    calib_spec = fixture$calib_spec,
    test.conditions = "A",
    anchors_to_test = "E1",
    unit_step = 1,
    calib_max_steps = 1,
    incl.cut = 0.75,
    incl_step = 0.1,
    incl_max_steps = 1,
    n.cut = 1,
    ncut_step = 1,
    ncut_max_steps = 1,
    n_draws = 1,
    seed = 101,
    solution = "conservative",
    exclude_mode = "none",
    progress = FALSE
  ))

  expect_s3_class(out, "altset_test")
})
