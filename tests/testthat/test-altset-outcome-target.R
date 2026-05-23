test_that("altset.test can perturb only the outcome calibration", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_outcome_calibration()
  out <- suppressWarnings(altset.test(
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
  out <- suppressWarnings(altset.test(
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
