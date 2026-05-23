test_that("calib.test can test only the outcome calibration", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_outcome_calibration()
  out <- suppressWarnings(calib.test(
    raw.data = fixture$raw,
    calib.data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    calib_spec = fixture$calib_spec,
    test.conditions = NULL,
    test.outcome = TRUE,
    unit_step = 1,
    max_steps = 1,
    incl.cut = 0.75,
    n.cut = 1,
    solution = "conservative",
    progress = FALSE
  ))

  expect_qcaert_result_structure(
    out,
    class = "calib_test",
    diagnostics = c("set", "role", "method", "anchor", "direction", "change_kind"),
    results = c("set", "role", "method", "anchor", "direction", "start", "reason"),
    settings = c("outcome", "conditions", "test.conditions", "test.outcome", "calib_spec"),
    top = c("bounds", "baseline", "by_set", "settings")
  )
  expect_identical(unique(out$results$set), "Y")
  expect_identical(unique(out$results$role), "outcome")
  expect_identical(names(out$by_set), "Y")
  expect_identical(out$settings$test.conditions, character(0))
  expect_true(out$settings$test.outcome)
})

test_that("calib.test can test selected conditions and the outcome together", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_outcome_calibration()
  out <- suppressWarnings(calib.test(
    raw.data = fixture$raw,
    calib.data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    calib_spec = fixture$calib_spec,
    test.conditions = "A",
    test.outcome = TRUE,
    unit_step = 1,
    max_steps = 1,
    incl.cut = 0.75,
    n.cut = 1,
    solution = "conservative",
    progress = FALSE
  ))

  expect_identical(unique(out$results$set), c("A", "Y"))
  expect_identical(unique(out$results$role), c("condition", "outcome"))
  expect_identical(names(out$by_set), c("A", "Y"))
  expect_identical(nrow(out$results), 24L)
})

test_that("calib.test requires outcome calibration specs for outcome testing", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_direct6()

  expect_error(
    calib.test(
      raw.data = fixture$raw,
      calib.data = fixture$calib,
      outcome = fixture$outcome,
      conditions = fixture$conditions,
      calib_spec = fixture$calib_spec,
      test.conditions = NULL,
      test.outcome = TRUE,
      unit_step = 1,
      max_steps = 1,
      incl.cut = 0.75,
      n.cut = 1,
      solution = "conservative",
      progress = FALSE
    ),
    "`calib_spec` must contain exactly one entry for each condition and the outcome when `test.outcome = TRUE`.",
    fixed = TRUE
  )
})
