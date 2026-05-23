test_that("reduced-run recalibration accepts the common calib_spec structure", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_outcome_calibration()
  analysis_vars <- c(fixture$outcome, fixture$conditions)

  spec <- .reduced_validate_calib_spec(
    calib_spec = fixture$calib_spec,
    analysis_vars = analysis_vars,
    raw.data = fixture$raw,
    data = fixture$calib
  )

  expect_identical(names(spec), analysis_vars)
  expect_identical(spec$Y$qca_type, "fuzzy")
  expect_identical(spec$A$method, "direct")
  expect_equal(unname(spec$A$thresholds), c(10, 20, 30, 70, 80, 90))
  expect_null(spec$A$findTh)

  rebuilt <- .reduced_recalibrate_dataset(
    data_step = fixture$calib[-1, , drop = FALSE],
    raw_step = fixture$raw[-1, , drop = FALSE],
    analysis_vars = analysis_vars,
    calib_spec = spec
  )

  expect_null(rebuilt$error)
  expect_true(all(analysis_vars %in% names(rebuilt$data)))
  expect_identical(names(rebuilt$thresholds), analysis_vars)
  expect_true(all(vapply(rebuilt$thresholds, length, integer(1)) > 0L))
  expect_identical(rebuilt$calls$A$method, "direct")
})

test_that("reduced-run recalibration preserves optional findTh recomputation", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_outcome_calibration()
  analysis_vars <- c(fixture$outcome, fixture$conditions)
  calib_spec <- fixture$calib_spec
  calib_spec$A$findTh <- list(groups = 4)

  spec <- .reduced_validate_calib_spec(
    calib_spec = calib_spec,
    analysis_vars = analysis_vars,
    raw.data = fixture$raw,
    data = fixture$calib
  )

  expect_identical(spec$A$findTh, list(groups = 4))

  rebuilt <- .reduced_recalibrate_dataset(
    data_step = fixture$calib[-1, , drop = FALSE],
    raw_step = fixture$raw[-1, , drop = FALSE],
    analysis_vars = analysis_vars,
    calib_spec = spec
  )

  expect_null(rebuilt$error)
  expect_identical(rebuilt$calls$A$findTh, list(groups = 4))
  expect_true(length(rebuilt$thresholds$A) %in% c(3L, 6L))
})

test_that("loo.test and subsample.test use the common calib_spec structure when recalibrating", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_outcome_calibration()

  loo_out <- suppressWarnings(loo.test(
    data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    cases = 1L,
    calib = "recompute",
    raw.data = fixture$raw,
    calib_spec = fixture$calib_spec,
    incl.cut = 0.75,
    n.cut = 1,
    solution = "conservative",
    progress = FALSE
  ))

  expect_s3_class(loo_out, "loo_test")
  expect_identical(loo_out$settings$calib, "recompute")
  expect_identical(loo_out$baseline$calibration$status, "ok")

  subsample_out <- suppressWarnings(subsample.test(
    data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    calib = "recompute",
    raw.data = fixture$raw,
    calib_spec = fixture$calib_spec,
    sample_n = 8,
    reps = 1,
    seed = 123,
    incl.cut = 0.75,
    n.cut = 1,
    solution = "conservative",
    progress = FALSE
  ))

  expect_s3_class(subsample_out, "subsample_test")
  expect_identical(subsample_out$settings$calib, "recompute")
  expect_identical(subsample_out$baseline$calibration$status, "ok")
})
