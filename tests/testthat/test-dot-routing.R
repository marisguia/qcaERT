test_that("truthTable/minimize dot split preserves minimize-only arguments", {
  dots <- list(all.sol = TRUE, details = TRUE)

  split <- qcaERT:::.split_truth_table_minimize_dots(dots)

  expect_length(split$tt, 0)
  expect_identical(split$min, dots)
})

test_that("truthTable/minimize dot split separates truthTable and minimize arguments", {
  dots <- list(pri.cut = 0.7, all.sol = TRUE, include = "?")

  split <- qcaERT:::.split_truth_table_minimize_dots(dots)

  expect_identical(split$tt, list(pri.cut = 0.7))
  expect_identical(split$min, list(all.sol = TRUE))
})

test_that("boundary and sampled siblings forward truthTable dots", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_direct6()

  incl <- qcaert_expect_no_warning(incl.test(
    data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    incl.cut = 0.75,
    step = 0.1,
    max_steps = 1,
    n.cut = 1,
    solution = "conservative",
    complete = TRUE,
    pri.cut = 0.6,
    all.sol = TRUE,
    progress = FALSE
  ))

  expect_true(incl$baseline$tt$options$complete)
  expect_equal(incl$baseline$tt$options$pri.cut, 0.6)

  ncut <- qcaert_expect_no_warning(ncut.test(
    data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    n.cut = 1,
    step = 1,
    max_steps = 1,
    incl.cut = 0.75,
    solution = "conservative",
    complete = TRUE,
    pri.cut = 0.6,
    all.sol = TRUE,
    progress = FALSE
  ))

  expect_true(ncut$baseline$tt$options$complete)
  expect_equal(ncut$baseline$tt$options$pri.cut, 0.6)

  altset <- qcaert_expect_no_warning(altset.test(
    raw.data = fixture$raw,
    calib.data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    calib_spec = fixture$calib_spec,
    test.conditions = fixture$test.conditions,
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
    complete = TRUE,
    pri.cut = 0.6,
    all.sol = TRUE,
    progress = FALSE
  ))

  expect_true(altset$baseline$result$tt$options$complete)
  expect_equal(altset$baseline$result$tt$options$pri.cut, 0.6)
})

test_that("calib.test accepts split truthTable and minimize dots", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_direct6()

  out <- qcaert_expect_no_warning(calib.test(
    raw.data = fixture$raw,
    calib.data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    calib_spec = fixture$calib_spec,
    test.conditions = fixture$test.conditions,
    anchors_to_test = "E1",
    unit_step = 1,
    max_steps = 1,
    incl.cut = 0.75,
    n.cut = 1,
    solution = "conservative",
    complete = TRUE,
    pri.cut = 0.6,
    all.sol = TRUE,
    progress = FALSE
  ))

  expect_s3_class(out, "calib_test")
  expect_identical(out$baseline$status, "ok")
})
