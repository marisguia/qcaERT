test_that("public functions reject the outcome as a condition", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_direct6()
  bad_conditions <- c(fixture$conditions, fixture$outcome)
  msg <- "`outcome` must not be included in `conditions`"

  expect_error(
    incl.test(
      data = fixture$calib,
      outcome = fixture$outcome,
      conditions = bad_conditions,
      progress = FALSE
    ),
    msg,
    fixed = TRUE
  )

  expect_error(
    ncut.test(
      data = fixture$calib,
      outcome = fixture$outcome,
      conditions = bad_conditions,
      progress = FALSE
    ),
    msg,
    fixed = TRUE
  )

  expect_error(
    calib.test(
      raw.data = fixture$raw,
      calib.data = fixture$calib,
      outcome = fixture$outcome,
      conditions = bad_conditions,
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
      conditions = bad_conditions,
      progress = FALSE
    ),
    msg,
    fixed = TRUE
  )

  expect_error(
    loo.test(
      data = fixture$calib,
      outcome = fixture$outcome,
      conditions = bad_conditions,
      progress = FALSE
    ),
    msg,
    fixed = TRUE
  )

  expect_error(
    subsample.test(
      data = fixture$calib,
      outcome = fixture$outcome,
      conditions = bad_conditions,
      sample_prop = 0.8,
      progress = FALSE
    ),
    msg,
    fixed = TRUE
  )
})

test_that("cluster.test rejects truth tables whose outcome is also a condition", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_cluster()
  bad_tt <- fixture$truth_table
  bad_tt$options$conditions <- c(fixture$conditions, fixture$outcome)

  expect_error(
    cluster.test(
      data = fixture$data,
      tt = bad_tt,
      cluster_id = fixture$cluster_id,
      unit_id = fixture$unit_id,
      progress = FALSE
    ),
    "`outcome` must not be included in `conditions`",
    fixed = TRUE
  )
})
