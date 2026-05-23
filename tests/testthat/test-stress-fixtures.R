test_that("shared direct-six and indirect fixtures are coherent", {
  skip_if_not_installed("QCA")

  direct <- qcaert_fixture_direct6()
  indirect <- qcaert_fixture_indirect()

  expect_identical(direct$outcome, "Y")
  expect_identical(direct$conditions, c("A", "B"))
  expect_identical(direct$calib_spec$A$method, "direct")
  expect_identical(length(direct$calib_spec$A$thresholds), 6L)
  expect_true(all(direct$calib$A >= 0 & direct$calib$A <= 1))

  expect_identical(indirect$outcome, "Y")
  expect_identical(indirect$conditions, c("A", "B"))
  expect_identical(indirect$calib_spec$A$method, "indirect")
  expect_identical(length(indirect$calib_spec$A$thresholds), 3L)
  expect_true(all(indirect$calib$A >= 0 & indirect$calib$A <= 1))
})

test_that("shared cluster fixture has truth table and repeated units", {
  skip_if_not_installed("QCA")

  cluster <- qcaert_fixture_cluster()

  expect_s3_class(cluster$truth_table, "QCA_tt")
  expect_identical(cluster$cluster_id, "cluster")
  expect_identical(cluster$unit_id, "unit")
  expect_true(any(duplicated(cluster$data$unit)))
  expect_setequal(unique(cluster$data$cluster), c("C1", "C2"))
})

test_that("shared LR fixture builds evaluated QCA solution objects", {
  skip_if_not_installed("QCA")

  lr <- qcaert_fixture_lr_solutions()

  expect_identical(lr$fixture$outcome, "SURV")
  expect_identical(lr$fixture$conditions, c("DEV", "URB", "LIT", "IND", "STB"))
  expect_identical(length(lr$fixture$thresholds$DEV), 6L)
  expect_identical(length(lr$fixture$thresholds$URB), 3L)
  expect_s3_class(lr$truth_table, "QCA_tt")
  expect_s3_class(lr$conservative, "QCA_min")
  expect_s3_class(lr$intermediate, "QCA_min")
  expect_s3_class(lr$parsimonious, "QCA_min")
})
