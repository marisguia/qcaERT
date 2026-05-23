test_that("integerish n.cut validator now rejects decimals", {
  expect_error(
    qcaERT:::.as_integerish_scalar(1.5, "n.cut", min = 1L),
    "`n.cut` must be integer-like.",
    fixed = TRUE
  )
})

test_that("integerish n.cut validator now rejects zero", {
  expect_error(
    qcaERT:::.as_integerish_scalar(0, "n.cut", min = 1L),
    "`n.cut` must be >= 1.",
    fixed = TRUE
  )
})

test_that("integerish n.cut validator accepts whole numbers", {
  expect_identical(
    qcaERT:::.as_integerish_scalar(1, "n.cut", min = 1L),
    1L
  )

  expect_identical(
    qcaERT:::.as_integerish_scalar(2.0, "n.cut", min = 1L),
    2L
  )
})
