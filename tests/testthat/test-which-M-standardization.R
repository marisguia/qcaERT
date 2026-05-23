test_that("coerce_which_M now rejects non-integer numerics", {
  expect_error(
    qcaERT:::.coerce_which_M(1.9),
    "`which_M` must be integer-like.",
    fixed = TRUE
  )
})

test_that("coerce_which_M still accepts integer-like input", {
  expect_identical(
    qcaERT:::.coerce_which_M(1),
    1L
  )

  expect_identical(
    qcaERT:::.coerce_which_M(1.0),
    1L
  )
})
