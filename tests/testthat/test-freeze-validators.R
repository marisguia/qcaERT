test_that("ncut.test rejects non-integer which_M", {
  skip_if_not_installed("QCA")

  expect_error(
    ncut.test(
      data = tiny_qca,
      outcome = "Y",
      conditions = c("A", "B"),
      which_M = 1.9,
      progress = FALSE
    ),
    "`which_M` must be integer-like.",
    fixed = TRUE
  )
})

test_that("sol.df rejects non-QCA objects", {
  expect_error(
    sol.df(conservative = mtcars),
    "`conservative` must be a `QCA_min` object or NULL.",
    fixed = TRUE
  )
})
