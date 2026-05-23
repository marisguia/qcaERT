test_that("sol.df now rejects non-integer which_M up front", {
  expect_error(
    sol.df(which_M = 1.9),
    "`which_M` must be integer-like.",
    fixed = TRUE
  )
})
