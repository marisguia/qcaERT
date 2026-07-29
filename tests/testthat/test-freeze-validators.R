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

test_that("ncut.test rejects a baseline n.cut above the number of cases", {
  skip_if_not_installed("QCA")

  expect_error(
    ncut.test(
      data = tiny_qca,
      outcome = "Y",
      conditions = c("A", "B"),
      n.cut = nrow(tiny_qca) + 1L,
      progress = FALSE
    ),
    "`n.cut` must not exceed the number of rows in `data`.",
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

test_that("paired-row validation distinguishes positions from case identifiers", {
  positional_x <- data.frame(value = 1:3)
  positional_y <- data.frame(value = 4:6)
  rownames(positional_y) <- as.character(seq_len(nrow(positional_y)))

  expect_true(.validate_row_alignment(
    positional_x,
    positional_y,
    x_name = "x",
    y_name = "y"
  ))

  expect_error(
    .validate_row_alignment(
      positional_x,
      positional_y[-1, , drop = FALSE],
      x_name = "x",
      y_name = "y"
    ),
    "`x` and `y` must have the same number of rows.",
    fixed = TRUE
  )

  identified_x <- positional_x
  identified_y <- positional_y
  case_ids <- paste0("case_", seq_len(nrow(identified_x)))
  rownames(identified_x) <- case_ids
  rownames(identified_y) <- case_ids

  expect_true(.validate_row_alignment(
    identified_x,
    identified_y,
    x_name = "x",
    y_name = "y"
  ))

  expect_error(
    .validate_row_alignment(
      identified_x,
      identified_y[rev(seq_len(nrow(identified_y))), , drop = FALSE],
      x_name = "x",
      y_name = "y"
    ),
    "`x` and `y` must have identical unique row names or case identifiers in the same order.",
    fixed = TRUE
  )

  expect_error(
    .validate_row_alignment(
      identified_x,
      positional_y,
      x_name = "x",
      y_name = "y"
    ),
    "Cannot verify row alignment between `x` and `y`",
    fixed = TRUE
  )

  duplicated_x <- matrix(
    seq_len(3),
    ncol = 1,
    dimnames = list(c("case_1", "case_1", "case_3"), "value")
  )
  duplicated_y <- duplicated_x

  expect_error(
    .validate_row_alignment(
      duplicated_x,
      duplicated_y,
      x_name = "x",
      y_name = "y"
    ),
    "must be non-missing, non-empty, and unique.",
    fixed = TRUE
  )

  case_labelled_y <- positional_y
  expect_true(.validate_row_alignment(
    identified_x,
    case_labelled_y,
    x_name = "x",
    y_name = "y",
    y_case_ids = case_ids
  ))
})
