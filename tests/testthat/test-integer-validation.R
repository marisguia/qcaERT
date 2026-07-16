test_that("integerish scalar validation distinguishes invalid and valid values", {
  expect_error(
    .as_integerish_scalar(1.5, "value"),
    "`value` must be integer-like.",
    fixed = TRUE
  )
  expect_error(
    .as_integerish_scalar(NA_real_, "value"),
    "`value` must be a single finite integer-like number.",
    fixed = TRUE
  )
  expect_error(
    .as_integerish_scalar(Inf, "value"),
    "`value` must be a single finite integer-like number.",
    fixed = TRUE
  )
  expect_error(
    .as_integerish_scalar(0, "value", min = 1L),
    "`value` must be >= 1.",
    fixed = TRUE
  )

  expect_identical(.as_integerish_scalar(5.0, "value"), 5L)
  expect_identical(.as_integerish_scalar(0.0, "value", min = 0L), 0L)
  expect_identical(.as_integerish_scalar(-1.0, "seed"), -1L)
})

test_that("search budgets reject fractional, non-finite, and zero values", {
  skip_if_not_installed("QCA")

  bad_values <- list(1.5, NA_real_, Inf, 0)
  messages <- c(
    "`max_steps` must be integer-like.",
    "`max_steps` must be a single finite integer-like number.",
    "`max_steps` must be a single finite integer-like number.",
    "`max_steps` must be >= 1."
  )

  for (i in seq_along(bad_values)) {
    expect_error(
      incl.test(
        data = tiny_qca,
        outcome = "Y",
        conditions = c("A", "B"),
        max_steps = bad_values[[i]],
        progress = FALSE
      ),
      messages[[i]],
      fixed = TRUE
    )
  }

  expect_error(
    incl.test(
      data = tiny_qca,
      outcome = "Y",
      conditions = c("A", "B"),
      max_steps = 1.0,
      n.cut = 0,
      progress = FALSE
    ),
    "`n.cut` must be >= 1.",
    fixed = TRUE
  )
})

test_that("calib.test validates integer controls directly", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_direct6()
  base_args <- list(
    raw.data = NULL,
    calib.data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    calib_spec = fixture$calib_spec,
    test.conditions = fixture$test.conditions,
    solution = "conservative",
    progress = FALSE
  )

  bad_values <- list(1.5, NA_real_, Inf, 0)
  suffixes <- c(
    "must be integer-like.",
    "must be a single finite integer-like number.",
    "must be a single finite integer-like number.",
    "must be >= 1."
  )

  for (control in c("max_steps", "n.cut", "which_M")) {
    for (i in seq_along(bad_values)) {
      args <- base_args
      args[[control]] <- bad_values[[i]]
      expect_error(
        do.call(calib.test, args),
        paste0("`", control, "` ", suffixes[[i]]),
        fixed = TRUE
      )
    }
  }

  valid_args <- base_args
  valid_args$max_steps <- 1.0
  valid_args$n.cut <- 1.0
  valid_args$which_M <- 1.0
  expect_error(
    do.call(calib.test, valid_args),
    "`raw.data` must be a non-empty data frame with at least one row.",
    fixed = TRUE
  )
})

test_that("subsample integer controls reject truncation and preserve valid boundaries", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_direct6()
  base_args <- list(
    data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    sample_n = 8,
    reps = 1,
    incl.cut = 0.75,
    solution = "conservative",
    progress = FALSE
  )

  invalid_cases <- list(
    list(control = "sample_n", value = 1.5, message = "`sample_n` must be integer-like."),
    list(control = "sample_n", value = NA_real_, message = "`sample_n` must be a single finite integer-like number."),
    list(control = "sample_n", value = Inf, message = "`sample_n` must be a single finite integer-like number."),
    list(control = "sample_n", value = 0, message = "`sample_n` must be >= 2."),
    list(control = "reps", value = 1.5, message = "`reps` must be integer-like."),
    list(control = "reps", value = NA_real_, message = "`reps` must be a single finite integer-like number."),
    list(control = "reps", value = Inf, message = "`reps` must be a single finite integer-like number."),
    list(control = "reps", value = 0, message = "`reps` must be >= 1."),
    list(control = "seed", value = 1.5, message = "`seed` must be integer-like."),
    list(control = "seed", value = NA_real_, message = "`seed` must be a single finite integer-like number."),
    list(control = "seed", value = Inf, message = "`seed` must be a single finite integer-like number.")
  )

  for (case in invalid_cases) {
    args <- base_args
    args[[case$control]] <- case$value
    expect_error(
      do.call(subsample.test, args),
      case$message,
      fixed = TRUE
    )
  }

  boundary_args <- base_args
  boundary_args$sample_n <- 2.0
  boundary_args$reps <- 1.0
  boundary_args$seed <- 0.0
  boundary_args$incl.cut <- 2
  expect_error(
    do.call(subsample.test, boundary_args),
    "`incl.cut` must be a single finite number in [0, 1].",
    fixed = TRUE
  )

  too_large_args <- base_args
  too_large_args$sample_n <- nrow(fixture$calib)
  expect_error(
    do.call(subsample.test, too_large_args),
    "The realized sample size must be at least 2 and strictly smaller than `nrow(data)`.",
    fixed = TRUE
  )
})

test_that("sample_prop retains intentional rounding", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_direct6()
  out <- qcaert_expect_no_warning(subsample.test(
    data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    sample_prop = 0.75,
    reps = 1.0,
    seed = 0.0,
    incl.cut = 0.75,
    solution = "conservative",
    progress = FALSE
  ))

  expect_identical(out$settings$sample_n, 8L)
  expect_identical(out$settings$reps, 1L)
  expect_identical(out$settings$seed, 0L)
  expect_equal(out$settings$sample_prop_requested, 0.75)
  expect_equal(out$settings$sample_prop_realized, 0.8)
})

test_that("altset seed validation is integer-only without requiring positivity", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_direct6()
  base_args <- list(
    raw.data = fixture$raw,
    calib.data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    calib_spec = fixture$calib_spec,
    test.conditions = fixture$test.conditions,
    solution = "conservative",
    n_draws = 1,
    seed = NULL,
    progress = FALSE
  )

  for (case in list(
    list(value = 1.5, message = "`seed` must be integer-like."),
    list(value = NA_real_, message = "`seed` must be a single finite integer-like number."),
    list(value = Inf, message = "`seed` must be a single finite integer-like number.")
  )) {
    args <- base_args
    args$seed <- case$value
    expect_error(
      do.call(altset.test, args),
      case$message,
      fixed = TRUE
    )
  }

  valid_args <- base_args
  valid_args$seed <- 0.0
  valid_args$which_M <- 0
  expect_error(
    do.call(altset.test, valid_args),
    "`which_M` must be >= 1.",
    fixed = TRUE
  )
})

test_that("display digits reject fractions but accept zero and integer-valued doubles", {
  expect_error(
    sol.df(digits = 1.5),
    "`digits` must be integer-like.",
    fixed = TRUE
  )
  expect_error(
    sol.df(digits = NA_real_),
    "`digits` must be a single finite integer-like number.",
    fixed = TRUE
  )
  expect_error(
    sol.df(digits = Inf),
    "`digits` must be a single finite integer-like number.",
    fixed = TRUE
  )
  expect_error(
    sol.df(digits = -1),
    "`digits` must be >= 0.",
    fixed = TRUE
  )

  expect_identical(.sol_chart_digits(0.0), 0L)
  expect_identical(.sol_chart_digits(5.0), 5L)
  expect_error(
    .sol_chart_digits(1.5),
    "`digits` must be integer-like.",
    fixed = TRUE
  )
  expect_error(
    .sol_chart_digits(NA_real_),
    "`digits` must be a single finite integer-like number.",
    fixed = TRUE
  )
  expect_error(
    .sol_chart_digits(Inf),
    "`digits` must be a single finite integer-like number.",
    fixed = TRUE
  )
  expect_error(
    .sol_chart_digits(-1),
    "`digits` must be >= 0.",
    fixed = TRUE
  )
})
