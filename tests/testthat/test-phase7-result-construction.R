test_that("result constructors compact shared solution and fit columns", {
  diagnostics <- data.frame(
    row_index = 1L,
    case_label = "case 1",
    status = "ok",
    change_kind = "conservative:A,parsimonious:B",
    fit_changed_types = "conservative,intermediate",
    n_fit_deltas = 2L,
    max_abs_fit_delta = 0.25,
    stringsAsFactors = FALSE
  )

  out <- .make_loo_results(diagnostics)

  expect_identical(
    names(out),
    c(
      "row_index",
      "case_label",
      "status",
      "solution_change",
      "fit_changed_types",
      "n_fit_deltas",
      "max_abs_fit_delta"
    )
  )
  expect_identical(out$solution_change, "CON:A | PAR:B")
  expect_identical(out$fit_changed_types, "CON | INT")
  expect_identical(rownames(out), as.character(seq_len(nrow(out))))
})

test_that("result constructors report missing diagnostic columns consistently", {
  diagnostics <- data.frame(
    row_index = 1L,
    case_label = "case 1",
    status = "ok",
    change_kind = NA_character_,
    n_fit_deltas = 0L,
    max_abs_fit_delta = 0,
    stringsAsFactors = FALSE
  )

  expect_error(
    .make_loo_results(diagnostics),
    "Cannot build leave-one-out `results`; missing columns: fit_changed_types",
    fixed = TRUE
  )
})

test_that("boundary results use change kind before stop reason", {
  diagnostics <- data.frame(
    direction = "lower",
    incl.cut_start = 0.8,
    incl.cut_last_safe = 0.7,
    incl.cut_first_failing = 0.6,
    number_of_steps = 2L,
    total_delta = -0.2,
    stop_reason = "search_boundary_lower",
    change_kind = "conservative:A",
    stringsAsFactors = FALSE
  )

  out <- .make_boundary_results(diagnostics, "incl.cut")

  expect_identical(
    names(out),
    c("direction", "start", "last_safe", "first_failing", "steps", "total_delta", "reason")
  )
  expect_identical(out$reason, "CON:A")
})

test_that("all-solution boundary results support wide and long layouts", {
  diagnostics <- data.frame(
    direction = rep(c("lower", "upper"), each = 2L),
    solution_type = rep(c("conservative", "parsimonious"), times = 2L),
    incl.cut_start = 0.8,
    incl.cut_last_safe = c(0.7, 0.8, 0.9, 0.85),
    incl.cut_first_failing = c(0.6, 0.7, 1, 0.9),
    number_of_steps = c(1L, 0L, 1L, 1L),
    total_delta = c(-0.1, 0, 0.1, 0.05),
    stop_reason = c("search_boundary_lower", "search_boundary_lower", "search_boundary_upper", "search_boundary_upper"),
    change_kind = c("conservative:A", NA, NA, "parsimonious:B"),
    stringsAsFactors = FALSE
  )

  wide <- .make_boundary_results(diagnostics, "incl.cut")
  long <- .make_boundary_results(diagnostics, "incl.cut", result_shape = "long")

  expect_false("reason" %in% names(wide))
  expect_true(all(c("con_reason", "par_reason") %in% names(wide)))
  expect_identical(
    names(long),
    c("solution_type", "direction", "start", "last_safe", "first_failing", "steps", "total_delta", "reason")
  )
  expect_identical(long$solution_type, c("conservative", "conservative", "parsimonious", "parsimonious"))
  expect_identical(long$direction, c("lower", "upper", "lower", "upper"))
})

test_that("all-solution calibration results support wide and long layouts", {
  diagnostics <- data.frame(
    set = rep("A", 4L),
    role = rep("condition", 4L),
    raw = rep("A_raw", 4L),
    type = rep("fuzzy", 4L),
    method = rep("direct", 4L),
    anchor = rep(c("E", "C"), each = 2L),
    direction = rep("lower", 4L),
    solution_type = rep(c("conservative", "parsimonious"), times = 2L),
    start_value = c(20, 20, 40, 40),
    last_safe_value = c(19, 20, 39, 38),
    failing_value = c(18, 19, 38, 37),
    step_unit_used = 1,
    number_of_steps = c(1L, 0L, 1L, 2L),
    total_delta_units = c(-1, 0, -1, -2),
    delta_as_pct_of_raw_range = c(-0.01, 0, -0.01, -0.02),
    stop_reason = "search_boundary_lower",
    change_kind = c("conservative:A", NA, NA, "parsimonious:B"),
    stringsAsFactors = FALSE
  )

  wide <- .make_calib_results(diagnostics)
  long <- .make_calib_results(diagnostics, result_shape = "long")

  expect_false("reason" %in% names(wide))
  expect_true(all(c("con_reason", "par_reason") %in% names(wide)))
  expect_identical(
    names(long),
    c(
      "solution_type",
      "set",
      "role",
      "raw",
      "type",
      "method",
      "anchor",
      "direction",
      "start",
      "last_safe",
      "first_failing",
      "step_unit",
      "steps",
      "total_delta",
      "pct_raw_range",
      "reason"
    )
  )
  expect_identical(long$solution_type, c("conservative", "conservative", "parsimonious", "parsimonious"))
  expect_identical(long$anchor, c("E", "C", "E", "C"))
})
