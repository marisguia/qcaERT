test_that("loo.test keeps conservative comparable when all-solution exclude recomputation fails", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_direct6()

  out <- qcaert_expect_no_warning(loo.test(
    data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    cases = 1,
    incl.cut = 0.75,
    n.cut = 1,
    solution = "all",
    dir.exp = fixture$dir.exp,
    exclude_recompute = list(type = 2, definitely_not_a_findRows_arg = TRUE),
    fit_measures = NULL,
    progress = FALSE
  ))

  expect_identical(out$baseline$status, "ok")
  expect_identical(out$baseline$solution_type_status[["conservative"]], "ok")
  expect_identical(out$baseline$solution_type_status[["parsimonious"]], "exclude_error")
  expect_identical(out$baseline$solution_type_status[["intermediate"]], "exclude_error")
  expect_identical(out$results$status, "ok")
})

test_that("subsample.test keeps conservative comparable when all-solution exclude recomputation fails", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_direct6()

  out <- qcaert_expect_no_warning(subsample.test(
    data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    sample_n = 8,
    reps = 1,
    seed = 202,
    incl.cut = 0.75,
    n.cut = 1,
    solution = "all",
    dir.exp = fixture$dir.exp,
    exclude_recompute = list(type = 2, definitely_not_a_findRows_arg = TRUE),
    fit_measures = NULL,
    progress = FALSE
  ))

  expect_identical(out$baseline$status, "ok")
  expect_identical(out$baseline$solution_type_status[["conservative"]], "ok")
  expect_identical(out$baseline$solution_type_status[["parsimonious"]], "exclude_error")
  expect_identical(out$baseline$solution_type_status[["intermediate"]], "exclude_error")
  expect_identical(out$by_run[[1L]]$reduced$status, "ok")
  expect_identical(out$by_run[[1L]]$reduced$solution_type_status[["conservative"]], "ok")
  expect_identical(out$by_run[[1L]]$reduced$solution_type_status[["parsimonious"]], "exclude_error")
  expect_identical(out$by_run[[1L]]$reduced$solution_type_status[["intermediate"]], "exclude_error")
  expect_identical(out$results$status, "ok")
})

test_that("baseline solution_type selection keeps valid solution_types when another baseline solution_type is unusable", {
  baseline <- list(
    status = "ok",
    selected_solution_missing = c(conservative = FALSE, parsimonious = TRUE, intermediate = FALSE),
    fit_missing = c(conservative = FALSE, parsimonious = FALSE, intermediate = TRUE),
    solution_type_status = c(conservative = "ok", parsimonious = "ok", intermediate = "ok")
  )

  monitored <- c("conservative", "parsimonious", "intermediate")

  expect_identical(
    .reduced_baseline_solution_types(baseline, monitored),
    c("conservative", "intermediate")
  )
  expect_identical(
    .reduced_baseline_solution_types(
      baseline,
      monitored,
      fit_measures = "inclS",
      require_fit = TRUE
    ),
    "conservative"
  )
})
