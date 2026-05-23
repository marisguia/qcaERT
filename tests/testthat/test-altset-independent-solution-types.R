test_that("altset.test keeps conservative draws comparable when all-solution exclude recomputation fails", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_direct6()

  out <- suppressWarnings(altset.test(
    raw.data = fixture$raw,
    calib.data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    calib_spec = fixture$calib_spec,
    test.conditions = fixture$test.conditions,
    anchors_to_test = c("E1", "C1", "I1"),
    unit_step = 1,
    calib_max_steps = 1,
    incl.cut = 0.75,
    incl_step = 0.1,
    incl_max_steps = 1,
    n.cut = 1,
    ncut_step = 1,
    ncut_max_steps = 1,
    n_draws = 2,
    seed = 101,
    solution = "all",
    dir.exp = fixture$dir.exp,
    exclude_recompute = list(type = 2, definitely_not_a_findRows_arg = TRUE),
    progress = FALSE
  ))

  expect_identical(out$baseline$solution_type_status[["conservative"]], "ok")
  expect_identical(out$baseline$solution_type_status[["parsimonious"]], "exclude_error")
  expect_identical(out$baseline$solution_type_status[["intermediate"]], "exclude_error")
  expect_identical(out$baseline$comparable_solutions, "conservative")

  expect_true(all(out$results$status == "ok"))
  expect_true(all(out$diagnostics$status_conservative == "ok"))
  expect_true(all(out$diagnostics$status_parsimonious == "exclude_error"))
  expect_true(all(out$diagnostics$status_intermediate == "exclude_error"))
  expect_true(all(is.na(out$diagnostics$solution_changed_parsimonious)))
  expect_true(all(is.na(out$diagnostics$solution_changed_intermediate)))
  expect_true(is.na(out$summary$score_solution_by_solution_type[["parsimonious"]]))
  expect_true(is.na(out$summary$score_solution_by_solution_type[["intermediate"]]))
})
