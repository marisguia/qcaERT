test_that("incl.test runs end to end on the shared direct-six fixture", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_direct6()
  out <- suppressWarnings(incl.test(
    data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    incl.cut = 0.75,
    step = 0.1,
    max_steps = 1,
    n.cut = 1,
    solution = "all",
    dir.exp = fixture$dir.exp,
    progress = FALSE
  ))

  expect_qcaert_result_structure(
    out,
    class = "incl_test",
    diagnostics = c("direction", "solution_type", "monitored_solutions", "change_kind", "stop_reason"),
    results = c("direction", "start", "con_last_safe", "par_last_safe", "int_last_safe"),
    settings = c("outcome", "conditions", "solution", "monitored_solutions"),
    top = c("bounds", "baseline", "by_direction", "settings")
  )
  expect_identical(out$results$direction, c("lower", "upper"))
  expect_setequal(
    out$settings$monitored_solutions,
    c("conservative", "parsimonious", "intermediate")
  )
})

test_that("ncut.test runs end to end on the shared direct-six fixture", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_direct6()
  out <- suppressWarnings(ncut.test(
    data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    n.cut = 1,
    step = 1,
    max_steps = 1,
    incl.cut = 0.75,
    solution = "all",
    dir.exp = fixture$dir.exp,
    progress = FALSE
  ))

  expect_qcaert_result_structure(
    out,
    class = "ncut_test",
    diagnostics = c("direction", "solution_type", "monitored_solutions", "change_kind", "stop_reason", "upper_limit"),
    results = c("direction", "start", "con_last_safe", "par_last_safe", "int_last_safe"),
    settings = c("outcome", "conditions", "solution", "monitored_solutions", "upper_limit"),
    top = c("bounds", "baseline", "by_direction", "settings")
  )
  expect_identical(out$results$direction, c("lower", "upper"))
  expect_true(out$settings$upper_limit >= out$settings$n.cut)
})

test_that("calib.test runs end to end for indirect calibration", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_indirect()
  out <- suppressWarnings(calib.test(
    raw.data = fixture$raw,
    calib.data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    calib_spec = fixture$calib_spec,
    test.conditions = fixture$test.conditions,
    unit_step = 1,
    max_steps = 1,
    incl.cut = 0.75,
    n.cut = 1,
    solution = "conservative",
    progress = FALSE
  ))

  expect_qcaert_result_structure(
    out,
    class = "calib_test",
    diagnostics = c("set", "role", "method", "anchor", "direction", "change_kind"),
    results = c("set", "role", "method", "anchor", "direction", "start", "reason"),
    settings = c("outcome", "conditions", "calib_spec", "solution", "test.outcome"),
    top = c("bounds", "baseline", "by_set", "settings")
  )
  expect_identical(unique(out$results$method), "indirect")
  expect_identical(unique(out$results$anchor), c("T1", "T2", "T3"))
})

test_that("altset.test runs end to end with deterministic draws", {
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
    solution = "conservative",
    progress = FALSE
  ))

  expect_qcaert_result_structure(
    out,
    class = "altset_test",
    diagnostics = c("draw", "status", "changed_sets", "changed_roles", "fit_changed_types"),
    results = c("draw", "status", "changed_sets", "changed_roles", "fit_changed_types"),
    settings = c("outcome", "conditions", "test.outcome", "n_draws", "seed", "solution"),
    top = c("summary", "baseline", "by_draw", "settings")
  )
  expect_identical(nrow(out$results), 2L)
  expect_identical(length(out$by_draw), 2L)
  expect_equal(out$settings$seed, 101)
})

test_that("loo.test runs end to end on selected cases", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_direct6()
  out <- suppressWarnings(loo.test(
    data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    cases = 1:3,
    incl.cut = 0.75,
    n.cut = 1,
    solution = "all",
    dir.exp = fixture$dir.exp,
    progress = FALSE
  ))

  expect_qcaert_result_structure(
    out,
    class = "loo_test",
    diagnostics = c("row_index", "case_label", "solution_changed", "fit_changed_types"),
    results = c("row_index", "case_label", "solution_change", "fit_changed_types"),
    settings = c("outcome", "conditions", "cases", "monitored_solutions"),
    top = c("baseline", "by_case", "settings")
  )
  expect_identical(nrow(out$results), 3L)
  expect_identical(names(out$by_case), c("1:1", "2:2", "3:3"))
})

test_that("subsample.test runs end to end with deterministic fixed calibration", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_direct6()
  out <- suppressWarnings(subsample.test(
    data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    sample_n = 8,
    reps = 2,
    seed = 202,
    incl.cut = 0.75,
    n.cut = 1,
    solution = "conservative",
    progress = FALSE
  ))

  expect_qcaert_result_structure(
    out,
    class = "subsample_test",
    diagnostics = c("rep", "status", "n_sample", "n_holdout", "fit_changed_types"),
    results = c("rep", "n_sample", "n_holdout", "status", "fit_changed_types"),
    settings = c("outcome", "conditions", "sample_n", "reps", "seed"),
    top = c("summary", "baseline", "by_run", "settings")
  )
  expect_identical(nrow(out$results), 2L)
  expect_identical(names(out$by_run), c("R1", "R2"))
  expect_identical(out$settings$seed, 202L)
})

test_that("cluster.test runs end to end on repeated-unit cluster data", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_cluster()
  out <- suppressWarnings(cluster.test(
    data = fixture$data,
    tt = fixture$truth_table,
    cluster_id = fixture$cluster_id,
    unit_id = fixture$unit_id,
    solution = "all",
    dir.exp = fixture$dir.exp,
    progress = FALSE
  ))

  expect_qcaert_cluster_structure(
    out,
    diagnostics = c("solution_type", "configuration_key", "status", "within_available"),
    overview = c("solution_type", "configuration", "status", "within_available"),
    clusters = c("solution_type", "configuration", "component", "cluster_id"),
    units = c("solution_type", "configuration", "component", "unit_id"),
    settings = c("outcome", "conditions", "solution", "monitored_solutions"),
    top = c("baseline", "by_cluster", "by_unit", "settings")
  )
  expect_setequal(
    out$settings$monitored_solutions,
    c("conservative", "parsimonious", "intermediate")
  )
})

test_that("sol.df extracts a clean table from QCA minimization objects", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_direct6()
  tt <- qcaert_truth_table(
    data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    incl.cut = 0.75,
    n.cut = 1
  )
  conservative <- suppressWarnings(QCA::minimize(tt, include = ""))
  intermediate <- suppressWarnings(QCA::minimize(
    tt,
    include = "?",
    dir.exp = fixture$dir.exp
  ))
  parsimonious <- suppressWarnings(QCA::minimize(tt, include = "?"))

  out <- sol.df(
    conservative = conservative,
    intermediate = intermediate,
    parsimonious = parsimonious,
    solution = "all",
    which_M = 1,
    include_cases = TRUE,
    digits = 3
  )

  qcaert_expect_table(
    out,
    "sol.df output",
    required = c(
      "Solution",
      "Model",
      "Prime_Implicants",
      "Solution_Consistency",
      "Solution_Coverage",
      "Cases"
    )
  )
  expect_true(nrow(out) >= 1L)
  expect_setequal(unique(out$Solution), c("Conservative", "Intermediate", "Parsimonious"))
})
