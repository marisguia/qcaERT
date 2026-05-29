test_that("stress: LR QCA solution workflow feeds sol.df", {
  skip_qcaert_stress()
  skip_if_not_installed("QCA")

  lr <- qcaert_fixture_lr_solutions()
  out <- sol.df(
    conservative = lr$conservative,
    intermediate = lr$intermediate,
    parsimonious = lr$parsimonious,
    solution = "all",
    which_M = 1,
    include_cases = TRUE,
    digits = 3
  )

  qcaert_expect_table(
    out,
    "LR sol.df output",
    required = c(
      "Solution",
      "Model",
      "Prime_Implicants",
      "Solution_Consistency",
      "Solution_Coverage",
      "Cases"
    )
  )
  expect_true(nrow(out) >= 3L)
  expect_setequal(unique(out$Solution), c("Conservative", "Intermediate", "Parsimonious"))
  expect_false(any(is.na(out$Prime_Implicants)))
})

test_that("stress: LR boundary and leave-one-out workflows preserve the family structure", {
  skip_qcaert_stress()
  skip_if_not_installed("QCA")

  lr <- qcaert_fixture_lr()

  incl <- qcaert_expect_no_warning(incl.test(
    data = lr$calib,
    outcome = lr$outcome,
    conditions = lr$conditions,
    incl.cut = 0.8,
    step = 0.05,
    max_steps = 2,
    n.cut = 1,
    solution = "all",
    dir.exp = lr$dir.exp,
    progress = FALSE
  ))
  expect_qcaert_result_structure(
    incl,
    class = "incl_test",
    diagnostics = c("direction", "solution_type", "monitored_solutions", "change_kind"),
    results = c("direction", "start", "con_last_safe", "par_last_safe"),
    settings = c("outcome", "conditions", "solution", "monitored_solutions"),
    top = c("bounds", "baseline", "by_direction", "settings")
  )

  ncut <- qcaert_expect_no_warning(ncut.test(
    data = lr$calib,
    outcome = lr$outcome,
    conditions = lr$conditions,
    n.cut = 1,
    step = 1,
    max_steps = 2,
    incl.cut = 0.8,
    solution = "all",
    dir.exp = lr$dir.exp,
    progress = FALSE
  ))
  expect_qcaert_result_structure(
    ncut,
    class = "ncut_test",
    diagnostics = c("direction", "solution_type", "monitored_solutions", "change_kind", "upper_limit"),
    results = c("direction", "start", "con_last_safe", "par_last_safe"),
    settings = c("outcome", "conditions", "solution", "monitored_solutions"),
    top = c("bounds", "baseline", "by_direction", "settings")
  )

  loo <- qcaert_expect_no_warning(loo.test(
    data = lr$calib,
    outcome = lr$outcome,
    conditions = lr$conditions,
    cases = 1:6,
    incl.cut = 0.8,
    n.cut = 1,
    solution = "all",
    dir.exp = lr$dir.exp,
    progress = FALSE
  ))
  expect_qcaert_result_structure(
    loo,
    class = "loo_test",
    diagnostics = c("row_index", "case_label", "solution_changed", "fit_changed_types"),
    results = c("row_index", "case_label", "solution_change", "fit_changed_types"),
    settings = c("outcome", "conditions", "cases", "monitored_solutions"),
    top = c("baseline", "by_case", "settings")
  )
  expect_identical(nrow(loo$results), 6L)
})

test_that("stress: calibration workflows cover direct-six and indirect anchors", {
  skip_qcaert_stress()
  skip_if_not_installed("QCA")

  direct <- qcaert_fixture_direct6()
  direct_out <- qcaert_expect_no_warning(calib.test(
    raw.data = direct$raw,
    calib.data = direct$calib,
    outcome = direct$outcome,
    conditions = direct$conditions,
    calib_spec = direct$calib_spec,
    test.conditions = direct$test.conditions,
    unit_step = 1,
    max_steps = 2,
    incl.cut = 0.75,
    n.cut = 1,
    solution = "all",
    dir.exp = direct$dir.exp,
    progress = FALSE
  ))
  expect_qcaert_result_structure(
    direct_out,
    class = "calib_test",
    diagnostics = c("set", "role", "method", "anchor", "direction", "monitored_solutions"),
    results = c("set", "role", "method", "anchor", "direction", "con_reason"),
    settings = c("outcome", "conditions", "calib_spec", "monitored_solutions", "test.outcome"),
    top = c("bounds", "baseline", "by_set", "settings")
  )
  expect_identical(unique(direct_out$results$anchor), c("E1", "C1", "I1", "I2", "C2", "E2"))

  indirect <- qcaert_fixture_indirect()
  # QCA's indirect calibration warns on this tiny fixture; keep it explicit.
  indirect_out <- qcaert_expect_warnings(calib.test(
    raw.data = indirect$raw,
    calib.data = indirect$calib,
    outcome = indirect$outcome,
    conditions = indirect$conditions,
    calib_spec = indirect$calib_spec,
    test.conditions = indirect$test.conditions,
    unit_step = 1,
    max_steps = 2,
    incl.cut = 0.75,
    n.cut = 1,
    solution = "all",
    dir.exp = indirect$dir.exp,
    progress = FALSE
  ), "glm.fit: algorithm did not converge")
  expect_qcaert_result_structure(
    indirect_out,
    class = "calib_test",
    diagnostics = c("set", "role", "method", "anchor", "direction", "monitored_solutions"),
    results = c("set", "role", "method", "anchor", "direction", "con_reason"),
    settings = c("outcome", "conditions", "calib_spec", "monitored_solutions", "test.outcome"),
    top = c("bounds", "baseline", "by_set", "settings")
  )
  expect_identical(unique(indirect_out$results$anchor), c("T1", "T2", "T3"))
})

test_that("stress: randomized workflows are reproducible with fixed seeds", {
  skip_qcaert_stress()
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_direct6()

  alt_one <- qcaert_expect_no_warning(altset.test(
    raw.data = fixture$raw,
    calib.data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    calib_spec = fixture$calib_spec,
    test.conditions = fixture$test.conditions,
    unit_step = 1,
    calib_max_steps = 2,
    incl.cut = 0.75,
    incl_step = 0.05,
    incl_max_steps = 2,
    n.cut = 1,
    ncut_step = 1,
    ncut_max_steps = 2,
    n_draws = 5,
    seed = 303,
    solution = "all",
    dir.exp = fixture$dir.exp,
    progress = FALSE
  ))
  alt_two <- qcaert_expect_no_warning(altset.test(
    raw.data = fixture$raw,
    calib.data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    calib_spec = fixture$calib_spec,
    test.conditions = fixture$test.conditions,
    unit_step = 1,
    calib_max_steps = 2,
    incl.cut = 0.75,
    incl_step = 0.05,
    incl_max_steps = 2,
    n.cut = 1,
    ncut_step = 1,
    ncut_max_steps = 2,
    n_draws = 5,
    seed = 303,
    solution = "all",
    dir.exp = fixture$dir.exp,
    progress = FALSE
  ))
  expect_qcaert_result_structure(
    alt_one,
    class = "altset_test",
    diagnostics = c("draw", "status", "changed_sets", "changed_roles", "fit_changed_types"),
    results = c("draw", "status", "changed_sets", "changed_roles", "fit_changed_types"),
    settings = c("outcome", "conditions", "test.outcome", "n_draws", "seed"),
    top = c("summary", "baseline", "by_draw", "settings")
  )
  expect_equal(alt_one$results, alt_two$results)
  expect_equal(alt_one$diagnostics, alt_two$diagnostics)

  subsample_one <- qcaert_expect_no_warning(subsample.test(
    data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    sample_prop = 0.8,
    reps = 5,
    stratify = "outcome",
    seed = 404,
    incl.cut = 0.75,
    n.cut = 1,
    solution = "all",
    dir.exp = fixture$dir.exp,
    progress = FALSE
  ))
  subsample_two <- qcaert_expect_no_warning(subsample.test(
    data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    sample_prop = 0.8,
    reps = 5,
    stratify = "outcome",
    seed = 404,
    incl.cut = 0.75,
    n.cut = 1,
    solution = "all",
    dir.exp = fixture$dir.exp,
    progress = FALSE
  ))
  expect_qcaert_result_structure(
    subsample_one,
    class = "subsample_test",
    diagnostics = c("rep", "status", "n_sample", "n_holdout", "fit_changed_types"),
    results = c("rep", "n_sample", "n_holdout", "status", "fit_changed_types"),
    settings = c("outcome", "conditions", "sample_prop_requested", "stratify", "seed"),
    top = c("summary", "baseline", "by_run", "settings")
  )
  expect_equal(subsample_one$results, subsample_two$results)
  expect_equal(subsample_one$diagnostics, subsample_two$diagnostics)
})

test_that("stress: cluster diagnostics handle all solution types with repeated units", {
  skip_qcaert_stress()
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_cluster()
  out <- qcaert_expect_no_warning(cluster.test(
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
    diagnostics = c("solution_type", "configuration_key", "status", "within_available", "n_units_repeated"),
    overview = c("solution_type", "configuration", "status", "within_available", "n_units_repeated"),
    clusters = c("solution_type", "configuration", "component", "cluster_id"),
    units = c("solution_type", "configuration", "component", "unit_id"),
    settings = c("outcome", "conditions", "solution", "monitored_solutions"),
    top = c("baseline", "by_cluster", "by_unit", "settings")
  )
  expect_setequal(out$settings$monitored_solutions, c("conservative", "parsimonious", "intermediate"))
  expect_true(any(out$results$overview$within_available))
})
