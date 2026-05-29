test_that("boundary robustness result schemas are frozen", {
  skip_if_not_installed("QCA")

  dat <- qcaert_schema_calib3()

  boundary_diag <- c(
    "direction",
    "solution",
    "monitored_solutions",
    "i_mode",
    "which_M",
    "incl.cut_start",
    "incl.cut_last_safe",
    "incl.cut_first_failing",
    "number_of_steps",
    "total_delta",
    "stop_reason",
    "changed_types",
    "change_kind",
    "error_source",
    "error_message",
    "n_exclude_baseline",
    "n_exclude_last_safe",
    "n_exclude_first_failing",
    "exclude_baseline",
    "exclude_last_safe",
    "exclude_first_failing"
  )
  boundary_results <- c(
    "direction",
    "start",
    "last_safe",
    "first_failing",
    "steps",
    "total_delta",
    "reason"
  )

  incl <- qcaert_expect_no_warning(incl.test(
    data = dat,
    outcome = "Y",
    conditions = c("A", "B"),
    incl.cut = 0.75,
    step = 0.1,
    max_steps = 1,
    n.cut = 1,
    solution = "conservative",
    progress = FALSE
  ))

  expect_qcaert_schema(
    incl,
    class = "incl_test",
    top = c("diagnostics", "results", "bounds", "baseline", "by_direction", "settings"),
    diagnostics = boundary_diag,
    results = boundary_results,
    settings = c(
      "outcome",
      "conditions",
      "incl.cut",
      "step",
      "max_steps",
      "n.cut",
      "solution",
      "monitored_solutions",
      "include",
      "dir.exp",
      "which_M",
      "i_mode",
      "exclude_mode",
      "exclude_recompute",
      "exclude_static",
      "result_shape"
    )
  )
  expect_identical(names(incl$by_direction), c("lower", "upper"))
  expect_identical(incl$results$direction, c("lower", "upper"))
  expect_identical(nrow(incl$diagnostics), 2L)

  ncut_diag <- boundary_diag
  ncut_diag[6:8] <- c("n.cut_start", "n.cut_last_safe", "n.cut_first_failing")
  ncut_diag <- c(ncut_diag, "upper_limit")

  ncut <- qcaert_expect_no_warning(ncut.test(
    data = dat,
    outcome = "Y",
    conditions = c("A", "B"),
    n.cut = 1,
    step = 1,
    max_steps = 1,
    incl.cut = 0.75,
    solution = "conservative",
    progress = FALSE
  ))

  expect_qcaert_schema(
    ncut,
    class = "ncut_test",
    top = c("diagnostics", "results", "bounds", "baseline", "by_direction", "settings"),
    diagnostics = ncut_diag,
    results = boundary_results,
    settings = c(
      "outcome",
      "conditions",
      "n.cut",
      "step",
      "max_steps",
      "incl.cut",
      "solution",
      "monitored_solutions",
      "include",
      "dir.exp",
      "which_M",
      "i_mode",
      "exclude_mode",
      "exclude_recompute",
      "exclude_static",
      "result_shape",
      "upper_limit"
    )
  )
  expect_identical(names(ncut$by_direction), c("lower", "upper"))
  expect_identical(ncut$results$direction, c("lower", "upper"))
  expect_identical(nrow(ncut$diagnostics), 2L)
})

test_that("calibration robustness result schemas are frozen", {
  skip_if_not_installed("QCA")

  raw <- qcaert_schema_raw()
  calib6 <- qcaert_schema_calib6(raw)
  spec6 <- qcaert_schema_calib6_spec()

  calib_diag <- c(
    "set",
    "role",
    "raw",
    "type",
    "method",
    "anchor",
    "direction",
    "solution",
    "monitored_solutions",
    "which_M_tested",
    "n_M_conservative_baseline",
    "n_M_parsimonious_baseline",
    "n_M_intermediate_min_baseline",
    "step_unit_used",
    "start_value",
    "last_safe_value",
    "failing_value",
    "number_of_steps",
    "total_delta_units",
    "delta_as_pct_of_raw_range",
    "stop_reason",
    "changed_types",
    "change_kind",
    "error_source",
    "error_message"
  )
  calib_results <- c(
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
  calib_settings <- c(
    "outcome",
    "conditions",
    "test.conditions",
    "test.outcome",
    "calib_spec",
    "anchors_to_test",
    "solution",
    "monitored_solutions",
    "include",
    "which_M",
    "unit_step",
    "unit_step_divisor",
    "max_steps",
    "incl.cut",
    "n.cut",
    "dir.exp",
    "i_mode",
    "exclude_mode",
    "exclude_recompute",
    "exclude_static",
    "result_shape"
  )

  calib <- qcaert_expect_no_warning(calib.test(
    raw.data = raw,
    calib.data = calib6,
    outcome = "Y",
    conditions = c("A", "B"),
    calib_spec = spec6,
    test.conditions = "A",
    unit_step = 1,
    max_steps = 1,
    incl.cut = 0.75,
    n.cut = 1,
    solution = "conservative",
    progress = FALSE
  ))

  expect_qcaert_schema(
    calib,
    class = "calib_test",
    top = c("diagnostics", "results", "bounds", "baseline", "by_set", "settings"),
    diagnostics = calib_diag,
    results = calib_results,
    settings = calib_settings
  )
  expect_identical(unique(calib$results$anchor), c("E1", "C1", "I1", "I2", "C2", "E2"))
  expect_identical(unique(calib$results$role), "condition")
  expect_identical(unique(calib$results$method), "direct")
  expect_identical(calib$baseline$status, "ok")
  expect_identical(names(calib$baseline$by_solution_type), "conservative")
  expect_identical(nrow(calib$results), 12L)

  # QCA's indirect calibration warns on this tiny fixture; keep it explicit.
  indirect <- NULL
  expect_warning(indirect <- calib.test(
    raw.data = raw,
    calib.data = qcaert_schema_calib3(raw),
    outcome = "Y",
    conditions = c("A", "B"),
    calib_spec = qcaert_schema_indirect_spec(),
    test.conditions = "A",
    unit_step = 1,
    max_steps = 1,
    incl.cut = 0.75,
    n.cut = 1,
    solution = "conservative",
    progress = FALSE
  ), "glm.fit: algorithm did not converge")

  expect_identical(unique(indirect$results$anchor), c("T1", "T2", "T3"))
  expect_identical(unique(indirect$results$method), "indirect")
  expect_identical(nrow(indirect$results), 6L)
})

test_that("alternative-set robustness result schemas are frozen", {
  skip_if_not_installed("QCA")

  raw <- qcaert_schema_raw()
  calib6 <- qcaert_schema_calib6(raw)

  alt <- qcaert_expect_no_warning(altset.test(
    raw.data = raw,
    calib.data = calib6,
    outcome = "Y",
    conditions = c("A", "B"),
    calib_spec = qcaert_schema_calib6_spec(),
    test.conditions = "A",
    unit_step = 1,
    calib_max_steps = 1,
    incl.cut = 0.75,
    incl_step = 0.1,
    incl_max_steps = 1,
    n.cut = 1,
    ncut_step = 1,
    ncut_max_steps = 1,
    n_draws = 2,
    seed = 1,
    solution = "conservative",
    progress = FALSE
  ))

  expect_qcaert_schema(
    alt,
    class = "altset_test",
    top = c("diagnostics", "results", "summary", "baseline", "by_draw", "settings"),
    diagnostics = c(
      "draw",
      "incl.cut",
      "n.cut",
      "status",
      "changed",
      "solution_changed",
      "changed_types",
      "change_kind",
      "fit_compared",
      "fit_changed",
      "fit_changed_types",
      "n_fit_deltas",
      "max_abs_fit_delta",
      "error_source",
      "error_message",
      "changed_sets",
      "changed_roles",
      "n_changed_sets",
      "exclude_used",
      "calibration",
      "status_conservative",
      "error_source_conservative",
      "error_message_conservative",
      "solution_changed_conservative",
      "fit_compared_conservative",
      "fit_changed_conservative",
      "max_abs_fit_delta_conservative",
      "matched_fit_keys_conservative"
    ),
    results = c(
      "draw",
      "incl.cut",
      "n.cut",
      "status",
      "n_changed_sets",
      "changed_sets",
      "changed_roles",
      "solution_change",
      "fit_changed_types",
      "n_fit_deltas",
      "max_abs_fit_delta"
    ),
    settings = c(
      "outcome",
      "conditions",
      "test.conditions",
      "test.outcome",
      "calib_spec",
      "anchors_to_test",
      "solution",
      "monitored_solutions",
      "include",
      "which_M",
      "unit_step",
      "unit_step_divisor",
      "calib_max_steps",
      "incl.cut",
      "incl_step",
      "incl_max_steps",
      "n.cut",
      "ncut_step",
      "ncut_max_steps",
      "dir.exp",
      "i_mode",
      "exclude_mode",
      "exclude_recompute",
      "exclude_static",
      "n_draws",
      "fit_tol",
      "seed"
    )
  )
  expect_identical(nrow(alt$diagnostics), 2L)
  expect_identical(names(alt$by_draw), NULL)
  expect_identical(names(alt$baseline$draw_meta$calibration$A$delta_steps), c("E1", "C1", "I1", "I2", "C2", "E2"))
  expect_identical(names(alt$by_draw[[1]]$result$draw_meta$calibration$A$delta_steps), c("E1", "C1", "I1", "I2", "C2", "E2"))
})

test_that("case-deletion and subsample result schemas are frozen", {
  skip_if_not_installed("QCA")

  dat <- qcaert_schema_calib3()
  case_diag <- c(
    "row_index",
    "case_label",
    "status",
    "changed",
    "solution_changed",
    "changed_types",
    "change_kind",
    "fit_changed",
    "fit_changed_types",
    "n_fit_deltas",
    "max_abs_fit_delta",
    "error_source",
    "error_message"
  )
  case_results <- c(
    "row_index",
    "case_label",
    "status",
    "solution_change",
    "fit_changed_types",
    "n_fit_deltas",
    "max_abs_fit_delta"
  )

  loo <- qcaert_expect_no_warning(loo.test(
    data = dat,
    outcome = "Y",
    conditions = c("A", "B"),
    cases = 1:3,
    incl.cut = 0.75,
    n.cut = 1,
    solution = "conservative",
    progress = FALSE
  ))

  expect_qcaert_schema(
    loo,
    class = "loo_test",
    top = c("diagnostics", "results", "baseline", "by_case", "settings"),
    diagnostics = case_diag,
    results = case_results,
    settings = c(
      "outcome",
      "conditions",
      "cases",
      "case_labels",
      "calib",
      "n_cases_tested",
      "incl.cut",
      "n.cut",
      "solution",
      "monitored_solutions",
      "include",
      "dir.exp",
      "which_M",
      "i_mode",
      "exclude_mode",
      "exclude_recompute",
      "exclude_static",
      "fit_measures",
      "fit_tol"
    )
  )
  expect_identical(nrow(loo$results), 3L)
  expect_identical(names(loo$by_case), c("1:1", "2:2", "3:3"))

  subsample <- qcaert_expect_no_warning(subsample.test(
    data = dat,
    outcome = "Y",
    conditions = c("A", "B"),
    sample_n = 8,
    reps = 2,
    seed = 1,
    incl.cut = 0.75,
    n.cut = 1,
    solution = "conservative",
    progress = FALSE
  ))

  expect_qcaert_schema(
    subsample,
    class = "subsample_test",
    top = c("diagnostics", "results", "summary", "baseline", "by_run", "settings"),
    diagnostics = c(
      "rep",
      "status",
      "n_sample",
      "n_holdout",
      "changed",
      "solution_changed",
      "changed_types",
      "change_kind",
      "fit_changed",
      "fit_changed_types",
      "n_fit_deltas",
      "max_abs_fit_delta",
      "exact_match_baseline",
      "term_jaccard_baseline",
      "error_source",
      "error_message"
    ),
    results = c(
      "rep",
      "n_sample",
      "n_holdout",
      "status",
      "exact_match_baseline",
      "term_jaccard_baseline",
      "solution_change",
      "fit_changed_types",
      "n_fit_deltas",
      "max_abs_fit_delta"
    ),
    settings = c(
      "outcome",
      "conditions",
      "calib",
      "sample_n",
      "sample_prop_requested",
      "sample_prop_realized",
      "reps",
      "stratify",
      "incl.cut",
      "n.cut",
      "solution",
      "monitored_solutions",
      "include",
      "dir.exp",
      "which_M",
      "i_mode",
      "exclude_mode",
      "exclude_recompute",
      "exclude_static",
      "fit_measures",
      "fit_tol",
      "seed"
    )
  )
  expect_identical(nrow(subsample$results), 2L)
  expect_identical(names(subsample$by_run), c("R1", "R2"))
  expect_setequal(
    names(subsample$summary),
    c("exact_solution", "term_stability", "fit_stability", "similarity", "calibration")
  )
})

test_that("cluster and sol.df result schemas are frozen", {
  skip_if_not_installed("QCA")

  raw <- qcaert_schema_raw()
  dat <- qcaert_schema_calib6(raw)
  tt <- qcaert_expect_no_warning(QCA::truthTable(
    dat,
    outcome = "Y",
    conditions = c("A", "B"),
    incl.cut = 0.75,
    n.cut = 1
  ))

  cluster <- qcaert_expect_no_warning(cluster.test(
    data = dat,
    tt = tt,
    cluster_id = "cluster",
    unit_id = "unit",
    solution = "conservative",
    progress = FALSE
  ))

  expect_qcaert_schema(
    cluster,
    class = "cluster_test",
    top = c("diagnostics", "results", "baseline", "by_cluster", "by_unit", "settings"),
    diagnostics = c(
      "solution_type",
      "i_mode",
      "which_M",
      "necessity",
      "configuration_key",
      "component_count",
      "status",
      "error_source",
      "error_message",
      "pooled_consistency",
      "pooled_coverage",
      "n_clusters",
      "max_abs_delta_consistency",
      "max_abs_delta_coverage",
      "mean_abs_delta_consistency",
      "mean_abs_delta_coverage",
      "worst_cluster_consistency_id",
      "worst_cluster_coverage_id",
      "worst_cluster_consistency",
      "worst_cluster_coverage",
      "within_available",
      "n_units_repeated",
      "max_abs_within_delta_consistency",
      "max_abs_within_delta_coverage",
      "mean_abs_within_delta_consistency",
      "mean_abs_within_delta_coverage"
    ),
    results = c("overview", "clusters", "units"),
    settings = c(
      "outcome",
      "conditions",
      "cluster_id",
      "unit_id",
      "solution",
      "monitored_solutions",
      "include",
      "dir.exp",
      "exclude",
      "which_M",
      "i_mode",
      "necessity",
      "progress"
    )
  )
  expect_identical(names(as.data.frame(cluster)), names(cluster$results$overview))
  expect_identical(names(cluster$results), c("overview", "clusters", "units"))

  sol <- qcaert_expect_no_warning(QCA::minimize(tt, include = ""))
  sol_table <- sol.df(conservative = sol, solution = "conservative")

  expect_identical(class(sol_table), "data.frame")
  expect_identical(
    names(sol_table),
    c(
      "Solution",
      "Model",
      "Intermediate_CnPn",
      "Prime_Implicants",
      "Consistency_PI",
      "PRI_PI",
      "Raw_Coverage_PI",
      "Unique_Coverage_PI",
      "Solution_Consistency",
      "Solution_PRI",
      "Solution_Coverage",
      "Cases"
    )
  )
  expect_true(nrow(sol_table) >= 1L)
})
