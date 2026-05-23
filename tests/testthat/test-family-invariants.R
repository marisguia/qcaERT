test_that("regular invariant helper validates the common returned-object structure", {
  skip_if_not_installed("QCA")

  dat <- qcaert_schema_calib3()
  out <- suppressWarnings(incl.test(
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

  expect_qcaert_result_structure(
    out,
    class = "incl_test",
    diagnostics = c("direction", "solution", "stop_reason", "change_kind"),
    results = c("direction", "start", "last_safe", "reason"),
    settings = c("outcome", "conditions", "solution", "exclude_recompute", "exclude_static"),
    top = c("bounds", "baseline", "by_direction", "settings")
  )
})

test_that("cluster invariant helper validates the explicit cluster exception", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_cluster()
  out <- suppressWarnings(cluster.test(
    data = fixture$data,
    tt = fixture$truth_table,
    cluster_id = fixture$cluster_id,
    unit_id = fixture$unit_id,
    solution = "conservative",
    progress = FALSE
  ))

  expect_qcaert_cluster_structure(
    out,
    diagnostics = c("solution_type", "configuration_key", "status", "within_available"),
    overview = c("solution_type", "configuration", "status", "within_available"),
    clusters = c("solution_type", "configuration", "component", "cluster_id"),
    units = c("solution_type", "configuration", "component", "unit_id"),
    settings = c("outcome", "conditions", "solution", "cluster_id", "unit_id"),
    top = c("baseline", "by_cluster", "by_unit", "settings")
  )
})

test_that("exclusion-control formals use the common recompute/static conventions", {
  shared <- c(
    "incl.test",
    "ncut.test",
    "calib.test",
    "loo.test",
    "subsample.test",
    "altset.test",
    "theory.test"
  )

  for (fn in shared) {
    args <- names(formals(get(fn, envir = asNamespace("qcaERT"))))
    expect_true(all(c("exclude_mode", "exclude_recompute", "exclude_static") %in% args), info = fn)
    expect_false("exclude" %in% args, info = fn)
    expect_false("exclude_spec" %in% args, info = fn)
  }

  cluster_args <- names(formals(cluster.test))
  expect_true("exclude" %in% cluster_args)
  expect_false("exclude_mode" %in% cluster_args)
  expect_false("exclude_recompute" %in% cluster_args)
  expect_false("exclude_static" %in% cluster_args)
})

test_that("shared exclusion-control validator rejects crossed wires", {
  expect_error(
    .validate_exclusion_controls(
      exclude_mode = "none",
      exclude_recompute = list(type = 2),
      exclude_static = NULL,
      exclude_recompute_supplied = TRUE,
      monitored_solutions = "parsimonious"
    ),
    "cannot be combined",
    fixed = TRUE
  )

  expect_error(
    .validate_exclusion_controls(
      exclude_mode = "static",
      exclude_recompute = list(type = 2),
      exclude_static = 1:2,
      exclude_recompute_supplied = TRUE,
      monitored_solutions = "parsimonious"
    ),
    "uses `exclude_static`, not `exclude_recompute`",
    fixed = TRUE
  )

  expect_error(
    .validate_exclusion_controls(
      exclude_mode = "recompute",
      exclude_recompute = list(type = 2),
      exclude_static = 1:2,
      monitored_solutions = "parsimonious"
    ),
    "uses `exclude_recompute`, not `exclude_static`",
    fixed = TRUE
  )
})
