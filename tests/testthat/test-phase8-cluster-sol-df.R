test_that("QCA solution helpers follow data-frame pims", {
  pims <- data.frame(
    "A * B" = c(1, 0.4, 0.8),
    check.names = FALSE,
    row.names = c("case1", "case2", "case3")
  )

  out <- .qca_pims_by_terms(pims, "A*B")
  expect_identical(names(out), "A*B")
  expect_equal(out[["A*B"]], c(1, 0.4, 0.8))

  expect_null(.qca_pims_by_terms(as.matrix(pims), "A*B"))
})

test_that("sol.df case reconstruction uses QCA-style pims data frames", {
  incl_cov <- data.frame(
    inclS = 0.9,
    PRI = 0.8,
    covS = 0.7,
    covU = 0.6,
    row.names = "A*B"
  )
  pims <- data.frame(
    "A*B" = c(1, 0.4, 0.8),
    check.names = FALSE,
    row.names = c("case1", "case2", "case3")
  )

  expect_identical(
    unname(.qca_cases_from_incl_cov(incl_cov, pims, include_cases = TRUE)),
    "case1, case3"
  )
})

test_that("QCA minimize provides data-frame pims for solution consumers", {
  skip_if_not_installed("QCA")

  dat <- qcaert_schema_calib3()
  tt <- suppressWarnings(QCA::truthTable(
    dat,
    outcome = "Y",
    conditions = c("A", "B"),
    incl.cut = 0.75,
    n.cut = 1
  ))
  sol <- suppressWarnings(QCA::minimize(tt, include = ""))

  expect_true(is.data.frame(sol$pims))

  sol_table <- sol.df(conservative = sol, solution = "conservative")
  expect_true("Cases" %in% names(sol_table))
  expect_true(nrow(sol_table) >= 1L)
})

test_that("cluster.test does not duplicate one-term solutions as term components", {
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

  single_configurations <- out$results$overview$configuration[out$results$overview$components == 1L]
  clusters <- out$results$clusters[out$results$clusters$configuration %in% single_configurations, , drop = FALSE]

  expect_true(nrow(clusters) > 0L)
  expect_identical(unique(clusters$component), "solution")
  expect_false(any(clusters$component == clusters$solution_expression))

  if (!is.null(out$results$units)) {
    units <- out$results$units[out$results$units$configuration %in% single_configurations, , drop = FALSE]
    expect_identical(unique(units$component), "solution")
    expect_false(any(units$component == units$solution_expression))
  }
})
