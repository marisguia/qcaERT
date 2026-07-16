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
  tt <- qcaert_expect_no_warning(QCA::truthTable(
    dat,
    outcome = "Y",
    conditions = c("A", "B"),
    incl.cut = 0.75,
    n.cut = 1
  ))
  sol <- qcaert_expect_no_warning(QCA::minimize(tt, include = ""))

  expect_true(is.data.frame(sol$pims))

  sol_table <- sol.df(conservative = sol, solution = "conservative")
  expect_true("Cases" %in% names(sol_table))
  expect_true(nrow(sol_table) >= 1L)
})

test_that("cluster.test does not duplicate one-term solutions as term components", {
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

test_that("cluster.test pooled and cluster fits agree with QCA pof", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_cluster()

  pof_fit <- function(x, y, necessity) {
    relation <- if (necessity) "necessity" else "sufficiency"
    fit <- qcaert_expect_no_warning(QCA::pof(
      setms = x,
      outcome = y,
      relation = relation
    ))$incl.cov[1L, , drop = FALSE]

    if (necessity) {
      c(
        consistency = fit$inclN,
        coverage = fit$covN
      )
    } else {
      c(
        consistency = fit$inclS,
        coverage = fit$covS
      )
    }
  }

  for (necessity in c(FALSE, TRUE)) {
    out <- qcaert_expect_no_warning(cluster.test(
      data = fixture$data,
      tt = fixture$truth_table,
      cluster_id = fixture$cluster_id,
      unit_id = fixture$unit_id,
      solution = "conservative",
      necessity = necessity,
      progress = FALSE
    ))

    expect_length(out$baseline$configurations, 1L)
    configuration <- out$baseline$configurations[[1L]]
    configuration_key <- configuration$configuration_key
    x <- configuration$solution_membership
    y <- configuration$outcome_membership

    pooled_expected <- pof_fit(x, y, necessity)
    diagnostic <- out$diagnostics[
      out$diagnostics$configuration_key == configuration_key,
      ,
      drop = FALSE
    ]

    expect_equal(diagnostic$pooled_consistency, unname(pooled_expected["consistency"]))
    expect_equal(diagnostic$pooled_coverage, unname(pooled_expected["coverage"]))

    cluster_results <- out$results$clusters[
      out$results$clusters$configuration == configuration_key &
        out$results$clusters$component == "solution",
      ,
      drop = FALSE
    ]

    for (cluster_value in cluster_results$cluster_id) {
      rows <- !is.na(fixture$data[[fixture$cluster_id]]) &
        fixture$data[[fixture$cluster_id]] == cluster_value
      cluster_expected <- pof_fit(x[rows], y[rows], necessity)
      cluster_observed <- cluster_results[
        cluster_results$cluster_id == cluster_value,
        ,
        drop = FALSE
      ]

      expect_equal(
        cluster_observed$consistency,
        unname(cluster_expected["consistency"])
      )
      expect_equal(
        cluster_observed$coverage,
        unname(cluster_expected["coverage"])
      )
    }
  }
})

test_that("cluster.test keeps unit identifiers out of pooled and cluster fit", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_cluster()

  baseline <- qcaert_expect_no_warning(cluster.test(
    data = fixture$data,
    tt = fixture$truth_table,
    cluster_id = fixture$cluster_id,
    unit_id = fixture$unit_id,
    solution = "conservative",
    progress = FALSE
  ))

  missing_unit_data <- fixture$data
  missing_unit_data[[fixture$unit_id]][4L] <- NA_character_
  missing_unit <- qcaert_expect_no_warning(cluster.test(
    data = missing_unit_data,
    tt = fixture$truth_table,
    cluster_id = fixture$cluster_id,
    unit_id = fixture$unit_id,
    solution = "conservative",
    progress = FALSE
  ))

  no_unit <- qcaert_expect_no_warning(cluster.test(
    data = missing_unit_data,
    tt = fixture$truth_table,
    cluster_id = fixture$cluster_id,
    unit_id = NULL,
    solution = "conservative",
    progress = FALSE
  ))

  overview_fit <- c(
    "configuration",
    "pooled_consistency",
    "pooled_coverage",
    "max_abs_cluster_delta_consistency",
    "max_abs_cluster_delta_coverage"
  )
  cluster_fit <- c(
    "configuration",
    "component",
    "cluster_id",
    "cluster_size",
    "consistency",
    "coverage",
    "delta_consistency",
    "delta_coverage"
  )

  expect_equal(
    missing_unit$results$overview[, overview_fit, drop = FALSE],
    baseline$results$overview[, overview_fit, drop = FALSE]
  )
  expect_equal(
    missing_unit$results$clusters[, cluster_fit, drop = FALSE],
    baseline$results$clusters[, cluster_fit, drop = FALSE]
  )
  expect_equal(
    no_unit$results$overview[, overview_fit, drop = FALSE],
    baseline$results$overview[, overview_fit, drop = FALSE]
  )
  expect_equal(
    no_unit$results$clusters[, cluster_fit, drop = FALSE],
    baseline$results$clusters[, cluster_fit, drop = FALSE]
  )

  expect_true(all(baseline$results$units$n_clusters == 2L))
  expect_false("U4" %in% missing_unit$results$units$unit_id)
  expect_null(no_unit$results$units)
})
