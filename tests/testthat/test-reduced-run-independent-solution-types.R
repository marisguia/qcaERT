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

test_that("leave-one-out distinguishes changed fit from a missing selected model", {
  skip_if_not_installed("QCA")

  data <- data.frame(
    A = c(1, 1, 0, 1, 0, 0, 0),
    B = c(1, 1, 0, 0, 1, 0, 0),
    C = c(1, 1, 0, 0, 0, 1, 0),
    Y = c(1, 1, 1, 0, 0, 0, 0)
  )
  out <- loo.test(
    data = data,
    outcome = "Y",
    conditions = c("A", "B", "C"),
    cases = c(3L, 4L),
    solution = "parsimonious",
    exclude_mode = "none",
    which_M = 3,
    progress = FALSE
  )

  # Removing case 3 leaves all three models and removes an uncovered outcome case.
  # Removing case 4 leaves two models, so model 3 is unavailable.
  expect_identical(out$diagnostics$solution_changed, c(FALSE, TRUE))
  expect_identical(out$diagnostics$fit_changed, c(TRUE, NA))
  expect_identical(out$results$n_fit_deltas, c(1L, NA_integer_))
  expect_equal(out$results$max_abs_fit_delta, c(1 / 3, NA_real_))
})

test_that("intermediate fit comparisons use the selected model within each branch", {
  first_model <- list(sol.incl.cov = data.frame(covS = 0.9))
  second_model <- list(sol.incl.cov = data.frame(covS = 0.5))
  branch <- list(IC = list(individual = list(first_model, second_model)))
  baseline <- list(i.sol = list(C1P1 = branch, C1P2 = branch))
  reduced <- list(i.sol = list(
    C1P1 = list(IC = first_model),
    C1P2 = list(IC = list(individual = list(
      first_model, list(sol.incl.cov = data.frame(covS = 0.75))
    )))
  ))

  base_fit <- .reduced_extract_intermediate_fit(baseline, "intermediate", 2L, "all", "covS")
  cur_fit <- .reduced_extract_intermediate_fit(reduced, "intermediate", 2L, "all", "covS")
  expect_equal(base_fit$fit, c(intermediate.C1P1.covS = 0.5, intermediate.C1P2.covS = 0.5))
  expect_equal(cur_fit$fit, c(intermediate.C1P2.covS = 0.75))

  comparison <- .reduced_fit_change_info(
    base = list(status = "ok", fit = list(intermediate = base_fit$fit)),
    cur = list(status = "ok", fit = list(intermediate = cur_fit$fit)),
    monitored_solutions = "intermediate",
    fit_tol = 0,
    fit_measures = "covS"
  )
  expect_equal(comparison$delta, c(intermediate.C1P2.covS = 0.25))
  expect_identical(comparison$n_changed, 1L)
})

test_that("an exact baseline match requires all requested solution comparisons", {
  skip_if_not_installed("QCA")

  baseline <- list(
    status = "ok",
    solution_type_status = c(conservative = "ok", parsimonious = "ok"),
    selected_solution_missing = c(conservative = FALSE, parsimonious = FALSE),
    sig = list(conservative = "A*B", parsimonious = "A")
  )
  partial <- baseline
  partial$solution_type_status[["parsimonious"]] <- "minimize_error"
  partial$sig$parsimonious <- NULL
  partial_with_change <- partial
  partial_with_change$sig$conservative <- "B"

  runs <- list(baseline, baseline, partial, partial_with_change)
  run_index <- 0L
  local_mocked_bindings(
    .reduced_run_once = function(...) {
      run_index <<- run_index + 1L
      runs[[run_index]]
    },
    .package = "qcaERT"
  )

  out <- subsample.test(
    data = tiny_qca, outcome = "Y", conditions = c("A", "B"),
    sample_n = 3, reps = 3, seed = 1, fit_measures = NULL, progress = FALSE
  )

  expect_identical(out$results$status, rep("ok", 3))
  expect_identical(out$results$exact_match_baseline, c(TRUE, NA, FALSE))
  expect_equal(out$summary$exact_solution$conservative$n_exact, 2)
  expect_equal(out$summary$exact_solution$parsimonious$n_successful, 1)
})
