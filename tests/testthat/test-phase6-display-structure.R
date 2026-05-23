test_that("LOO print counts standardized fit-change results", {
  x <- structure(
    list(
      results = data.frame(
        row_index = 1:2,
        case_label = c("case 1", "case 2"),
        status = c("ok", "ok"),
        solution_change = c(NA_character_, "CON: A"),
        fit_changed_types = c("CON", NA_character_),
        n_fit_deltas = c(1L, 0L),
        max_abs_fit_delta = c(0.1, 0),
        stringsAsFactors = FALSE
      ),
      settings = list(
        solution = "conservative",
        monitored_solutions = "conservative",
        which_M = 1L,
        n_cases_tested = 2L,
        fit_measures = "inclS"
      )
    ),
    class = "loo_test"
  )

  out <- capture.output(print(x))

  expect_true(any(grepl("Solution changes: 1", out, fixed = TRUE)))
  expect_true(any(grepl("Fit changes: 1", out, fixed = TRUE)))
})

test_that("boundary print summaries do not repeat stop reasons", {
  results <- data.frame(
    direction = c("lower", "upper"),
    start = c(0.8, 0.8),
    last_safe = c(0.75, 0.85),
    first_failing = c(0.7, 0.9),
    steps = c(1L, 1L),
    total_delta = c(-0.05, 0.05),
    reason = c("CON:formula_changed", "PAR:formula_changed"),
    stringsAsFactors = FALSE
  )
  x <- structure(
    list(
      results = results,
      bounds = c(Lower = 0.75, Upper = 0.85),
      settings = list(
        solution = "conservative",
        monitored_solutions = "conservative",
        which_M = 1L,
        step = 0.05,
        max_steps = 2L
      )
    ),
    class = "incl_test"
  )

  out <- capture.output(print(x))

  expect_true(any(grepl("Stable interval", out, fixed = TRUE)))
  expect_false(any(grepl("search stopped because", out, fixed = TRUE)))
})

test_that("all-solution long boundary print summaries do not repeat solution_type stop reasons", {
  results <- data.frame(
    solution_type = rep(c("conservative", "parsimonious", "intermediate"), each = 2L),
    direction = rep(c("lower", "upper"), times = 3L),
    start = 0.8,
    last_safe = c(0.75, 0.85, 0.8, 0.85, 0.75, 0.8),
    first_failing = c(0.7, 0.9, 0.75, 0.9, 0.7, 0.85),
    steps = c(1L, 1L, 0L, 1L, 1L, 0L),
    total_delta = c(-0.05, 0.05, 0, 0.05, -0.05, 0),
    reason = c(
      "CON:formula_changed",
      "CON:formula_changed",
      "PAR:formula_changed",
      "PAR:formula_changed",
      "INT:formula_changed",
      "INT:formula_changed"
    ),
    stringsAsFactors = FALSE
  )
  bounds <- matrix(
    c(0.75, 0.85, 0.8, 0.85, 0.75, 0.8),
    nrow = 2L,
    dimnames = list(c("Lower", "Upper"), c("conservative", "parsimonious", "intermediate"))
  )
  x <- structure(
    list(
      results = results,
      bounds = bounds,
      settings = list(
        solution = "all",
        monitored_solutions = c("conservative", "parsimonious", "intermediate"),
        which_M = 1L,
        i_mode = "all",
        step = 0.05,
        max_steps = 2L
      )
    ),
    class = "ncut_test"
  )

  out <- capture.output(print(x))

  expect_true(any(grepl("Stable intervals", out, fixed = TRUE)))
  expect_false(any(grepl("CON Lower search stopped because", out, fixed = TRUE)))
  expect_false(any(grepl("search stopped because", out, fixed = TRUE)))
})

test_that("subsample print labels aggregate rows and solution-type-specific exact matches clearly", {
  x <- structure(
    list(
      results = data.frame(
        rep = 1:2,
        n_sample = c(8L, 8L),
        n_holdout = c(2L, 2L),
        status = c("ok", "ok"),
        exact_match_baseline = c(TRUE, FALSE),
        term_jaccard_baseline = c(1, 0.5),
        solution_change = c(NA_character_, "CON: formula_changed"),
        fit_changed_types = c(NA_character_, "CON"),
        n_fit_deltas = c(0L, 1L),
        max_abs_fit_delta = c(0, 0.1),
        stringsAsFactors = FALSE
      ),
      summary = list(
        exact_solution = list(
          conservative = list(
            n_exact = 1L,
            n_successful = 2L,
            prop_exact = 0.5
          )
        )
      ),
      settings = list(
        solution = "conservative",
        monitored_solutions = "conservative",
        reps = 2L,
        sample_n = 8L
      )
    ),
    class = "subsample_test"
  )

  out <- capture.output(print(x))

  expect_true(any(grepl("Runs with any solution change: 1", out, fixed = TRUE)))
  expect_true(any(grepl("Runs with any fit change: 1", out, fixed = TRUE)))
  expect_true(any(grepl("Exact baseline matches by solution type", out, fixed = TRUE)))
})

test_that("altset print foregrounds solution-type-specific match rates", {
  x <- structure(
    list(
      results = data.frame(
        draw = 1:2,
        incl.cut = c(0.75, 0.8),
        n.cut = c(1L, 1L),
        status = c("ok", "ok"),
        n_changed_sets = c(1L, 1L),
        changed_sets = c("A", "A"),
        changed_roles = c("condition", "condition"),
        solution_change = c(NA_character_, "CON: formula_changed"),
        fit_changed_types = c(NA_character_, "CON"),
        n_fit_deltas = c(0L, 1L),
        max_abs_fit_delta = c(0, 0.1),
        stringsAsFactors = FALSE
      ),
      summary = list(
        n_draws = 2L,
        n_fit_compared = 2L,
        score_solution_by_solution_type = c(conservative = 0.5),
        score_fit_by_solution_type = c(conservative = 0.5)
      ),
      settings = list(
        solution = "conservative",
        monitored_solutions = "conservative",
        n_draws = 2L,
        incl.cut = 0.75,
        n.cut = 1,
        fit_tol = 1e-6,
        test.conditions = "A",
        test.outcome = FALSE
      )
    ),
    class = "altset_test"
  )

  out <- capture.output(print(x))

  expect_true(any(grepl("Draws with any solution change: 1", out, fixed = TRUE)))
  expect_true(any(grepl("Solution match rate by solution type", out, fixed = TRUE)))
  expect_false(any(grepl("Solution stability score", out, fixed = TRUE)))
  expect_false(any(grepl("Total stability score", out, fixed = TRUE)))
})

test_that("as.data.frame returns the standardized user-facing table", {
  results <- data.frame(id = 1L)
  classes <- c(
    "altset_test",
    "calib_test",
    "incl_test",
    "loo_test",
    "ncut_test",
    "subsample_test"
  )

  for (class in classes) {
    x <- structure(list(results = results), class = class)
    expect_identical(as.data.frame(x), results)
  }

  cluster <- structure(
    list(results = list(overview = results, clusters = NULL, units = NULL)),
    class = "cluster_test"
  )
  expect_identical(as.data.frame(cluster), results)
})

test_that("calib print uses compact condition-first display table", {
  results <- data.frame(
    set = c("A", "Y"),
    role = c("condition", "outcome"),
    raw = c("A_raw", "Y_raw"),
    type = c("fuzzy", "fuzzy"),
    method = c("direct", "direct"),
    anchor = c("E1", "E1"),
    direction = c("lower", "lower"),
    start = c(10, 20),
    last_safe = c(9, 19),
    first_failing = c(8, 18),
    step_unit = c(1, 1),
    steps = c(1L, 1L),
    total_delta = c(-1, -1),
    pct_raw_range = c(-0.1, -0.1),
    reason = c("formula_changed", "formula_changed"),
    stringsAsFactors = FALSE
  )
  x <- structure(
    list(
      results = results,
      bounds = list(),
      settings = list(
        outcome = "Y",
        solution = "conservative",
        monitored_solutions = "conservative",
        which_M = 1L,
        test.conditions = "A",
        test.outcome = TRUE,
        max_steps = 2L
      )
    ),
    class = "calib_test"
  )

  display <- qcaERT:::.calib_results_for_print(results)
  expect_false("set" %in% names(display))
  expect_false("role" %in% names(display))
  expect_false("raw" %in% names(display))
  expect_true("condition" %in% names(display))
  expect_identical(display$condition, c("A_raw", "Y_raw"))
  expect_identical(as.data.frame(x), results)

  out <- capture.output(print(x))
  expect_true(any(grepl("condition", out, fixed = TRUE)))
  expect_false(any(grepl("\\brole\\b", out)))
  expect_false(any(grepl("\\braw\\b", out)))
  expect_true(any(grepl("Conditions covered: A_raw, Y_raw", out, fixed = TRUE)))
})

test_that("cluster print uses compact overview and single-configuration details", {
  overview <- data.frame(
    solution_type = "INT",
    configuration = "C1P1:~DEV*URB*LIT*IND*~STB",
    components = 2L,
    status = "ok",
    pooled_consistency = 0.8716332,
    pooled_coverage = 0.3921098,
    max_abs_cluster_delta_consistency = 0.3308799,
    max_abs_cluster_delta_coverage = 0.02347898,
    worst_cluster_consistency_id = "low development",
    worst_cluster_coverage_id = "low development",
    within_available = FALSE,
    n_units_repeated = 0L,
    stringsAsFactors = FALSE
  )
  clusters <- data.frame(
    solution_type = c("INT", "INT"),
    configuration = overview$configuration,
    solution_expression = overview$configuration,
    component = "solution",
    cluster_id = c("high development", "low development"),
    cluster_size = c(9L, 9L),
    consistency = c(0.9637, 0.5408),
    coverage = c(0.3961, 0.3686),
    delta_consistency = c(0.0921, -0.3309),
    delta_coverage = c(0.0040, -0.0235),
    stringsAsFactors = FALSE
  )
  x <- structure(
    list(
      results = list(overview = overview, clusters = clusters, units = NULL),
      settings = list(
        solution = "intermediate",
        monitored_solutions = "intermediate",
        which_M = 1L,
        i_mode = "all",
        cluster_id = "DEV_group",
        unit_id = "case_id",
        necessity = FALSE
      )
    ),
    class = "cluster_test"
  )

  out <- capture.output(print(x))

  expect_true(any(grepl("pooled_cons", out, fixed = TRUE)))
  expect_true(any(grepl("worst_consistency_cluster", out, fixed = TRUE)))
  expect_true(any(grepl("Cluster details", out, fixed = TRUE)))
  expect_true(any(grepl("+0.092", out, fixed = TRUE)))
  expect_true(any(grepl("Within-unit diagnostics", out, fixed = TRUE)))
  expect_true(any(grepl("Not available: 0 units are observed in more than one cluster.", out, fixed = TRUE)))
  expect_true(any(grepl("Detailed tables: x$results$overview, x$results$clusters", out, fixed = TRUE)))
  expect_false(any(grepl("Detailed cluster table", out, fixed = TRUE)))
})
