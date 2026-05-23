theory_test_fixture <- function() {
  data.frame(
    Y = c(0, 0.33, 0.67, 1),
    A = c(1, 0.8, 0.2, 0),
    B = c(0.9, 0.7, 0.4, 0.1),
    C = c(0.2, 0.6, 0.8, 1),
    D = c(0.1, 0.4, 0.7, 0.9)
  )
}

test_that("theory.test empty table helpers define the public schemas", {
  diagnostics <- .empty_theory_diagnostics()
  models <- .empty_theory_models()
  pairwise <- .empty_theory_pairwise()
  solutions <- .empty_theory_solutions()
  results <- .empty_theory_results()

  expect_identical(
    names(diagnostics),
    c(
      "theory",
      "solution_type",
      "status",
      "n_conditions",
      "conditions",
      "n_tt_rows",
      "n_observed_rows",
      "n_remainders",
      "n_excluded",
      "selected_solution_missing",
      "error_source",
      "error_message"
    )
  )
  expect_identical(
    names(models),
    c(
      "theory",
      "solution_type",
      "intermediate_branch",
      "status",
      "n_conditions",
      "n_tt_rows",
      "n_observed_rows",
      "n_remainders",
      "n_excluded",
      "n_models",
      "selected_model",
      "n_terms",
      "inclS",
      "PRI",
      "covS"
    )
  )
  expect_identical(
    names(pairwise),
    c(
      "solution_type",
      "intermediate_branch",
      "theory_1",
      "theory_2",
      "delta_inclS",
      "delta_PRI",
      "delta_covS",
      "membership_jaccard",
      "mean_abs_membership_delta"
    )
  )
  expect_identical(
    names(solutions),
    c("theory", "solution_type", "model", "intermediate_branch", "prime_implicant", "inclS", "PRI", "covS")
  )
  expect_identical(names(results), c("models", "pairwise", "solutions"))
  expect_identical(results$models, models)
  expect_identical(results$pairwise, pairwise)
  expect_identical(results$solutions, solutions)

  expect_identical(nrow(diagnostics), 0L)
  expect_identical(nrow(models), 0L)
  expect_identical(nrow(pairwise), 0L)
  expect_identical(nrow(solutions), 0L)
  expect_type(diagnostics$selected_solution_missing, "logical")
  expect_type(models$inclS, "double")
  expect_type(pairwise$membership_jaccard, "double")
  expect_type(solutions$model, "integer")
})

test_that("theory.test lightweight normalizers preserve theory-specific settings", {
  dat <- theory_test_fixture()
  theories <- .normalize_theories(
    theories = list(
      institutional = c("A", "B"),
      structural = c("B", "C", "D")
    ),
    data = dat,
    outcome = "Y"
  )

  expect_identical(
    theories,
    list(
      institutional = c("A", "B"),
      structural = c("B", "C", "D")
    )
  )

  dir_exp <- .normalize_theory_dir_exp(
    dir.exp = list(
      institutional = c(B = "0", A = "1"),
      structural = c(D = "1", B = "0", C = "1")
    ),
    theories = theories,
    solution = "all"
  )

  expect_identical(dir_exp$institutional, c("1", "0"))
  expect_identical(dir_exp$structural, c("0", "1", "1"))
  expect_null(.normalize_theory_dir_exp(NULL, theories, solution = "conservative"))

  expect_error(
    .normalize_theories(
      theories = list(one = c("A", "Y"), two = c("B", "C")),
      data = dat,
      outcome = "Y"
    ),
    "Theory `one` includes the outcome as a condition",
    fixed = TRUE
  )
  expect_error(
    .normalize_theory_dir_exp(
      dir.exp = list(institutional = c("1", "0")),
      theories = theories,
      solution = "all"
    ),
    "Missing: structural.",
    fixed = TRUE
  )
  expect_error(
    .normalize_theory_dir_exp(NULL, theories, solution = "intermediate"),
    "When `solution = \"intermediate\"`/`\"int\"`, `dir.exp` must be provided",
    fixed = TRUE
  )
})

test_that("theory.test LR integration fills models, solutions, and pairwise tables", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_lr()
  theories <- list(
    development = c("DEV", "URB", "LIT"),
    industrial = c("DEV", "URB", "IND"),
    broad = c("DEV", "URB", "LIT", "IND", "STB")
  )
  dir_exp <- list(
    development = c("1", "1", "1"),
    industrial = c("1", "1", "1"),
    broad = c("1", "1", "1", "1", "1")
  )

  out <- theory.test(
    data = fixture$calib,
    outcome = fixture$outcome,
    theories = theories,
    incl.cut = 0.8,
    n.cut = 1,
    solution = "all",
    dir.exp = dir_exp,
    progress = FALSE
  )

  expect_s3_class(out, "theory_test")
  expect_identical(out$settings$theories, theories)
  expect_identical(out$settings$monitored_solutions, c("conservative", "parsimonious", "intermediate"))
  expect_identical(as.data.frame(out), out$results$models)
  expect_true(all(vapply(out$by_theory, `[[`, character(1), "status") == "ok"))

  expect_identical(nrow(out$diagnostics), 9L)
  expect_identical(nrow(out$results$models), 9L)
  expect_identical(nrow(out$results$solutions), 9L)
  expect_identical(nrow(out$results$pairwise), 9L)
  expect_true(all(out$diagnostics$status == "ok"))
  expect_true(all(out$results$models$status == "ok"))

  expect_identical(
    out$results$models[, c("theory", "solution_type")],
    data.frame(
      theory = rep(names(theories), each = 3L),
      solution_type = rep(c("conservative", "parsimonious", "intermediate"), times = 3L),
      stringsAsFactors = FALSE
    )
  )
  expect_identical(out$results$models$n_conditions, rep(c(3L, 3L, 5L), each = 3L))
  expect_identical(out$results$models$n_tt_rows, rep(c(8L, 8L, 32L), each = 3L))
  expect_identical(out$results$models$n_observed_rows, rep(c(5L, 6L, 11L), each = 3L))
  expect_true(all(out$results$models$n_terms >= 1L))
  expect_true(all(out$results$models$inclS >= 0.8))
  expect_true(all(out$results$pairwise$membership_jaccard >= 0))
  expect_true(all(out$results$pairwise$membership_jaccard <= 1))
  expect_true(all(out$results$pairwise$mean_abs_membership_delta >= 0))
  expect_setequal(
    out$results$solutions$prime_implicant,
    c("~DEV*URB*LIT", "~DEV*URB*IND", "~DEV*URB*LIT*IND*~STB", "~DEV*URB*LIT*~STB")
  )
})

test_that("theory.test returns family object structure with per-theory runs", {
  skip_if_not_installed("QCA")

  dat <- theory_test_fixture()
  theories <- list(
    institutional = c("A", "B"),
    structural = c("B", "C", "D")
  )
  dir_exp <- list(
    institutional = c(B = "0", A = "1"),
    structural = c(D = "1", B = "0", C = "1")
  )

  out <- theory.test(
    data = dat,
    outcome = "Y",
    theories = theories,
    incl.cut = 0.8,
    n.cut = 1,
    solution = "all",
    dir.exp = dir_exp,
    i_mode = "C1P1",
    progress = FALSE
  )

  expect_s3_class(out, "theory_test")
  expect_identical(names(out), c("diagnostics", "results", "by_theory", "settings"))
  expect_identical(names(out$results), c("models", "pairwise", "solutions"))
  expect_identical(as.data.frame(out), out$results$models)
  expect_identical(out$settings$monitored_solutions, c("conservative", "parsimonious", "intermediate"))
  expect_identical(out$settings$theories, theories)
  expect_identical(out$settings$dir.exp$institutional, c("1", "0"))
  expect_identical(out$settings$dir.exp$structural, c("0", "1", "1"))
  expect_identical(out$by_theory$institutional$status, "ok")
  expect_s3_class(out$by_theory$institutional$tt, "QCA_tt")
  expect_s3_class(out$by_theory$institutional$res$conservative, "QCA_min")
  expect_s3_class(out$by_theory$institutional$res$parsimonious, "QCA_min")
  expect_s3_class(out$by_theory$institutional$res$intermediate, "QCA_min")
  expect_identical(
    out$diagnostics[, c("theory", "solution_type", "status")],
    data.frame(
      theory = rep(names(theories), each = 3L),
      solution_type = rep(c("conservative", "parsimonious", "intermediate"), times = 2L),
      status = rep("ok", 6L),
      stringsAsFactors = FALSE
    )
  )
  expect_identical(out$diagnostics$n_tt_rows, c(4L, 4L, 4L, 8L, 8L, 8L))
  expect_identical(out$diagnostics$n_observed_rows, c(2L, 2L, 2L, 3L, 3L, 3L))
  expect_identical(out$diagnostics$n_remainders, c(2L, 2L, 2L, 5L, 5L, 5L))

  models <- out$results$models
  expect_identical(
    names(models),
    c(
      "theory",
      "solution_type",
      "intermediate_branch",
      "status",
      "n_conditions",
      "n_tt_rows",
      "n_observed_rows",
      "n_remainders",
      "n_excluded",
      "n_models",
      "selected_model",
      "n_terms",
      "inclS",
      "PRI",
      "covS"
    )
  )
  expect_identical(as.data.frame(out), models)
  expect_identical(models[, c("theory", "solution_type", "status")], out$diagnostics[, c("theory", "solution_type", "status")])
  expect_identical(models$n_conditions, out$diagnostics$n_conditions)
  expect_identical(models$n_tt_rows, out$diagnostics$n_tt_rows)
  expect_identical(models$n_observed_rows, out$diagnostics$n_observed_rows)
  expect_identical(models$n_remainders, out$diagnostics$n_remainders)
  expect_identical(models$n_excluded, out$diagnostics$n_excluded)
  expect_identical(models$n_models, rep(1L, 6L))
  expect_identical(models$selected_model, rep(1L, 6L))
  expect_identical(models$n_terms, rep(1L, 6L))
  expect_identical(models$intermediate_branch, c(NA, NA, "C1P1", NA, NA, "C1P1"))
  expect_equal(models$inclS, c(1, 1, 1, 0.9473684, 0.9473684, 0.9473684), tolerance = 1e-6)
  expect_equal(models$PRI, c(1, 1, 1, 0.9212598, 0.9212598, 0.9212598), tolerance = 1e-6)
  expect_equal(models$covS, c(0.85, 0.85, 0.85, 0.9, 0.9, 0.9), tolerance = 1e-12)

  solutions <- out$results$solutions
  expect_identical(
    names(solutions),
    c("theory", "solution_type", "model", "intermediate_branch", "prime_implicant", "inclS", "PRI", "covS")
  )
  expect_identical(nrow(solutions), sum(models$n_terms))
  expect_identical(
    solutions[, c("theory", "solution_type", "model", "intermediate_branch", "prime_implicant")],
    data.frame(
      theory = rep(names(theories), each = 3L),
      solution_type = rep(c("conservative", "parsimonious", "intermediate"), times = 2L),
      model = rep(1L, 6L),
      intermediate_branch = c(NA, NA, "C1P1", NA, NA, "C1P1"),
      prime_implicant = c("~A*~B", "~A*~B", "~A*~B", "~B*C*D", "~B*D", "~B*C*D"),
      stringsAsFactors = FALSE
    )
  )
  expect_equal(solutions$inclS, models$inclS, tolerance = 1e-6)
  expect_equal(solutions$PRI, models$PRI, tolerance = 1e-6)
  expect_equal(solutions$covS, models$covS, tolerance = 1e-12)

  pairwise <- out$results$pairwise
  expect_identical(
    names(pairwise),
    c(
      "solution_type",
      "intermediate_branch",
      "theory_1",
      "theory_2",
      "delta_inclS",
      "delta_PRI",
      "delta_covS",
      "membership_jaccard",
      "mean_abs_membership_delta"
    )
  )
  expect_identical(pairwise$solution_type, c("conservative", "parsimonious", "intermediate"))
  expect_identical(pairwise$intermediate_branch, c(NA, NA, "C1P1"))
  expect_identical(pairwise$theory_1, rep("institutional", 3L))
  expect_identical(pairwise$theory_2, rep("structural", 3L))
  expect_equal(pairwise$delta_inclS, rep(-0.05263158, 3L), tolerance = 1e-6)
  expect_equal(pairwise$delta_PRI, rep(-0.07874016, 3L), tolerance = 1e-6)
  expect_equal(pairwise$delta_covS, rep(0.05, 3L), tolerance = 1e-12)
  expect_equal(pairwise$membership_jaccard, rep(0.8947368, 3L), tolerance = 1e-6)
  expect_equal(pairwise$mean_abs_membership_delta, rep(0.05, 3L), tolerance = 1e-12)
})

test_that("theory.test keeps conservative-only settings without dir.exp", {
  skip_if_not_installed("QCA")

  dat <- theory_test_fixture()

  out <- theory.test(
    data = dat,
    outcome = "Y",
    theories = list(
      one = c("A", "B"),
      two = c("B", "C")
    ),
    incl.cut = 0.8,
    n.cut = 1,
    solution = "conservative",
    progress = FALSE
  )

  expect_identical(out$settings$solution, "conservative")
  expect_identical(out$settings$include, "")
  expect_identical(out$settings$monitored_solutions, "conservative")
  expect_null(out$settings$dir.exp)
  expect_identical(out$by_theory$one$status, "ok")
  expect_identical(out$by_theory$two$status, "ok")
  expect_identical(out$diagnostics$solution_type, c("conservative", "conservative"))
  expect_true(all(out$diagnostics$status == "ok"))
  expect_identical(out$results$models$solution_type, c("conservative", "conservative"))
  expect_true(all(out$results$models$status == "ok"))
  expect_true(all(out$results$models$n_models >= 1L))
  expect_identical(out$results$solutions$solution_type, c("conservative", "conservative"))
  expect_true(all(!is.na(out$results$solutions$prime_implicant)))
  expect_identical(out$results$pairwise$solution_type, "conservative")
  expect_true(is.na(out$results$pairwise$intermediate_branch))
  expect_equal(out$results$pairwise$membership_jaccard, 0.8947368, tolerance = 1e-6)
})

test_that("theory.test pairwise comparisons expand all theory pairs by solution type", {
  skip_if_not_installed("QCA")

  dat <- theory_test_fixture()
  out <- theory.test(
    data = dat,
    outcome = "Y",
    theories = list(
      institutional = c("A", "B"),
      structural = c("B", "C", "D"),
      behavioral = c("A", "C")
    ),
    incl.cut = 0.8,
    n.cut = 1,
    solution = "all",
    dir.exp = list(
      institutional = c(B = "0", A = "1"),
      structural = c(D = "1", B = "0", C = "1"),
      behavioral = c(A = "1", C = "1")
    ),
    progress = FALSE
  )

  pairwise <- out$results$pairwise

  expect_identical(nrow(pairwise), 9L)
  solution_type_counts <- table(pairwise$solution_type)
  expect_identical(
    as.integer(solution_type_counts[c("conservative", "parsimonious", "intermediate")]),
    c(3L, 3L, 3L)
  )
  expect_setequal(
    paste(pairwise$theory_1, pairwise$theory_2, sep = " / "),
    c(
      "institutional / structural",
      "institutional / behavioral",
      "structural / behavioral"
    )
  )
  expect_true(all(pairwise$membership_jaccard >= 0 & pairwise$membership_jaccard <= 1))
  expect_true(all(pairwise$mean_abs_membership_delta >= 0))
})

test_that("theory.test validates theory condition sets strictly", {
  dat <- theory_test_fixture()

  expect_error(
    theory.test(
      data = dat,
      outcome = "Y",
      theories = list(one = c("A", "Y"), two = c("B", "C")),
      progress = FALSE
    ),
    "Theory `one` includes the outcome as a condition",
    fixed = TRUE
  )

  expect_error(
    theory.test(
      data = dat,
      outcome = "Y",
      theories = list(one = c("A", "missing"), two = c("B", "C")),
      progress = FALSE
    ),
    "Theory `one` contains condition(s) not found in `data`: missing",
    fixed = TRUE
  )

  expect_error(
    theory.test(
      data = dat,
      outcome = "Y",
      theories = list(one = c("A", "A"), two = c("B", "C")),
      progress = FALSE
    ),
    "Theory `one` contains duplicate condition names.",
    fixed = TRUE
  )
})

test_that("theory.test validates theory-specific dir.exp", {
  dat <- theory_test_fixture()
  theories <- list(one = c("A", "B"), two = c("B", "C"))

  expect_error(
    theory.test(
      data = dat,
      outcome = "Y",
      theories = theories,
      solution = "intermediate",
      progress = FALSE
    ),
    "When `solution = \"intermediate\"`/`\"int\"`, `dir.exp` must be provided as a named list",
    fixed = TRUE
  )

  expect_error(
    theory.test(
      data = dat,
      outcome = "Y",
      theories = theories,
      dir.exp = list(one = c("1", "0")),
      progress = FALSE
    ),
    "Missing: two.",
    fixed = TRUE
  )

  expect_error(
    theory.test(
      data = dat,
      outcome = "Y",
      theories = theories,
      dir.exp = list(one = c(A = "1", C = "0"), two = c(B = "1", C = "0")),
      progress = FALSE
    ),
    "`dir.exp[[\"one\"]]` names must match that theory's conditions.",
    fixed = TRUE
  )
})

test_that("theory.test validates static theory-specific exclusions", {
  dat <- theory_test_fixture()
  theories <- list(one = c("A", "B"), two = c("B", "C"))
  dir_exp <- list(one = c("1", "0"), two = c("1", "1"))

  expect_error(
    theory.test(
      data = dat,
      outcome = "Y",
      theories = theories,
      dir.exp = dir_exp,
      exclude_mode = "static",
      exclude_static = list(one = 1:2),
      progress = FALSE
    ),
    "Missing: two.",
    fixed = TRUE
  )
})

test_that("theory.test diagnostics preserve solution-type-specific exclude errors", {
  skip_if_not_installed("QCA")

  dat <- theory_test_fixture()
  dir_exp <- list(
    one = c(A = "1", B = "0"),
    two = c(B = "0", C = "1")
  )

  out <- theory.test(
    data = dat,
    outcome = "Y",
    theories = list(one = c("A", "B"), two = c("B", "C")),
    incl.cut = 0.8,
    n.cut = 1,
    solution = "all",
    dir.exp = dir_exp,
    exclude_recompute = list(type = 2, unknown_argument = TRUE),
    progress = FALSE
  )

  expect_identical(out$by_theory$one$status, "partial")
  expect_identical(out$by_theory$two$status, "partial")
  expect_identical(
    out$diagnostics$status,
    rep(c("ok", "exclude_error", "exclude_error"), times = 2L)
  )
  expect_identical(
    out$results$models$status,
    rep(c("ok", "exclude_error", "exclude_error"), times = 2L)
  )
  expect_true(all(is.na(out$results$models$inclS[out$results$models$status == "exclude_error"])))
  expect_true(all(is.na(out$results$models$selected_model[out$results$models$status == "exclude_error"])))
  expect_identical(out$results$solutions$solution_type, c("conservative", "conservative"))
  expect_false(any(out$results$solutions$solution_type %in% c("parsimonious", "intermediate")))
  expect_identical(out$results$pairwise$solution_type, "conservative")
  expect_false(any(out$results$pairwise$solution_type %in% c("parsimonious", "intermediate")))
  expect_true(all(out$diagnostics$error_source[out$diagnostics$status == "exclude_error"] == "exclude"))
  expect_true(all(is.na(out$diagnostics$error_source[out$diagnostics$status == "ok"])))
})

test_that("theory.test print summarizes run structure", {
  skip_if_not_installed("QCA")

  dat <- theory_test_fixture()
  out <- theory.test(
    data = dat,
    outcome = "Y",
    theories = list(one = c("A", "B"), two = c("B", "C")),
    incl.cut = 0.8,
    n.cut = 1,
    solution = "all",
    progress = FALSE
  )

  printed <- capture.output(print(out))

  expect_true(any(grepl("<theory_test>", printed, fixed = TRUE)))
  expect_true(any(grepl("qcaERT theory-specification test", printed, fixed = TRUE)))
  expect_true(any(grepl("Theories tested: 2", printed, fixed = TRUE)))
  expect_true(any(grepl("Solution: all", printed, fixed = TRUE)))
  expect_true(any(grepl("Monitored solutions: conservative, parsimonious", printed, fixed = TRUE)))
  expect_false(any(grepl("Exclusion handling:", printed, fixed = TRUE)))
  expect_true(any(grepl("one: ok", printed, fixed = TRUE)))
  expect_true(any(grepl("Solutions", printed, fixed = TRUE)))
  expect_true(any(grepl("prime_implicant", printed, fixed = TRUE)))
  expect_true(any(grepl("Tables:", printed, fixed = TRUE)))
  expect_true(any(grepl("x$results$models: model-level diagnostics (4 rows)", printed, fixed = TRUE)))
  expect_true(any(grepl("x$results$solutions: extracted solution terms (4 rows)", printed, fixed = TRUE)))
  expect_true(any(grepl("x$results$pairwise: pairwise theory comparisons (2 rows)", printed, fixed = TRUE)))
  expect_true(any(grepl("x$diagnostics: raw per-theory solution_type diagnostics (4 rows)", printed, fixed = TRUE)))
  expect_true(any(grepl("x$results$pairwise", printed, fixed = TRUE)))
})
