#' Leave-one-out robustness test for QCA solutions
#'
#' Deletes one case at a time, reruns the QCA analysis, and records whether the
#' monitored solution changes, if selected fit measures change, and
#' whether the reduced-data run ends with an error. The tested cases can be
#' identified by row index or by case label.
#'
#' @param data A data frame object containing the outcome and condition
#'   columns used in the QCA analysis. `data` must have at least
#'   two rows.
#' @param outcome Name of the outcome. This must be a single
#'   non-empty character string.
#' @param conditions Optional character vector of condition names. If `NULL`,
#'   the condition set is left to [QCA::truthTable()]. When
#'   `calib = "recompute"`, `conditions` must be supplied explicitly.
#' @param cases Cases to test. Use `NULL` to test all rows, a numeric vector of
#'   row indices, or a character vector of case labels.
#' @param case_labels Optional character vector of case labels with length
#'   `nrow(data)`. If `NULL`, the function uses `rownames(data)` when
#'   available; otherwise it uses row numbers converted to character.
#' @param calib Calibration handling for reduced-data runs. `"fixed"` uses the
#'   calibrated values already present in `data`. `"recompute"` recalibrates
#'   the outcome and conditions after deleting each case, using `raw.data` and
#'   `calib_spec`.
#' @param raw.data Raw-data frame object used when `calib = "recompute"`.
#'   It must have the same number of rows as `data`.
#' @param calib_spec Calibration specification used when
#'   `calib = "recompute"`. This must be a named list keyed by the analysis
#'   sets `c(outcome, conditions)`. Each entry must describe the raw source
#'   column, the set type, and the calibration inputs used to rebuild the
#'   calibrated set. Use the same `calib_spec` structure described for
#'   [calib.test()]. An entry may additionally contain `findTh`, a named list
#'   of [QCA::findTh()] arguments; when supplied, thresholds are re-estimated
#'   after each case deletion instead of reusing the baseline `thresholds`.
#' @param incl.cut Inclusion cutoff passed to [QCA::truthTable()]. This must
#'   be a single finite number in `[0, 1]`.
#' @param n.cut Truth table frequency cutoff passed to [QCA::truthTable()].
#'   This must be an integer-like value of at least `1`.
#' @param solution Solution type to monitor. Accepted values are `"all"`,
#'   `"con"` or `"conservative"`, `"par"` or `"parsimonious"`, and `"int"` or
#'   `"intermediate"`.
#' @param include Optional minimization include setting. Currently, this
#'   argument accepts only `NULL`, `""`, or `"?"`.
#' @param dir.exp Directional expectations used when the monitored solution is
#'   intermediate.
#' @param which_M Positive integer giving which solution alternative to use
#'   when minimization returns multiple models.
#' @param i_mode Character string controlling intermediate-solution selection.
#'   Accepted values are `"all"` and `"C1P1"`.
#' @param exclude_mode Character string controlling how excluded rows are
#'   handled for parsimonious and intermediate minimization. `"recompute"`
#'   recalculates exclusions from each reduced truth table, `"static"` reuses
#'   `exclude_static`, and `"none"` does not use exclusions.
#' @param exclude_recompute Named list of arguments passed to [QCA::findRows()]
#'   when `exclude_mode = "recompute"`.
#' @param exclude_static Already computed exclusion object reused when
#'   `exclude_mode = "static"`.
#' @param fit_measures Character vector of fit-measure names to compare between
#'   the baseline run and each reduced-data run. Use `NULL` to disable fit
#'   comparison.
#' @param fit_tol Non-negative tolerance used when deciding whether fit values
#'   changed.
#' @param progress Logical; if `TRUE` and the session is interactive, show a
#'   text progress bar.
#' @param x A `loo_test` object returned by [loo.test()].
#' @param row.names Logical; passed to [print.data.frame()] by
#'   [print.loo_test()].
#' @param ... Additional arguments. In [loo.test()], named arguments matching
#'   [QCA::truthTable()] formals are passed to [QCA::truthTable()], and the
#'   remaining named arguments are forwarded to [QCA::minimize()] after
#'   removing names reserved by `loo.test()`. The function also
#'   looks in `...` for `include`, `dir.exp`, or `direxp` if those arguments
#'   were not supplied explicitly. In [print.loo_test()], `...` is passed to
#'   [print.data.frame()]. In [as.data.frame.loo_test()], `...` is ignored.
#'
#' @returns An object of class `loo_test` with the following components:
#'   \describe{
#'     \item{`diagnostics`}{A detailed data frame with one row per tested case.
#'     It records the tested row index, case label, run status, solution-change
#'     classification, fit-change classification, the number of changed fit
#'     measures, the largest absolute fit change, and error information when a
#'     reduced-data run fails.}
#'     \item{`results`}{A compact data frame with the columns
#'     `row_index`, `case_label`, `status`, `solution_change`,
#'     `fit_changed_types`, `n_fit_deltas`, and `max_abs_fit_delta`.}
#'     \item{`baseline`}{A list containing the baseline analysis built from the
#'     full dataset, including the truth table, minimization output, selected
#'     solution terms used for comparison, fit values, exclusion information,
#'     and calibration information.}
#'     \item{`by_case`}{A named list with one entry per tested case. Each entry
#'     stores the baseline run, the reduced-data run when available, change
#'     summaries, fit deltas, exclusion information, calibration information,
#'     and any error information for that case.}
#'     \item{`settings`}{A list containing the analysis settings used to build
#'     the result object.}
#'   }
#'
#'   `print.loo_test()` prints a concise summary and the `results` table.
#'   `as.data.frame.loo_test()` returns the `results` table.
#'
#' @details
#' The function first runs the baseline analysis on the full dataset, then
#' removes one selected case at a time and reruns the analysis on the reduced
#' data.
#'
#' When `calib = "fixed"`, the reduced-data runs use the calibrated values
#' already present in `data`. When `calib = "recompute"`, the function rebuilds
#' the calibrated outcome and condition columns after each case deletion
#' using [QCA::findTh()] and [QCA::calibrate()] as specified in `calib_spec`.
#'
#' For each tested case, the function records whether the monitored solution
#' changed, whether monitored fit measures changed beyond `fit_tol`, and
#' whether the reduced-data run stopped with a calibration, truth table,
#' exclusion, or minimization error.
#'
#' The shared solution-control, exclusion, and returned-object conventions are
#' described in `?qcaERT_conventions`.
#'
#' @examples
#' \donttest{
#' library(QCA)
#' data(LR)
#'
#' conditions <- c("DEV", "URB", "LIT", "IND", "STB")
#' outcome <- "SURV"
#' dir_exp <- rep("1", length(conditions))
#'
#' thresholds <- list(
#'   DEV = findTh(LR$DEV, groups = 7),
#'   URB = findTh(LR$URB, groups = 4),
#'   LIT = findTh(LR$LIT, groups = 4),
#'   IND = findTh(LR$IND, groups = 4),
#'   STB = findTh(LR$STB, groups = 4),
#'   SURV = findTh(LR$SURV, groups = 4)
#' )
#'
#' dat <- LR
#' dat$DEV <- calibrate(LR$DEV, type = "fuzzy", thresholds = thresholds$DEV)
#' dat$URB <- calibrate(LR$URB, type = "fuzzy", thresholds = thresholds$URB)
#' dat$LIT <- calibrate(LR$LIT, type = "fuzzy", thresholds = thresholds$LIT)
#' dat$IND <- calibrate(LR$IND, type = "fuzzy", thresholds = thresholds$IND)
#' dat$STB <- calibrate(LR$STB, type = "fuzzy", thresholds = thresholds$STB)
#' dat$SURV <- calibrate(LR$SURV, type = "fuzzy", thresholds = thresholds$SURV)
#'
#' out <- loo.test(
#'   data = dat,
#'   outcome = outcome,
#'   conditions = conditions,
#'   cases = seq_len(nrow(dat)),
#'   case_labels = rownames(dat),
#'   calib = "fixed",
#'   incl.cut = 0.8,
#'   n.cut = 1,
#'   solution = "all",
#'   dir.exp = dir_exp,
#'   fit_measures = c("inclS", "PRI", "covS"),
#'   progress = TRUE
#' )
#'
#' out
#' as.data.frame(out)
#' }
#'
#' @seealso [calib.test()], [incl.test()], [ncut.test()], [subsample.test()],
#'   [altset.test()], [theory.test()], [cluster.test()], [sol.df()]
#' @export
loo.test <- function(
    data,
    outcome,
    conditions = NULL,
    cases = NULL,
    case_labels = NULL,
    calib = c("fixed", "recompute"),
    raw.data = NULL,
    calib_spec = NULL,
    incl.cut = 1,
    n.cut = 1,
    solution = "all",
    include = NULL,
    dir.exp = NULL,
    which_M = 1,
    i_mode = c("all", "C1P1"),
    exclude_mode = c("recompute", "static", "none"),
    exclude_recompute = list(type = 2),
    exclude_static = NULL,
    fit_measures = c("inclS", "PRI", "covS"),
    fit_tol = 0,
    progress = TRUE,
    ...
) {
  .require_qca()

  selection_controls <- .normalize_i_mode(i_mode)
  i_mode <- selection_controls$i_mode
  exclude_mode <- match.arg(exclude_mode)
  calib <- match.arg(calib)

  if (is.null(dim(data)) || is.null(nrow(data)) || nrow(data) < 2L) {
    stop("`data` must be a data frame object with at least 2 rows.")
  }

  if (!is.character(outcome) || length(outcome) != 1L || !nzchar(outcome)) {
    stop("`outcome` must be a single non-empty character string.")
  }

  if (!is.null(conditions)) {
    if (!is.character(conditions) || length(conditions) < 1L) {
      stop("`conditions` must be NULL or a non-empty character vector.")
    }
  }
  .validate_outcome_conditions_distinct(outcome, conditions)

  if (!is.numeric(incl.cut) || length(incl.cut) != 1L || !is.finite(incl.cut) ||
      incl.cut < 0 || incl.cut > 1) {
    stop("`incl.cut` must be a single finite number in [0, 1].")
  }

  n.cut <- .as_integerish_scalar(n.cut, "n.cut", min = 1L)

  which_M <- .as_integerish_scalar(which_M, "which_M", min = 1L)

  if (!is.numeric(fit_tol) || length(fit_tol) != 1L || !is.finite(fit_tol) || fit_tol < 0) {
    stop("`fit_tol` must be a single finite number >= 0.")
  }

  if (is.null(fit_measures)) {
    fit_measures <- character(0)
  } else {
    if (!is.character(fit_measures)) {
      stop("`fit_measures` must be NULL or a character vector.")
    }
    fit_measures <- unique(trimws(fit_measures))
    fit_measures <- fit_measures[nzchar(fit_measures)]
  }

  mc <- match.call(expand.dots = FALSE)
  dots_raw <- list(...)
  .reject_exclusion_controls_in_dots(dots_raw, "loo.test")
  .reject_calibration_inputs_in_dots(dots_raw, "loo.test")

  if (is.null(dir.exp)) {
    de <- .dot_get(dots_raw, "dir.exp")
    if (is.null(de)) de <- .dot_get(dots_raw, "direxp")
    if (!is.null(de)) dir.exp <- de
  }

  if (is.null(include)) {
    inc <- .dot_get(dots_raw, "include")
    if (!is.null(inc)) include <- inc
  }

  solution_controls <- .resolve_solution_controls(
    solution = solution,
    include = include,
    dir.exp = dir.exp,
    caller = "loo.test",
    style = "std"
  )

  solution <- solution_controls$solution
  include <- solution_controls$include
  monitored_solutions <- solution_controls$monitored

  .validate_exclusion_controls(
    exclude_mode = exclude_mode,
    exclude_recompute = exclude_recompute,
    exclude_static = exclude_static,
    exclude_recompute_supplied = !is.null(mc$exclude_recompute),
    exclude_static_supplied = !is.null(mc$exclude_static),
    monitored_solutions = monitored_solutions,
    style = "std"
  )

  dots_split <- .split_truth_table_minimize_dots(dots_raw)
  dots_tt <- dots_split$tt
  dots_min <- dots_split$min

  analysis_vars <- NULL
  raw_data_work <- NULL

  if (calib == "recompute") {
    if (is.null(conditions)) {
      stop("When `calib = \"recompute\"`, `conditions` must be supplied explicitly.")
    }

    analysis_vars <- c(outcome, conditions)

    if (is.null(raw.data)) {
      stop("When `calib = \"recompute\"`, `raw.data` must be supplied.")
    }
    if (is.null(dim(raw.data)) || is.null(nrow(raw.data)) || nrow(raw.data) != nrow(data)) {
      stop("When `calib = \"recompute\"`, `raw.data` must be a data frame object with the same number of rows as `data`.")
    }

    raw_data_work <- .reduced_as_df(raw.data)
    calib_spec <- .reduced_validate_calib_spec(
      calib_spec = calib_spec,
      analysis_vars = analysis_vars,
      raw.data = raw_data_work,
      data = data
    )
  }

  .build_truth_table <- function(data_step) {
    args <- c(
      list(
        data = data_step,
        outcome = outcome,
        incl.cut = incl.cut,
        n.cut = n.cut
      ),
      dots_tt
    )

    if (!is.null(conditions)) {
      args$conditions <- conditions
    }

    do.call(QCA::truthTable, args)
  }

  .run_once <- function(data_step, raw_step = NULL) {
    .reduced_run_once(
      data_step = data_step,
      raw_step = raw_step,
      calib = calib,
      recalibrate_dataset = function(data_step, raw_step) {
        .reduced_recalibrate_dataset(
          data_step = data_step,
          raw_step = raw_step,
          analysis_vars = analysis_vars,
          calib_spec = calib_spec
        )
      },
      build_truth_table = .build_truth_table,
      monitored_solutions = monitored_solutions,
      exclude_mode = exclude_mode,
      exclude_recompute = exclude_recompute,
      exclude_static = exclude_static,
      dots_min = dots_min,
      dir.exp = dir.exp,
      which_M = which_M,
      i_mode = i_mode,
      fit_measures = fit_measures,
      include_analysis_data = FALSE
    )
  }

  case_ref <- .reduced_resolve_case_reference(cases = cases, data = data, case_labels = case_labels)

  total_cases <- nrow(case_ref)

  progress_state <- .new_qcaert_progress(total = total_cases, progress = progress)
  on.exit(progress_state$close(), add = TRUE)
  .bump_pb <- progress_state$tick

  baseline <- .run_once(
    data_step = data,
    raw_step = if (calib == "recompute") raw_data_work else NULL
  )

  baseline_solution_types <- .reduced_baseline_solution_types(
    base = baseline,
    monitored_solutions = monitored_solutions
  )
  baseline_fit_solution_types <- .reduced_baseline_solution_types(
    base = baseline,
    monitored_solutions = monitored_solutions,
    fit_measures = fit_measures,
    require_fit = TRUE
  )
  baseline_missing <- baseline$status == "ok" && length(baseline_solution_types) == 0L
  baseline_invalid <- baseline$status != "ok" || length(baseline_solution_types) == 0L

  diag_rows <- list()
  by_case <- list()

  for (ii in seq_len(nrow(case_ref))) {
    row_idx <- case_ref$row_index[ii]
    case_label_i <- case_ref$case_label[ii]
    key <- paste0(row_idx, ":", case_label_i)

    if (baseline_invalid) {
      status <- if (baseline$status != "ok") {
        .reduced_stop_reason_from_status(baseline$status, baseline = TRUE)
      } else if (baseline_missing) {
        "baseline_selected_solution_missing"
      } else {
        "baseline_no_comparable_solution"
      }

      diag_rows[[length(diag_rows) + 1L]] <- data.frame(
        row_index = row_idx,
        case_label = case_label_i,
        status = status,
        changed = NA,
        solution_changed = NA,
        changed_types = NA_character_,
        change_kind = NA_character_,
        fit_changed = NA,
        fit_changed_types = NA_character_,
        n_fit_deltas = NA_integer_,
        max_abs_fit_delta = NA_real_,
        error_source = baseline$error_source,
        error_message = baseline$error_message,
        stringsAsFactors = FALSE
      )

      by_case[[key]] <- list(
        row_index = row_idx,
        case_label = case_label_i,
        status = status,
        baseline = baseline,
        reduced = NULL,
        solution_changed = NA,
        changed_types = NA_character_,
        change_kind = NA_character_,
        fit_changed = NA,
        fit_changed_types = NA_character_,
        fit_changed_measures = character(0),
        fit_delta = numeric(0),
        n_fit_deltas = NA_integer_,
        max_abs_fit_delta = NA_real_,
        exclude_baseline = baseline$exclude_used,
        exclude_reduced = NULL,
        calibration_baseline = baseline$calibration,
        calibration_reduced = NULL,
        baseline_fit = baseline$fit,
        reduced_fit = NULL,
        error_source = baseline$error_source,
        error_message = baseline$error_message
      )

      .bump_pb()
      next
    }

    data_reduced <- data[-row_idx, , drop = FALSE]
    raw_reduced <- if (calib == "recompute") raw_data_work[-row_idx, , drop = FALSE] else NULL

    cur <- .run_once(
      data_step = data_reduced,
      raw_step = raw_reduced
    )

    comparable_solution_types <- .reduced_comparable_solution_types(
      base = baseline,
      cur = cur,
      monitored_solutions = baseline_solution_types
    )

    if (cur$status != "ok" || length(comparable_solution_types) == 0L) {
      status_info <- .reduced_no_comparable_status(
        cur = cur,
        candidate_solution_types = baseline_solution_types
      )
      status <- status_info$status

      diag_rows[[length(diag_rows) + 1L]] <- data.frame(
        row_index = row_idx,
        case_label = case_label_i,
        status = status,
        changed = NA,
        solution_changed = NA,
        changed_types = NA_character_,
        change_kind = NA_character_,
        fit_changed = NA,
        fit_changed_types = NA_character_,
        n_fit_deltas = NA_integer_,
        max_abs_fit_delta = NA_real_,
        error_source = status_info$error_source,
        error_message = status_info$error_message,
        stringsAsFactors = FALSE
      )

      by_case[[key]] <- list(
        row_index = row_idx,
        case_label = case_label_i,
        status = status,
        baseline = baseline,
        reduced = cur,
        solution_changed = NA,
        changed_types = NA_character_,
        change_kind = NA_character_,
        fit_changed = NA,
        fit_changed_types = NA_character_,
        fit_changed_measures = character(0),
        fit_delta = numeric(0),
        n_fit_deltas = NA_integer_,
        max_abs_fit_delta = NA_real_,
        exclude_baseline = baseline$exclude_used,
        exclude_reduced = cur$exclude_used,
        calibration_baseline = baseline$calibration,
        calibration_reduced = cur$calibration,
        baseline_fit = baseline$fit,
        reduced_fit = NULL,
        error_source = status_info$error_source,
        error_message = status_info$error_message
      )

      .bump_pb()
      next
    }

    sol_info <- .reduced_solution_change_info(
      base = baseline,
      cur = cur,
      monitored_solutions = baseline_solution_types
    )

    fit_info <- .reduced_fit_change_info(
      base = baseline,
      cur = cur,
      monitored_solutions = baseline_fit_solution_types,
      fit_tol = fit_tol,
      fit_measures = fit_measures
    )

    changed_any <- isTRUE(sol_info$changed) || isTRUE(fit_info$changed)

    diag_rows[[length(diag_rows) + 1L]] <- data.frame(
      row_index = row_idx,
      case_label = case_label_i,
      status = "ok",
      changed = changed_any,
      solution_changed = isTRUE(sol_info$changed),
      changed_types = sol_info$changed_types,
      change_kind = sol_info$change_kind,
      fit_changed = isTRUE(fit_info$changed),
      fit_changed_types = fit_info$changed_types,
      n_fit_deltas = as.integer(fit_info$n_changed),
      max_abs_fit_delta = fit_info$max_abs_delta,
      error_source = NA_character_,
      error_message = NA_character_,
      stringsAsFactors = FALSE
    )

    by_case[[key]] <- list(
      row_index = row_idx,
      case_label = case_label_i,
      status = "ok",
      baseline = baseline,
      reduced = cur,
      solution_changed = isTRUE(sol_info$changed),
      changed_types = sol_info$changed_types,
      change_kind = sol_info$change_kind,
      fit_changed = isTRUE(fit_info$changed),
      fit_changed_types = fit_info$changed_types,
      fit_changed_measures = fit_info$changed_measures,
      fit_delta = fit_info$delta,
      n_fit_deltas = as.integer(fit_info$n_changed),
      max_abs_fit_delta = fit_info$max_abs_delta,
      exclude_baseline = baseline$exclude_used,
      exclude_reduced = cur$exclude_used,
      calibration_baseline = baseline$calibration,
      calibration_reduced = cur$calibration,
      baseline_fit = baseline$fit,
      reduced_fit = cur$fit,
      error_source = NA_character_,
      error_message = NA_character_
    )

    .bump_pb()
  }

  diagnostics <- .bind_rows_result_sorted(diag_rows, order_by = "row_index")

  .new_result_object(
    "loo_test",
    diagnostics = diagnostics,
    results = .make_loo_results(diagnostics),
    baseline = baseline,
    by_case = by_case,
    settings = list(
      outcome = outcome,
      conditions = conditions,
      cases = cases,
      case_labels = case_labels,
      calib = calib,
      n_cases_tested = nrow(case_ref),
      incl.cut = incl.cut,
      n.cut = n.cut,
      solution = solution,
      monitored_solutions = monitored_solutions,
      include = include,
      dir.exp = dir.exp,
      which_M = which_M,
      i_mode = i_mode,
      exclude_mode = exclude_mode,
      exclude_recompute = exclude_recompute,
      exclude_static = exclude_static,
      fit_measures = fit_measures,
      fit_tol = fit_tol
    )
  )
}

#' Print a `loo_test` object
#'
#' @rdname loo.test
#' @export
print.loo_test <- function(x, row.names = FALSE, ...) {
  results <- x$results
  settings <- x$settings

  .print_qcaert_heading("loo_test", "leave-one-out robustness", settings)
  if (!is.null(settings$n_cases_tested)) {
    cat("Cases tested: ", settings$n_cases_tested, "\n", sep = "")
  }
  if (!is.null(settings$fit_measures) && length(settings$fit_measures) > 0L) {
    cat("Fit measures: ", paste(settings$fit_measures, collapse = ", "), "\n", sep = "")
  }

  .print_qcaert_table(results, "Result", row.names = row.names, ...)

  cat("\nSummary\n")
  if (is.data.frame(results) && nrow(results) > 0L) {
    n_ok <- sum(results$status == "ok", na.rm = TRUE)
    n_solution_changed <- .count_present(results$solution_change)
    n_fit_changed <- .count_present(results$fit_changed_types)
    n_errors <- sum(!is.na(results$status) & results$status != "ok", na.rm = TRUE)

    cat(" Successful runs: ", n_ok, "/", nrow(results), "\n", sep = "")
    cat(" Solution changes: ", n_solution_changed, "\n", sep = "")
    cat(" Fit changes: ", n_fit_changed, "\n", sep = "")
    cat(" Non-ok runs: ", n_errors, "\n", sep = "")

    if (n_solution_changed > 0L) {
      labs <- results$case_label[!is.na(results$solution_change)]
      cat(" Cases with solution changes: ", paste(labs, collapse = ", "), "\n", sep = "")
    }

    if (n_errors > 0L) {
      labs <- results$case_label[!is.na(results$status) & results$status != "ok"]
      cat(" Non-ok cases: ", paste(labs, collapse = ", "), "\n", sep = "")
    }
  }

  invisible(x)
}

#' Return the main results table from a `loo_test` object
#'
#' @rdname loo.test
#' @export
as.data.frame.loo_test <- function(x, ...) {
  .as.data.frame_results(x, ...)
}
