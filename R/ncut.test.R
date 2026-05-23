#' Truth table frequency-cutoff robustness test for QCA solutions
#'
#' Perturbs the truth table frequency cutoff used in the analysis and checks
#' when the monitored QCA solution changes. Starting from a baseline `n.cut`,
#' the function searches downward and upward in fixed integer steps and records
#' the last value that preserved the baseline solution, the first value that
#' changed the solution or triggered an error, and the reason the search
#' stopped in each direction.
#'
#' @param data A non-empty data frame containing the outcome and condition
#'   columns used in the QCA analysis.
#' @param outcome Name of the outcome. This must be a single
#'   non-empty character string.
#' @param conditions Optional character vector of condition names. If `NULL`,
#'   the condition set is left to [QCA::truthTable()].
#' @param n.cut Baseline truth table frequency cutoff passed to
#'   [QCA::truthTable()]. This must be an integer-like value of at least `1`.
#' @param step Positive integer step size used to move `n.cut` downward and
#'   upward from the baseline value.
#' @param max_steps Maximum number of stepwise moves to attempt in each
#'   direction.
#' @param incl.cut Inclusion cutoff passed to [QCA::truthTable()]. This must
#'   be a single finite number in `[0, 1]`.
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
#'   recalculates exclusions from each truth table, `"static"` reuses
#'   `exclude_static`, and `"none"` does not use exclusions.
#' @param exclude_recompute Named list of arguments passed to [QCA::findRows()]
#'   when `exclude_mode = "recompute"`.
#' @param exclude_static Already computed exclusion object reused when
#'   `exclude_mode = "static"`.
#' @param result_shape Layout of the clean `results` table when
#'   `solution = "all"`. `"wide"` keeps one row per direction with
#'   solution-type-specific columns such as `con_last_safe` and `par_reason`.
#'   `"long"` returns one row per direction and solution type, with a `solution_type`
#'   column. Single-solution-type calls keep the compact single-solution-type
#'   layout.
#' @param progress Logical; if `TRUE` and the session is interactive, show a
#'   text progress bar.
#' @param x An `ncut_test` object returned by [ncut.test()].
#' @param row.names Logical; passed to [print.data.frame()] by
#'   [print.ncut_test()].
#' @param ... Additional arguments routed through the QCA workflow. Arguments
#'   matching [QCA::truthTable()] are forwarded to truth table construction;
#'   remaining minimization arguments are filtered and forwarded to
#'   [QCA::minimize()]. The function also looks in `...` for `include`,
#'   `dir.exp`, or `direxp` if those arguments were not supplied explicitly.
#'   In [print.ncut_test()], `...` is passed to [print.data.frame()]. In
#'   [as.data.frame.ncut_test()], `...` is ignored.
#'
#' @returns An object of class `ncut_test` with the following components:
#'   \describe{
#'     \item{`diagnostics`}{A detailed data frame with one row for the lower
#'     search and one row for the upper search. When `solution = "all"`, each
#'     monitored solution type is searched independently, so diagnostics has
#'     one row per direction and solution type. It includes the baseline
#'     `n.cut`, the last safe value, the first failing value, the number of
#'     successful steps, stop reason, change classification, exclusion
#'     information for the baseline, last safe, and first failing runs, and
#'     the computed upper limit.}
#'     \item{`results`}{A compact data frame with the columns
#'     `direction`, `start`, `last_safe`, `first_failing`, `steps`,
#'     `total_delta`, and `reason`. When `solution = "all"` and
#'     `result_shape = "wide"`, the row unit remains `direction`, but the
#'     boundary and reason columns are solution-type-specific, using prefixes `con_`,
#'     `par_`, and, when available, `int_`. When `result_shape = "long"`,
#'     `results` has one row per direction and solution type.}
#'     \item{`bounds`}{A named integer vector with `Lower` and `Upper`
#'     elements taken from the last safe values in each search direction. When
#'     `solution = "all"`, this is a lower/upper matrix with one column per
#'     monitored solution type.}
#'     \item{`baseline`}{A list containing the baseline truth table,
#'     minimization results, selected solution terms used for comparison,
#'     exclusion set used, and
#'     status information.}
#'     \item{`by_direction`}{A named list with one entry for the lower search
#'     and one for the upper search, each containing the search trace and
#'     stopping information.}
#'     \item{`settings`}{A list containing the analysis settings used to build
#'     the result object, including the computed upper limit.}
#'   }
#'
#'   `print.ncut_test()` prints a concise summary and the `results` table.
#'   `as.data.frame.ncut_test()` returns the `results` table.
#'
#' @details
#' The baseline analysis is built with [QCA::truthTable()] using the supplied
#' `incl.cut` and `n.cut`, followed by minimization under the requested
#' solution type. The function then tests lower and upper values of `n.cut`
#' one step at a time. When `solution = "all"`, conservative, parsimonious,
#' and, when requested, intermediate paths are searched independently.
#'
#' The search range is bounded below by `1` and above by the number of rows in
#' `data`.
#'
#' A directional search stops when one of the following occurs:
#' \itemize{
#'   \item the next tested value falls outside the allowed range,
#'   \item truth table construction fails,
#'   \item exclusion recomputation fails,
#'   \item minimization fails, or
#'   \item the monitored solution changes relative to the baseline.
#' }
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
#' out <- ncut.test(
#'   data = dat,
#'   outcome = outcome,
#'   conditions = conditions,
#'   n.cut = 1,
#'   step = 1,
#'   max_steps = 4,
#'   incl.cut = 0.8,
#'   solution = "all",
#'   dir.exp = dir_exp,
#'   result_shape = "long",
#'   progress = TRUE
#' )
#'
#' out
#' as.data.frame(out)
#' }
#'
#' @seealso [calib.test()], [incl.test()], [loo.test()], [subsample.test()],
#'   [altset.test()], [theory.test()], [cluster.test()], [sol.df()]
#' @export
ncut.test <- function(
    data,
    outcome,
    conditions = NULL,
    n.cut = 1,
    step = 1,
    max_steps = 20,
    incl.cut = 1,
    solution = "all",
    include = NULL,
    dir.exp = NULL,
    which_M = 1,
    i_mode = c("all", "C1P1"),
    exclude_mode = c("recompute", "static", "none"),
    exclude_recompute = list(type = 2),
    exclude_static = NULL,
    result_shape = c("wide", "long"),
    progress = TRUE,
    ...
) {
  .require_qca()

  selection_controls <- .normalize_i_mode(i_mode)
  i_mode <- selection_controls$i_mode
  exclude_mode <- match.arg(exclude_mode)
  result_shape <- match.arg(result_shape)

  if (is.null(dim(data)) || is.null(nrow(data)) || nrow(data) < 1L) {
    stop("`data` must be a non-empty data frame object with at least one row.")
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
  step <- .as_integerish_scalar(step, "step", min = 1L)
  max_steps <- .as_integerish_scalar(max_steps, "max_steps", min = 1L)
  which_M <- .as_integerish_scalar(which_M, "which_M", min = 1L)

  mc <- match.call(expand.dots = FALSE)
  dots_raw <- list(...)
  .reject_exclusion_controls_in_dots(dots_raw, "ncut.test")

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
    caller = "ncut.test",
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

  .build_truth_table <- function(cutoff) {
    args <- c(
      list(
        data = data,
        outcome = outcome,
        incl.cut = incl.cut,
        n.cut = cutoff
      ),
      dots_tt
    )

    if (!is.null(conditions)) {
      args$conditions <- conditions
    }

    do.call(QCA::truthTable, args)
  }

  .run_once <- function(cutoff, solution_types = monitored_solutions) {
    .boundary_run_once(
      value = cutoff,
      build_truth_table = .build_truth_table,
      monitored_solutions = solution_types,
      exclude_mode = exclude_mode,
      exclude_recompute = exclude_recompute,
      exclude_static = exclude_static,
      dots_filtered = dots_min,
      dir.exp = dir.exp,
      which_M = which_M,
      i_mode = i_mode
    )
  }

  upper_limit <- as.integer(nrow(data))

  progress_total <- if (solution == "all") 2L * length(monitored_solutions) else 2L
  progress_state <- .new_qcaert_progress(total = progress_total, progress = progress)
  on.exit(progress_state$close(), add = TRUE)
  .bump_pb <- progress_state$tick

  searched <- .boundary_search(
    value_name = "n.cut",
    value_type = "integer",
    start_value = n.cut,
    step = step,
    max_steps = max_steps,
    lower_limit = 1L,
    upper_limit = upper_limit,
    run_once = .run_once,
    solution = solution,
    monitored_solutions = monitored_solutions,
    i_mode = i_mode,
    which_M = which_M,
    progress_tick = .bump_pb,
    extra_row = list(upper_limit = upper_limit),
    extra_path = list(upper_limit = upper_limit)
  )
  diagnostics <- searched$diagnostics
  baseline <- searched$baseline
  by_direction <- searched$by_direction

  bounds <- .boundary_bounds(diagnostics, "n.cut")

  .new_result_object(
    "ncut_test",
    diagnostics = diagnostics,
    results = .make_boundary_results(diagnostics, "n.cut", result_shape = result_shape),
    bounds = bounds,
    baseline = baseline,
    by_direction = by_direction,
    settings = list(
      outcome = outcome,
      conditions = conditions,
      n.cut = n.cut,
      step = step,
      max_steps = max_steps,
      incl.cut = incl.cut,
      solution = solution,
      monitored_solutions = monitored_solutions,
      include = include,
      dir.exp = dir.exp,
      which_M = which_M,
      i_mode = i_mode,
      exclude_mode = exclude_mode,
      exclude_recompute = exclude_recompute,
      exclude_static = exclude_static,
      result_shape = result_shape,
      upper_limit = upper_limit
    )
  )
}

#' Print an `ncut_test` object
#'
#' @rdname ncut.test
#' @export
print.ncut_test <- function(x, row.names = FALSE, ...) {
  results <- x$results
  settings <- x$settings

  .print_qcaert_heading("ncut_test", "n.cut robustness", settings)
  if (is.data.frame(results) && nrow(results) > 0L) {
    cat("Starting n.cut: ", format(results$start[1L], trim = TRUE), "\n", sep = "")
  }
  if (!is.null(settings$step)) {
    cat("Step size: ", format(settings$step, trim = TRUE), "\n", sep = "")
  }
  if (!is.null(settings$max_steps)) {
    cat("Max steps: ", settings$max_steps, "\n", sep = "")
  }
  if (!is.null(settings$upper_limit)) {
    cat("Upper limit: ", settings$upper_limit, "\n", sep = "")
  }

  .print_qcaert_table(results, "Result", row.names = row.names, ...)
  .print_boundary_summary(x)

  invisible(x)
}

#' Return the main results table from an `ncut_test` object
#'
#' @rdname ncut.test
#' @export
as.data.frame.ncut_test <- function(x, ...) {
  .as.data.frame_results(x, ...)
}
