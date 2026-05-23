#' Alternative-set robustness test for QCA solutions
#'
#' Repeatedly generates alternative analyses by jointly varying calibration
#' anchors, the raw consistency threshold, and the frequency cutoff for truth
#' table rows, then reruns the QCA analysis for each generated specification.
#' For each specification, the function records whether the solution under
#' evaluation changes, whether parameters of fit change, which conditions or
#' outcome were recalibrated, and whether the analysis failed to produce a
#' solution.
#'
#' @param raw.data A data frame object containing the raw, uncalibrated
#'   condition columns, and the raw outcome column when `test.outcome = TRUE`.
#' @param calib.data A data frame object containing the calibrated outcome
#'   and calibrated condition columns used for the baseline analysis.
#' @param outcome Name of the outcome column in `calib.data`. This must be a
#'   single non-empty character string.
#' @param conditions Character vector of calibrated condition names in
#'   `calib.data`.
#' @param calib_spec Named list of calibration specifications, one per
#'   condition, plus one for the outcome when `test.outcome = TRUE`. Each entry
#'   must contain `raw`, `type`, and `thresholds`, may contain `method`, and may
#'   contain `calibrate`, a named list of additional [QCA::calibrate()]
#'   arguments for that set.
#' @param test.conditions Subset of `conditions` to perturb when generating
#'   random alternative calibration draws. May be `NULL` only when
#'   `test.outcome = TRUE`, in which case only the outcome calibration is
#'   perturbed.
#' @param test.outcome Logical; if `TRUE`, also perturb the outcome calibration.
#'   The outcome must be represented in `calib_spec`, and must not be included
#'   in `conditions`.
#' @param anchors_to_test Character vector of anchors eligible for perturbation,
#'   or `NULL` to use all anchors implied by each tested set's calibration
#'   method. Crisp sets use `"T"`. Fuzzy direct three-threshold sets use
#'   `"E"`, `"C"`, and `"I"`. Fuzzy indirect sets use `"T1"`,
#'   `"T2"`, and so on. Fuzzy direct six-threshold sets use `"E1"`,
#'   `"C1"`, `"I1"`, `"I2"`, `"C2"`, and `"E2"`.
#' @param solution Solution type to monitor. Accepted values are `"all"`,
#'   `"con"` or `"conservative"`, `"par"` or `"parsimonious"`, and `"int"` or
#'   `"intermediate"`.
#' @param include Optional minimization include setting. Currently, this
#'   argument accepts only `NULL`, `""`, or `"?"`.
#' @param which_M Positive integer giving which solution alternative to use
#'   when minimization returns multiple models.
#' @param unit_step Numeric step size used to move calibration thresholds when
#'   building candidate alternative draws. Supply either one value applied to
#'   all calibrated sets or one value per condition, plus the outcome when
#'   `test.outcome = TRUE`. If `NULL`, qcaERT computes scale-aware steps from
#'   each set's threshold spacing.
#' @param unit_step_divisor Positive number used to compute `unit_step`
#'   automatically when `unit_step = NULL`. The default is `10`.
#' @param calib_max_steps Maximum number of threshold moves away from the
#'   baseline used when building alternative calibration candidates.
#' @param incl.cut Baseline inclusion cutoff passed to [QCA::truthTable()].
#' @param incl_step Positive step size used to build the candidate grid of
#'   inclusion-cutoff values around `incl.cut`.
#' @param incl_max_steps Maximum number of stepwise moves away from the
#'   baseline `incl.cut` used to build the candidate grid.
#' @param n.cut Baseline truth table frequency cutoff passed to
#'   [QCA::truthTable()].
#' @param ncut_step Positive integer step size used to build the candidate grid
#'   of frequency-cutoff values around `n.cut`.
#' @param ncut_max_steps Maximum number of stepwise moves away from the
#'   baseline `n.cut` used to build the candidate grid.
#' @param dir.exp Directional expectations used when the monitored solution is
#'   intermediate.
#' @param i_mode Character string controlling intermediate-solution selection.
#'   Accepted values are `"all"` and `"C1P1"`.
#' @param exclude_mode Character string controlling how excluded rows are
#'   handled for parsimonious and intermediate minimization. `"recompute"`
#'   recalculates exclusions from each drawn truth table using
#'   `exclude_recompute`, `"static"` reuses `exclude_static`, and `"none"`
#'   does not use exclusions.
#' @param exclude_recompute Named list of arguments passed to [QCA::findRows()]
#'   when `exclude_mode = "recompute"`.
#' @param exclude_static Already computed exclusion object reused when
#'   `exclude_mode = "static"`.
#' @param n_draws Number of random alternative draws to run.
#' @param fit_tol Non-negative tolerance used when deciding whether fit values
#'   changed.
#' @param seed Optional integer seed passed to [set.seed()] before generating
#'   the random draws.
#' @param progress Logical; if `TRUE` and the session is interactive, show a
#'   text progress bar.
#' @param verbose Logical; if `TRUE`, print a completion message after each
#'   draw.
#' @param x An `altset_test` object returned by [altset.test()].
#' @param row.names Logical; passed to [print.data.frame()] by
#'   [print.altset_test()].
#' @param ... Additional arguments routed through the QCA workflow. Arguments
#'   matching [QCA::truthTable()] are forwarded to truth table construction;
#'   remaining minimization arguments are filtered and forwarded to
#'   [QCA::minimize()]. The function also looks in `...` for `include`,
#'   `dir.exp`, or `direxp` if those arguments were not supplied explicitly.
#'   In [print.altset_test()], `...` is passed to [print.data.frame()]. In
#'   [as.data.frame.altset_test()], `...` is ignored.
#'
#' @returns An object of class `altset_test` with the following components:
#'   \describe{
#'     \item{`summary`}{A list with summary values named `n_draws`,
#'     `n_same_solution`, `n_fit_compared`, `n_same_fit`,
#'     `score_solution`, `score_fit`, `score_total`,
#'     `score_solution_by_solution_type`, and `score_fit_by_solution_type`.}
#'     \item{`baseline`}{A list containing the baseline analysis, baseline draw
#'     metadata, exclusion information, selected solution terms used for
#'     comparison, fit information, and status information.}
#'     \item{`diagnostics`}{A detailed data frame with one row per draw. It
#'     includes the sampled `incl.cut` and `n.cut`, run status,
#'     solution-change information, fit-change information, changed-set
#'     information, and error information when a draw fails.}
#'     \item{`results`}{A compact data frame with the columns
#'     `draw`, `incl.cut`, `n.cut`, `status`, `n_changed_sets`,
#'     `changed_sets`, `changed_roles`, `solution_change`, `fit_changed_types`,
#'     `n_fit_deltas`, and `max_abs_fit_delta`.}
#'     \item{`by_draw`}{A named list with one entry per draw. Each entry stores
#'     the full run result, solution-comparison information, fit-comparison
#'     information, and draw-level fit details.}
#'     \item{`settings`}{A list containing the analysis settings used to build
#'     the result object.}
#'   }
#'
#'   `print.altset_test()` prints a concise summary, the `results` table, and
#'   solution-type-specific match-rate tables. `as.data.frame.altset_test()` returns
#'   the `results` table.
#'
#' @details
#' The function first runs a baseline analysis using `calib.data`,
#' `incl.cut`, and `n.cut`. It then generates `n_draws` random alternative
#' draws.
#'
#' Each draw samples:
#' \itemize{
#'   \item one admissible inclusion cutoff from the grid defined by
#'   `incl.cut`, `incl_step`, and `incl_max_steps`,
#'   \item one admissible frequency cutoff from the grid defined by `n.cut`,
#'   `ncut_step`, and `ncut_max_steps`, and
#'   \item one admissible alternative calibration specification sampled from
#'   `calib_spec` by moving method-specific anchors for `test.conditions`, and
#'   for the outcome when `test.outcome = TRUE`, by integer multiples of each
#'   set's `unit_step`.
#' }
#'
#' The function rejects a sampled draw if it is identical to the baseline on
#' all three dimensions and resamples until it gets a non-baseline draw or
#' reaches the internal retry limit.
#'
#' The shared solution-control, exclusion, calibration-specification, and
#' returned-object conventions are described in `?qcaERT_conventions`.
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
#' calib_spec <- list(
#'   DEV = list(raw = "DEV", type = "fuzzy", method = "direct", thresholds = thresholds$DEV),
#'   URB = list(raw = "URB", type = "fuzzy", method = "direct", thresholds = thresholds$URB),
#'   LIT = list(raw = "LIT", type = "fuzzy", method = "direct", thresholds = thresholds$LIT),
#'   IND = list(raw = "IND", type = "fuzzy", method = "direct", thresholds = thresholds$IND),
#'   STB = list(raw = "STB", type = "fuzzy", method = "direct", thresholds = thresholds$STB)
#' )
#'
#' calib_spec_outcome <- calib_spec
#' calib_spec_outcome$SURV <- list(
#'   raw = "SURV",
#'   type = "fuzzy",
#'   method = "direct",
#'   thresholds = thresholds$SURV
#' )
#'
#' # Common use: sample alternative calibration, incl.cut, and n.cut settings.
#' altset_out <- altset.test(
#'   raw.data = LR,
#'   calib.data = dat,
#'   outcome = outcome,
#'   conditions = conditions,
#'   calib_spec = calib_spec,
#'   test.conditions = c("DEV", "URB"),
#'   unit_step = NULL,
#'   unit_step_divisor = 10,
#'   calib_max_steps = 5,
#'   incl.cut = 0.8,
#'   incl_step = 0.02,
#'   incl_max_steps = 5,
#'   n.cut = 1,
#'   ncut_step = 1,
#'   ncut_max_steps = 2,
#'   n_draws = 50,
#'   seed = 123,
#'   solution = "all",
#'   dir.exp = dir_exp,
#'   progress = TRUE
#' )
#'
#' altset_out
#' as.data.frame(altset_out)
#' altset_out$summary
#'
#' # Special case: include the calibrated outcome among sampled calibration
#' # perturbations. The outcome stays out of conditions.
#' altset_outcome <- altset.test(
#'   raw.data = LR,
#'   calib.data = dat,
#'   outcome = outcome,
#'   conditions = conditions,
#'   calib_spec = calib_spec_outcome,
#'   test.conditions = c("DEV", "URB"),
#'   test.outcome = TRUE,
#'   unit_step = NULL,
#'   unit_step_divisor = 10,
#'   calib_max_steps = 5,
#'   incl.cut = 0.8,
#'   incl_step = 0.02,
#'   incl_max_steps = 5,
#'   n.cut = 1,
#'   ncut_step = 1,
#'   ncut_max_steps = 2,
#'   n_draws = 50,
#'   seed = 456,
#'   solution = "all",
#'   dir.exp = dir_exp,
#'   progress = TRUE
#' )
#'
#' as.data.frame(altset_outcome)
#' }
#'
#' @seealso [calib.test()], [incl.test()], [ncut.test()], [loo.test()],
#'   [subsample.test()], [theory.test()], [cluster.test()], [sol.df()]
#' @export
altset.test <- function(
    raw.data,
    calib.data,
    outcome,
    conditions,
    calib_spec,
    test.conditions = conditions,
    test.outcome = FALSE,
    anchors_to_test = NULL,
    solution = "all",
    include = NULL,
    which_M = 1,
    unit_step = NULL,
    unit_step_divisor = 10,
    calib_max_steps = 20,
    incl.cut = 1,
    incl_step = 0.01,
    incl_max_steps = 20,
    n.cut = 1,
    ncut_step = 1,
    ncut_max_steps = 20,
    dir.exp = NULL,
    i_mode = c("all", "C1P1"),
    exclude_mode = c("recompute", "static", "none"),
    exclude_recompute = list(type = 2),
    exclude_static = NULL,
    n_draws = 100,
    fit_tol = 1e-6,
    seed = NULL,
    progress = TRUE,
    verbose = FALSE,
    ...
) {
  .require_qca()

  selection_controls <- .normalize_i_mode(i_mode)
  i_mode <- selection_controls$i_mode
  exclude_mode <- match.arg(exclude_mode)

  altset <- match.call(expand.dots = FALSE)
  dots_raw <- list(...)
  caller_env <- parent.frame()
  .reject_exclusion_controls_in_dots(dots_raw, "altset.test")
  .reject_calibration_inputs_in_dots(dots_raw, "altset.test")

  # basic checks --
  if (is.null(dim(raw.data)) || is.null(nrow(raw.data)) || nrow(raw.data) < 1L) {
    stop("`raw.data` must be a non-empty data frame object with at least one row.")
  }
  if (is.null(dim(calib.data)) || is.null(nrow(calib.data)) || nrow(calib.data) < 1L) {
    stop("`calib.data` must be a non-empty data frame object with at least one row.")
  }
  if (nrow(raw.data) != nrow(calib.data)) {
    stop("`raw.data` and `calib.data` must have the same number of rows.")
  }
  if (!is.character(outcome) || length(outcome) != 1L || !nzchar(outcome)) {
    stop("`outcome` must be a single non-empty character string.")
  }
  if (!outcome %in% colnames(calib.data)) {
    stop("`outcome` must exist in `calib.data`.")
  }
  if (!is.character(conditions) || length(conditions) < 1L) {
    stop("`conditions` must be a non-empty character vector.")
  }
  if (!all(conditions %in% colnames(calib.data))) {
    stop("All `conditions` must exist in `calib.data`.")
  }
  .validate_outcome_conditions_distinct(outcome, conditions)

  test.outcome <- .normalize_test_outcome(test.outcome)
  test.conditions <- .normalize_test_conditions(
    test.conditions = test.conditions,
    conditions = conditions,
    test.outcome = test.outcome
  )
  if (!is.numeric(incl.cut) || length(incl.cut) != 1L || !is.finite(incl.cut) ||
      incl.cut < 0 || incl.cut > 1) {
    stop("`incl.cut` must be a single finite number in [0, 1].")
  }
  if (!is.numeric(incl_step) || length(incl_step) != 1L || !is.finite(incl_step) || incl_step <= 0) {
    stop("`incl_step` must be a single finite number > 0.")
  }
  if (!is.numeric(fit_tol) || length(fit_tol) != 1L || !is.finite(fit_tol) || fit_tol < 0) {
    stop("`fit_tol` must be a single finite number >= 0.")
  }
  n_draws <- .as_integerish_scalar(n_draws, "n_draws", min = 1L)

  n.cut <- .as_integerish_scalar(n.cut, "n.cut", min = 1L)
  ncut_step <- .as_integerish_scalar(ncut_step, "ncut_step", min = 1L)
  ncut_max_steps <- .as_integerish_scalar(ncut_max_steps, "ncut_max_steps", min = 1L)
  incl_max_steps <- .as_integerish_scalar(incl_max_steps, "incl_max_steps", min = 1L)
  calib_max_steps <- .as_integerish_scalar(calib_max_steps, "calib_max_steps", min = 1L)

  # minimize-style conveniences from calib.test -
  if (!is.null(altset$outcome)) {
    expr <- altset$outcome
    val <- .safe_eval_expr(expr, envir = caller_env)
    if (!inherits(val, "error") && is.character(val) && length(val) == 1) {
      outcome <- val
    } else {
      outcome <- .expr_to_chrvec(expr)
      if (length(outcome) != 1) stop("outcome must resolve to a single outcome name.")
    }
  }

  if (!is.null(altset$conditions)) {
    expr <- altset$conditions
    val <- .safe_eval_expr(expr, envir = caller_env)
    if (!inherits(val, "error") && is.character(val) && length(val) >= 1) {
      conditions <- val
    } else {
      conditions <- .expr_to_chrvec(expr)
    }
  }

  if (!is.null(altset[["dir.exp"]])) {
    expr <- altset[["dir.exp"]]
    val <- .safe_eval_expr(expr, envir = caller_env)
    if (!inherits(val, "error")) {
      dir.exp <- val
    } else {
      dir.exp <- .expr_to_chrvec(expr)
    }
  }

  dir.exp <- .normalize_dir_exp_generic(dir.exp, conditions, endpoint_phrase = "`conditions`")

  if (is.null(dir.exp)) {
    de <- .dot_get(dots_raw, "dir.exp")
    if (is.null(de)) de <- .dot_get(dots_raw, "direxp")
    if (!is.null(de)) dir.exp <- de
  }

  dir.exp <- .normalize_dir_exp_generic(dir.exp, conditions, endpoint_phrase = "`conditions`")

  if (is.null(include)) {
    inc <- .dot_get(dots_raw, "include")
    if (!is.null(inc)) include <- inc
  }

  solution_controls <- .resolve_solution_controls(
    solution = solution,
    include = include,
    dir.exp = dir.exp,
    caller = "altset.test",
    style = "plain"
  )

  solution <- solution_controls$solution
  include <- solution_controls$include
  monitored_solutions <- solution_controls$monitored

  .validate_exclusion_controls(
    exclude_mode = exclude_mode,
    exclude_recompute = exclude_recompute,
    exclude_static = exclude_static,
    exclude_recompute_supplied = !is.null(altset$exclude_recompute),
    exclude_static_supplied = !is.null(altset$exclude_static),
    monitored_solutions = monitored_solutions,
    style = "plain"
  )

  .err_msg <- function(e) {
    if (!inherits(e, "error")) return(NA_character_)
    tryCatch(conditionMessage(e), error = function(...) as.character(e))
  }

  # fit helpers aligned to selected solution keys -
  .extract_standard_fit_map <- function(res, which_M) {
    sol_all <- if (!is.null(res) && !inherits(res, "error")) res$solution else NULL
    primes <- if (!is.null(res) && !inherits(res, "error")) res$primes else NULL
    ic_obj <- if (!is.null(res) && !inherits(res, "error")) res$IC else NULL

    nM <- if (!is.null(sol_all)) length(sol_all) else 0L
    out <- list()

    if (nM < which_M) return(out)

    key <- paste(.solution_terms_from_primes(sol_all[[which_M]], primes), collapse = "+")
    vec <- .fit_vec_from_ic(ic_obj, which_M)
    if (!is.null(vec)) out[[key]] <- vec

    out
  }

  .extract_intermediate_fit_map <- function(res, which_M, i_mode) {
    i.sol <- if (!is.null(res) && !inherits(res, "error")) res$i.sol else NULL
    if (is.null(i.sol) || length(i.sol) == 0L) return(list())

    nm <- .normalize_i_solution_names(i.sol)

    if (i_mode == "C1P1") {
      idx <- which(nm == "C1P1")
      if (length(idx) == 0L) return(list())
      i.sol <- i.sol[idx[1L]]
      nm <- nm[idx[1L]]
    }

    out <- list()

    for (k in seq_along(i.sol)) {
      el <- i.sol[[k]]
      sol_all <- if (is.list(el) && "solution" %in% names(el)) el$solution else NULL
      primes <- if (is.list(el) && "primes" %in% names(el)) el$primes else NULL
      ic_obj <- if (is.list(el) && "IC" %in% names(el)) el$IC else NULL

      nM <- if (!is.null(sol_all)) length(sol_all) else 0L

      if (nM < which_M) next

      key <- paste0(
        nm[k], ":",
        paste(.solution_terms_from_primes(sol_all[[which_M]], primes), collapse = "+")
      )
      vec <- .fit_vec_from_ic(ic_obj, which_M)
      if (!is.null(vec)) out[[key]] <- vec
    }

    out
  }

  .extract_solution_type_fit <- function(res, solution_type, which_M, i_mode) {
    if (solution_type %in% c("conservative", "parsimonious")) {
      return(list(
        raw = if (!is.null(res) && !inherits(res, "error")) res$IC else NULL,
        map = .extract_standard_fit_map(res, which_M = which_M)
        ))
    }
    list(
      raw = if (!is.null(res) && !inherits(res, "error")) lapply(res$i.sol, function(x) x$IC) else NULL,
      map = .extract_intermediate_fit_map(res, which_M = which_M, i_mode = i_mode)
      )
  }

  .compare_solution_type_fit <- function(base, cur, solution_type, tol) {
    base_map <- base$fit[[solution_type]]$map
    cur_map <- cur$fit[[solution_type]]$map

    .compare_fit_maps_hybrid(
      base_map = base_map,
      cur_map = cur_map,
      solution_type = solution_type,
      tol = tol
    )
  }

  # QCA run helpers -
  .build_truth_table <- function(data_step, incl_cutoff, n_cutoff) {
    args <- c(
      list(
        data = data_step,
        outcome = outcome,
        incl.cut = incl_cutoff,
        n.cut = n_cutoff
      ),
      dots_tt
    )

    if (!is.null(conditions)) {
      args$conditions <- conditions
    }

    do.call(QCA::truthTable, args)
  }

  .compute_exclude <- function(tt_obj) {
    .qcaert_compute_exclude(
      tt_obj = tt_obj,
      exclude_mode = exclude_mode,
      exclude_recompute = exclude_recompute,
      exclude_static = exclude_static
    )
  }

  .run_solution_type <- function(tt_obj, solution_type, ex, dots_filtered) {
    args <- c(
      list(
        input = tt_obj,
        include = if (solution_type == "conservative") "" else "?"
      ),
      dots_filtered
    )

    if (solution_type == "intermediate") {
      args$dir.exp <- dir.exp
    }

    if (solution_type %in% c("parsimonious", "intermediate") && !is.null(ex)) {
      args$exclude <- ex
    }

    res <- tryCatch(
      suppressWarnings(do.call(QCA::minimize, args)),
      error = function(e) e
    )

    if (inherits(res, "error")) {
      return(list(
        error = res,
        res = NULL,
        sig = NULL,
        fit = NULL,
        selected_solution_missing = NA,
        meta = NULL
      ))
    }

    sig_info <- .extract_solution_type_sig(
      res,
      solution_type = solution_type,
      which_M = which_M,
      i_mode = i_mode
    )

    fit_info <- .extract_solution_type_fit(
      res,
      solution_type = solution_type,
      which_M = which_M,
      i_mode = i_mode
    )

    list(
      error = NULL,
      res = res,
      sig = sig_info$sig,
      fit = fit_info,
      selected_solution_missing = isTRUE(sig_info$selected_solution_missing),
      meta = sig_info$meta
    )
  }

  .empty_solution_type_list <- function() {
    list(conservative = NULL, parsimonious = NULL, intermediate = NULL)
  }

  .empty_solution_type_status <- function() {
    c(conservative = NA_character_, parsimonious = NA_character_, intermediate = NA_character_)
  }

  .run_status_from_solution_types <- function(solution_type_status, solution_type_error_source, solution_type_error_message, monitored_solutions) {
    status_vec <- solution_type_status[monitored_solutions]

    if (any(status_vec == "ok", na.rm = TRUE)) {
      return(list(
        status = "ok",
        error_source = NA_character_,
        error_message = NA_character_
      ))
    }

    first_bad <- which(!is.na(status_vec) & nzchar(status_vec))[1L]

    if (is.na(first_bad)) {
      return(list(
        status = "run_error",
        error_source = NA_character_,
        error_message = NA_character_
      ))
    }

    solution_type <- monitored_solutions[first_bad]

    list(
      status = unname(solution_type_status[[solution_type]]),
      error_source = unname(solution_type_error_source[[solution_type]]),
      error_message = unname(solution_type_error_message[[solution_type]])
    )
  }

  .baseline_solution_types <- function(base, monitored_solutions) {
    if (is.null(base$solution_type_status)) {
      status_ok <- rep(identical(base$status, "ok"), length(monitored_solutions))
    } else {
      status_ok <- base$solution_type_status[monitored_solutions] == "ok"
      status_ok[is.na(status_ok)] <- FALSE
    }

    selected_solution_ok <- !(base$selected_solution_missing[monitored_solutions] %in% TRUE)
    selected_solution_ok[is.na(selected_solution_ok)] <- FALSE

    monitored_solutions[status_ok & selected_solution_ok]
  }

  .comparable_solution_types <- function(base, cur, monitored_solutions) {
    if (is.null(base$solution_type_status) || is.null(cur$solution_type_status)) {
      if (!identical(base$status, "ok") || !identical(cur$status, "ok")) {
        return(character(0))
      }
      return(monitored_solutions)
    }

    base_ok <- base$solution_type_status[monitored_solutions] == "ok"
    cur_ok <- cur$solution_type_status[monitored_solutions] == "ok"
    base_ok[is.na(base_ok)] <- FALSE
    cur_ok[is.na(cur_ok)] <- FALSE

    monitored_solutions[base_ok & cur_ok]
  }

  .no_comparable_status <- function(cur, candidate_solution_types) {
    if (!identical(cur$status, "ok")) {
      return(list(
        status = cur$status,
        error_source = cur$error_source,
        error_message = cur$error_message
      ))
    }

    if (!is.null(cur$solution_type_status)) {
      bad <- candidate_solution_types[cur$solution_type_status[candidate_solution_types] != "ok"]
      bad <- bad[!is.na(bad)]

      if (length(bad) > 0L) {
        solution_type <- bad[1L]
        return(list(
          status = cur$solution_type_status[[solution_type]],
          error_source = cur$solution_type_error_source[[solution_type]],
          error_message = cur$solution_type_error_message[[solution_type]]
        ))
      }
    }

    list(
      status = "no_comparable_solution",
      error_source = NA_character_,
      error_message = NA_character_
    )
  }

  .run_once <- function(data_step, incl_cutoff, n_cutoff, draw_meta) {
    tt_obj <- tryCatch(
      suppressWarnings(.build_truth_table(data_step, incl_cutoff = incl_cutoff, n_cutoff = n_cutoff)),
      error = function(e) e
    )

    if (inherits(tt_obj, "error")) {
      solution_type_status <- .empty_solution_type_status()
      solution_type_status[monitored_solutions] <- "truth_table_error"
      solution_type_error_source <- .empty_solution_type_status()
      solution_type_error_source[monitored_solutions] <- "truthTable"
      solution_type_error_message <- .empty_solution_type_status()
      solution_type_error_message[monitored_solutions] <- .err_msg(tt_obj)

      return(list(
        status = "truth_table_error",
        data = data_step,
        tt = NULL,
        exclude_used = NULL,
        res = .empty_solution_type_list(),
        sig = .empty_solution_type_list(),
        fit = .empty_solution_type_list(),
        selected_solution_missing = c(conservative = NA, parsimonious = NA, intermediate = NA),
        meta = .empty_solution_type_list(),
        solution_type_status = solution_type_status,
        solution_type_error_source = solution_type_error_source,
        solution_type_error_message = solution_type_error_message,
        error_source = "truthTable",
        error_message = .err_msg(tt_obj),
        draw_meta = draw_meta
      ))
    }

    need_exclude <- any(monitored_solutions %in% c("parsimonious", "intermediate"))

    ex <- NULL
    ex_error <- NULL
    if (need_exclude) {
      ex <- .compute_exclude(tt_obj)

      if (inherits(ex, "error")) {
        ex_error <- ex
        ex <- NULL
      }
    }

    res_out <- .empty_solution_type_list()
    sig_out <- .empty_solution_type_list()
    fit_out <- .empty_solution_type_list()
    selected_solution_out <- c(conservative = NA, parsimonious = NA, intermediate = NA)
    meta_out <- .empty_solution_type_list()
    solution_type_status <- .empty_solution_type_status()
    solution_type_error_source <- .empty_solution_type_status()
    solution_type_error_message <- .empty_solution_type_status()

    for (solution_type in monitored_solutions) {
      if (solution_type %in% c("parsimonious", "intermediate") && inherits(ex_error, "error")) {
        solution_type_status[[solution_type]] <- "exclude_error"
        solution_type_error_source[[solution_type]] <- "exclude"
        solution_type_error_message[[solution_type]] <- .err_msg(ex_error)
        next
      }

      rr <- .run_solution_type(tt_obj, solution_type = solution_type, ex = ex, dots_filtered = dots_filtered)

      if (!is.null(rr$error)) {
        solution_type_status[[solution_type]] <- "minimize_error"
        solution_type_error_source[[solution_type]] <- solution_type
        solution_type_error_message[[solution_type]] <- .err_msg(rr$error)
        next
      }

      res_out[[solution_type]] <- rr$res
      sig_out[[solution_type]] <- rr$sig
      fit_out[[solution_type]] <- rr$fit
      selected_solution_out[[solution_type]] <- isTRUE(rr$selected_solution_missing)
      meta_out[[solution_type]] <- rr$meta
      solution_type_status[[solution_type]] <- "ok"
      solution_type_error_source[[solution_type]] <- NA_character_
      solution_type_error_message[[solution_type]] <- NA_character_
    }

    run_status <- .run_status_from_solution_types(
      solution_type_status = solution_type_status,
      solution_type_error_source = solution_type_error_source,
      solution_type_error_message = solution_type_error_message,
      monitored_solutions = monitored_solutions
    )

    list(
      status = run_status$status,
      data = data_step,
      tt = tt_obj,
      exclude_used = ex,
      res = res_out,
      sig = sig_out,
      fit = fit_out,
      selected_solution_missing = selected_solution_out,
      meta = meta_out,
      solution_type_status = solution_type_status,
      solution_type_error_source = solution_type_error_source,
      solution_type_error_message = solution_type_error_message,
      error_source = run_status$error_source,
      error_message = run_status$error_message,
      draw_meta = draw_meta
    )
  }

  .solution_change_info <- function(base, cur, monitored_solutions) {
    changed_flags <- vapply(
      monitored_solutions,
      function(solution_type) .sig_changed(base$sig[[solution_type]], cur$sig[[solution_type]]),
      logical(1)
    )

    changed <- any(changed_flags)
    changed_by_solution_type <- setNames(as.list(changed_flags), monitored_solutions)

    if (!changed) {
      return(list(
        changed = FALSE,
        changed_overall = FALSE,
        changed_types = NA_character_,
        change_kind = NA_character_,
        changed_by_solution_type = changed_by_solution_type
      ))
    }

    changed_types <- paste(monitored_solutions[changed_flags], collapse = ",")
    change_kind <- paste(
      vapply(
        monitored_solutions[changed_flags],
        function(solution_type) {
          paste0(solution_type, ":", .change_kind_sig(base$sig[[solution_type]], cur$sig[[solution_type]]))
        },
        character(1)
      ),
      collapse = ","
    )

    list(
      changed = TRUE,
      changed_overall = TRUE,
      changed_types = changed_types,
      change_kind = change_kind,
      changed_by_solution_type = changed_by_solution_type
    )
  }

  # materialize settings -
  which_M <- .coerce_which_M(which_M)
  calib_context <- .prepare_calib_context(
    conditions = conditions,
    outcome = outcome,
    calib_spec = calib_spec,
    test.outcome = test.outcome,
    raw.data = raw.data,
    unit_step = unit_step,
    unit_step_divisor = unit_step_divisor,
    anchors_to_test = anchors_to_test,
    caller = "altset.test"
  )
  calib_specs <- calib_context$calib_specs
  raw_targets <- calib_context$raw_targets
  type_fc <- calib_context$type_targets
  thr_list <- calib_context$thresholds_targets
  unit_step_vec <- calib_context$unit_step_targets
  anchors_to_test <- calib_context$anchors_to_test
  dots_split <- .split_truth_table_minimize_dots(dots_raw)
  dots_tt <- dots_split$tt
  dots_filtered <- dots_split$min

  tested_sets <- c(test.conditions, if (test.outcome) outcome else character(0))
  tested_roles <- c(rep("condition", length(test.conditions)), if (test.outcome) "outcome" else character(0))
  names(tested_roles) <- tested_sets

  incl_grid <- incl.cut + seq.int(-incl_max_steps, incl_max_steps) * incl_step
  incl_grid <- sort(unique(incl_grid[incl_grid >= 0 & incl_grid <= 1]))
  if (length(incl_grid) == 0L) {
    stop("No admissible `incl.cut` values were generated from `incl_step` and `incl_max_steps`.")
  }

  ncut_grid <- n.cut + seq.int(-ncut_max_steps, ncut_max_steps) * ncut_step
  ncut_grid <- as.integer(sort(unique(ncut_grid[ncut_grid >= 1L & ncut_grid <= nrow(calib.data)])))
  if (length(ncut_grid) == 0L) {
    stop("No admissible `n.cut` values were generated from `ncut_step` and `ncut_max_steps`.")
  }

  candidate_map <- Map(
    function(set_name, role) {
      .altset_build_calib_candidate(
        set_name = set_name,
        role = role,
        calib_specs = calib_specs,
        raw.data = raw.data,
        unit_step_vec = unit_step_vec,
        anchors_to_test = anchors_to_test,
        calib_max_steps = calib_max_steps
      )
    },
    tested_sets,
    tested_roles
  )
  names(candidate_map) <- tested_sets

  if (!is.null(seed)) set.seed(seed)

  # baseline -
  baseline_draw_meta <- list(
    draw = 0L,
    incl.cut = incl.cut,
    n.cut = n.cut,
    calibration = lapply(names(thr_list), function(nm) {
      list(
        set = nm,
        role = if (nm %in% conditions) "condition" else "outcome",
        raw = raw_targets[[nm]],
        type = type_fc[[nm]],
        method = calib_specs[[nm]]$method,
        thresholds_baseline = thr_list[[nm]],
        thresholds_draw = thr_list[[nm]],
        delta_steps = .calib_zero_delta_steps(calib_specs[[nm]]),
        changed = FALSE
      )
    })
  )
  names(baseline_draw_meta$calibration) <- names(thr_list)

  baseline <- .run_once(
    data_step = calib.data,
    incl_cutoff = incl.cut,
    n_cutoff = n.cut,
    draw_meta = baseline_draw_meta
  )

  baseline_solution_types <- .baseline_solution_types(
    base = baseline,
    monitored_solutions = monitored_solutions
  )

  if (length(baseline_solution_types) == 0L) {
    stop(
      "Baseline model is not valid for comparison. ",
      if (baseline$status != "ok") {
        paste0("Status: ", baseline$status, "; source: ", baseline$error_source, "; message: ", baseline$error_message)
      } else {
        "The selected baseline solution is missing."
      }
    )
  }

  # draws -
  progress_state <- .new_qcaert_progress(total = n_draws, progress = progress)
  on.exit(progress_state$close(), add = TRUE)

  diag_rows <- vector("list", n_draws)
  by_draw <- vector("list", n_draws)

  for (b in seq_len(n_draws)) {
    tries <- 0L

    repeat {
      tries <- tries + 1L

      incl_b <- sample(incl_grid, size = 1L)
      ncut_b <- sample(ncut_grid, size = 1L)
      calib_b <- .altset_sample_calibration_draw(
        candidate_map = candidate_map,
        calib.data = calib.data,
        raw.data = raw.data,
        thr_list = thr_list
      )

      any_change <- !isTRUE(all.equal(incl_b, incl.cut)) ||
        !identical(ncut_b, n.cut) ||
        isTRUE(calib_b$changed)

      if (any_change) break
      if (tries >= 1000L) {
        stop("Could not generate a non-baseline random draw after 1000 attempts.")
      }
    }

    draw_meta <- list(
      draw = b,
      incl.cut = incl_b,
      n.cut = ncut_b,
      calibration = calib_b$changes
    )

    cur <- .run_once(
      data_step = calib_b$data,
      incl_cutoff = incl_b,
      n_cutoff = ncut_b,
      draw_meta = draw_meta
    )

    comparable_solution_types <- .comparable_solution_types(
      base = baseline,
      cur = cur,
      monitored_solutions = baseline_solution_types
    )
    status_ok <- identical(cur$status, "ok") && length(comparable_solution_types) > 0L
    status_info <- if (status_ok) {
      list(
        status = "ok",
        error_source = NA_character_,
        error_message = NA_character_
      )
    } else {
      .no_comparable_status(
        cur = cur,
        candidate_solution_types = baseline_solution_types
      )
    }

    solution_cmp <- if (status_ok) {
      .solution_change_info(baseline, cur, monitored_solutions = comparable_solution_types)
    } else {
      list(
        changed = NA,
        changed_overall = NA,
        changed_types = NA_character_,
        change_kind = NA_character_,
        changed_by_solution_type = setNames(as.list(rep(NA, length(monitored_solutions))), monitored_solutions)
      )
    }

    fit_delta_by_solution_type <- setNames(as.list(rep(NA_real_, length(monitored_solutions))), monitored_solutions)
    fit_compared_by_solution_type <- setNames(as.list(rep(FALSE, length(monitored_solutions))), monitored_solutions)
    fit_keys_by_solution_type <- setNames(vector("list", length(monitored_solutions)), monitored_solutions)
    fit_details <- setNames(vector("list", length(monitored_solutions)), monitored_solutions)
    fit_changed_names_by_solution_type <- setNames(vector("list", length(monitored_solutions)), monitored_solutions)
    fit_delta_vec_by_solution_type <- setNames(vector("list", length(monitored_solutions)), monitored_solutions)

    if (status_ok) {
      for (solution_type in comparable_solution_types) {
        cmp <- .compare_solution_type_fit(baseline, cur, solution_type = solution_type, tol = fit_tol)
        fit_delta_by_solution_type[[solution_type]] <- cmp$max_abs_delta
        fit_compared_by_solution_type[[solution_type]] <- isTRUE(cmp$fit_compared)
        fit_keys_by_solution_type[[solution_type]] <- cmp$matched_keys
        fit_details[[solution_type]] <- cmp$details
        fit_changed_names_by_solution_type[[solution_type]] <- cmp$changed_names
        fit_delta_vec_by_solution_type[[solution_type]] <- cmp$delta
      }
    }

    compared_solution_types <- monitored_solutions[unlist(fit_compared_by_solution_type)]

    fit_changed_solution_types <- compared_solution_types[
      vapply(
        compared_solution_types,
        function(solution_type) length(fit_changed_names_by_solution_type[[solution_type]]) > 0L,
        logical(1)
      )
    ]

    fit_changed_measures <- if (length(compared_solution_types) > 0L) {
      sort(unique(unlist(fit_changed_names_by_solution_type[compared_solution_types], use.names = FALSE)))
    } else {
      character(0)
    }

    fit_delta <- if (length(compared_solution_types) > 0L) {
      unlist(fit_delta_vec_by_solution_type[compared_solution_types], use.names = TRUE)
    } else {
      numeric(0)
    }

    solution_changed <- if (status_ok) isTRUE(solution_cmp$changed_overall) else NA
    fit_changed <- if (status_ok && length(compared_solution_types) > 0L) length(fit_changed_measures) > 0L else NA
    changed_any <- if (status_ok) isTRUE(solution_changed) || isTRUE(fit_changed) else NA
    fit_changed_types <- if (status_ok && length(fit_changed_solution_types) > 0L) paste(fit_changed_solution_types, collapse = ",") else NA_character_
    n_fit_deltas <- if (status_ok) as.integer(length(fit_changed_measures)) else NA_integer_
    max_abs_fit_delta <- if (status_ok) .fit_abs_max(fit_delta) else NA_real_

    calib_changed <- Filter(function(x) isTRUE(x$changed), draw_meta$calibration)
    calib_changed_sets <- names(calib_changed)
    calib_changed_roles <- unique(vapply(calib_changed, function(x) x$role, character(1)))
    if (length(calib_changed_sets) == 0L) calib_changed_sets <- NA_character_
    if (length(calib_changed_roles) == 0L) calib_changed_roles <- NA_character_

    diag_rows[[b]] <- data.frame(
      draw = b,
      incl.cut = incl_b,
      n.cut = ncut_b,
      status = status_info$status,
      changed = changed_any,
      solution_changed = solution_changed,
      changed_types = solution_cmp$changed_types,
      change_kind = solution_cmp$change_kind,
      fit_compared = length(compared_solution_types) > 0L,
      fit_changed = fit_changed,
      fit_changed_types = fit_changed_types,
      n_fit_deltas = n_fit_deltas,
      max_abs_fit_delta = max_abs_fit_delta,
      error_source = status_info$error_source,
      error_message = status_info$error_message,
      changed_sets = paste(calib_changed_sets, collapse = ","),
      changed_roles = paste(calib_changed_roles, collapse = ","),
      n_changed_sets = sum(vapply(draw_meta$calibration, function(x) isTRUE(x$changed), logical(1))),
      stringsAsFactors = FALSE
    )
    diag_rows[[b]]$exclude_used <- I(list(cur$exclude_used))
    diag_rows[[b]]$calibration <- I(list(draw_meta$calibration))

    for (solution_type in monitored_solutions) {
      diag_rows[[b]][[paste0("status_", solution_type)]] <- cur$solution_type_status[[solution_type]]
      diag_rows[[b]][[paste0("error_source_", solution_type)]] <- cur$solution_type_error_source[[solution_type]]
      diag_rows[[b]][[paste0("error_message_", solution_type)]] <- cur$solution_type_error_message[[solution_type]]
      diag_rows[[b]][[paste0("solution_changed_", solution_type)]] <- if (status_ok && (solution_type %in% comparable_solution_types)) isTRUE(solution_cmp$changed_by_solution_type[[solution_type]]) else NA
      diag_rows[[b]][[paste0("fit_compared_", solution_type)]] <- isTRUE(fit_compared_by_solution_type[[solution_type]])
      diag_rows[[b]][[paste0("fit_changed_", solution_type)]] <- if (status_ok && isTRUE(fit_compared_by_solution_type[[solution_type]])) length(fit_changed_names_by_solution_type[[solution_type]]) > 0L else NA
      diag_rows[[b]][[paste0("max_abs_fit_delta_", solution_type)]] <- fit_delta_by_solution_type[[solution_type]]
      diag_rows[[b]][[paste0("matched_fit_keys_", solution_type)]] <- paste(fit_keys_by_solution_type[[solution_type]], collapse = ",")
    }

    by_draw[[b]] <- list(
      result = cur,
      solution_comparison = solution_cmp,
      comparable_solutions = comparable_solution_types,
      solution_changed = solution_changed,
      fit_changed = fit_changed,
      fit_changed_types = fit_changed_types,
      fit_changed_measures = fit_changed_measures,
      fit_delta = fit_delta,
      n_fit_deltas = n_fit_deltas,
      max_abs_fit_delta = max_abs_fit_delta,
      fit_compared_by_solution_type = fit_compared_by_solution_type,
      fit_changed_by_solution_type = setNames(
        lapply(
          monitored_solutions,
          function(solution_type) {
            if (!status_ok || !(solution_type %in% comparable_solution_types) || !isTRUE(fit_compared_by_solution_type[[solution_type]])) return(NA)
            length(fit_changed_names_by_solution_type[[solution_type]]) > 0L
          }
        ),
        monitored_solutions
      ),
      max_abs_fit_delta_by_solution_type = fit_delta_by_solution_type,
      matched_fit_keys_by_solution_type = fit_keys_by_solution_type,
      fit_details = fit_details
    )

    progress_state$set(b)
    if (isTRUE(verbose)) message("Completed draw ", b, "/", n_draws)
  }

  diagnostics <- .bind_rows_result(diag_rows)

  # summaries --
  solution_same_vec <- !diagnostics$solution_changed
  solution_same_vec[is.na(solution_same_vec)] <- FALSE

  score_solution <- mean(solution_same_vec)
  n_same_solution <- sum(solution_same_vec, na.rm = TRUE)

  n_fit_compared <- sum(diagnostics$fit_compared, na.rm = TRUE)
  n_fit_den <- sum(solution_same_vec, na.rm = TRUE)
  if (n_fit_den > 0L) {
    fit_same_equal <- !diagnostics$fit_changed[solution_same_vec]
    fit_same_equal[is.na(fit_same_equal)] <- FALSE
    n_same_fit <- sum(fit_same_equal)
    score_fit <- n_same_fit / n_fit_den
  } else {
    n_same_fit <- 0L
    score_fit <- NA_real_
  }

  score_total <- if (is.na(score_fit)) 0 else score_solution * score_fit

  score_solution_by_solution_type <- setNames(
    vapply(
      monitored_solutions,
      function(solution_type) {
        changed_vec <- diagnostics[[paste0("solution_changed_", solution_type)]]
        if (all(is.na(changed_vec))) return(NA_real_)
        mean(!changed_vec[!is.na(changed_vec)])
      },
      numeric(1)
    ),
    monitored_solutions
  )

  score_fit_by_solution_type <- setNames(
    vapply(
      monitored_solutions,
      function(solution_type) {
        idx <- diagnostics[[paste0("fit_compared_", solution_type)]]
        if (!any(idx, na.rm = TRUE)) return(NA_real_)
        vec <- !diagnostics[[paste0("fit_changed_", solution_type)]][idx]
        vec[is.na(vec)] <- FALSE
        mean(vec)
      },
      numeric(1)
    ),
    monitored_solutions
  )

  .new_result_object(
    "altset_test",
    diagnostics = diagnostics,
    results = .make_altset_results(diagnostics),
    summary = list(
      n_draws = n_draws,
      n_same_solution = n_same_solution,
      n_fit_compared = n_fit_compared,
      n_same_fit = n_same_fit,
      score_solution = score_solution,
      score_fit = score_fit,
      score_total = score_total,
      score_solution_by_solution_type = score_solution_by_solution_type,
      score_fit_by_solution_type = score_fit_by_solution_type
    ),
    baseline = list(
      result = baseline,
      draw_meta = baseline_draw_meta,
      status = baseline$status,
      exclude_used = baseline$exclude_used,
      sig = baseline$sig,
      fit = baseline$fit,
      selected_solution_missing = baseline$selected_solution_missing,
      meta = baseline$meta,
      solution_type_status = baseline$solution_type_status,
      solution_type_error_source = baseline$solution_type_error_source,
      solution_type_error_message = baseline$solution_type_error_message,
      comparable_solutions = baseline_solution_types
    ),
    by_draw = by_draw,
    settings = list(
      outcome = outcome,
      conditions = conditions,
      test.conditions = test.conditions,
      test.outcome = test.outcome,
      calib_spec = calib_specs,
      anchors_to_test = anchors_to_test,
      solution = solution,
      monitored_solutions = monitored_solutions,
      include = include,
      which_M = which_M,
      unit_step = unit_step,
      unit_step_divisor = unit_step_divisor,
      calib_max_steps = calib_max_steps,
      incl.cut = incl.cut,
      incl_step = incl_step,
      incl_max_steps = incl_max_steps,
      n.cut = n.cut,
      ncut_step = ncut_step,
      ncut_max_steps = ncut_max_steps,
      dir.exp = dir.exp,
      i_mode = i_mode,
      exclude_mode = exclude_mode,
      exclude_recompute = exclude_recompute,
      exclude_static = exclude_static,
      n_draws = n_draws,
      fit_tol = fit_tol,
      seed = seed
    )
  )
}

#' Print an `altset_test` object
#'
#' @rdname altset.test
#' @export
print.altset_test <- function(x, row.names = FALSE, ...) {
  results <- x$results
  settings <- x$settings
  summary <- x$summary

  .print_qcaert_heading("altset_test", "alternative-set robustness", settings)
  if (!is.null(settings$n_draws)) {
    cat("Draws: ", settings$n_draws, "\n", sep = "")
  }
  if (!is.null(settings$incl.cut)) {
    cat("Baseline incl.cut: ", format(settings$incl.cut, trim = TRUE), "\n", sep = "")
  }
  if (!is.null(settings$n.cut)) {
    cat("Baseline n.cut: ", format(settings$n.cut, trim = TRUE), "\n", sep = "")
  }
  if (!is.null(settings$fit_tol)) {
    cat("Fit tolerance: ", format(settings$fit_tol, trim = TRUE), "\n", sep = "")
  }
  if (!is.null(settings$test.conditions) && length(settings$test.conditions) > 0L) {
    cat("Test conditions: ", paste(settings$test.conditions, collapse = ", "), "\n", sep = "")
  }
  if (isTRUE(settings$test.outcome)) {
    cat("Test outcome: ", settings$outcome, "\n", sep = "")
  }

  .print_qcaert_table(results, "Result", row.names = row.names, ...)

  cat("\nSummary\n")
  if (!is.null(summary$n_draws)) {
    cat(" Draws: ", summary$n_draws, "\n", sep = "")
  }
  if (is.data.frame(results) && nrow(results) > 0L) {
    n_solution_changed <- .count_present(results$solution_change)
    n_fit_changed <- .count_present(results$fit_changed_types)

    cat(" Draws with any solution change: ", n_solution_changed, "\n", sep = "")
    cat(" Draws with any fit change: ", n_fit_changed, "\n", sep = "")
  }
  if (!is.null(summary$n_fit_compared)) {
    cat(" Draws with fit comparison: ", summary$n_fit_compared, "\n", sep = "")
  }

  if (!is.null(summary$score_solution_by_solution_type) && length(summary$score_solution_by_solution_type) > 0L) {
    solution_table <- data.frame(
      solution_type = vapply(names(summary$score_solution_by_solution_type), .compact_solution_type_names, character(1)),
      match_rate = as.numeric(summary$score_solution_by_solution_type),
      stringsAsFactors = FALSE
    )
    .print_qcaert_table(solution_table, "Solution match rate by solution type", row.names = FALSE)
  }

  if (!is.null(summary$score_fit_by_solution_type) && length(summary$score_fit_by_solution_type) > 0L) {
    fit_table <- data.frame(
      solution_type = vapply(names(summary$score_fit_by_solution_type), .compact_solution_type_names, character(1)),
      match_rate = as.numeric(summary$score_fit_by_solution_type),
      stringsAsFactors = FALSE
    )
    if (any(!is.na(fit_table$match_rate))) {
      .print_qcaert_table(fit_table, "Fit match rate by solution type", row.names = FALSE)
    }
  }

  invisible(x)
}

#' Return the main results table from an `altset_test` object
#'
#' @rdname altset.test
#' @export
as.data.frame.altset_test <- function(x, ...) {
  .as.data.frame_results(x, ...)
}
