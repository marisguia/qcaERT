#' Calibration-threshold robustness test for QCA solutions
#'
#' Perturbs calibration thresholds for selected conditions, and optionally the
#' outcome, one anchor and one direction at a time, and checks when the
#' monitored QCA solution changes.
#' For each tested path, the function records the starting threshold, the last
#' value that preserved the baseline solution, the first value that changed the
#' solution or triggered an error, and the reason the path stopped.
#'
#' @param raw.data A data frame object containing the raw, uncalibrated
#'   condition columns.
#' @param calib.data A data frame object containing the calibrated outcome
#'   and calibrated condition columns used for the analysis.
#' @param outcome Name of the outcome column in `calib.data`. This can be
#'   supplied as a character string or as an unquoted name.
#' @param conditions Character vector of calibrated condition names in
#'   `calib.data`. This can be supplied as a character vector or as unquoted
#'   names.
#' @param calib_spec Named list of calibration specifications. When
#'   `test.outcome = FALSE`, it must contain one entry per condition. When
#'   `test.outcome = TRUE`, it must contain one entry per condition plus one
#'   entry named by `outcome`. Each entry must contain `raw`, `type`, and
#'   `thresholds`, may contain `method`, and may contain `calibrate`, a named
#'   list of additional [QCA::calibrate()] arguments.
#' @param test.conditions Subset of `conditions` to perturb. Use `NULL` only
#'   when `test.outcome = TRUE` and no condition calibration should be tested.
#' @param test.outcome Logical; if `TRUE`, also perturb the outcome
#'   calibration while keeping the outcome out of `conditions`.
#' @param anchors_to_test Character vector of anchors to test, or `NULL` to
#'   test all anchors implied by each set's calibration method. Crisp
#'   sets use `"T"`. Fuzzy direct three-threshold sets use `"E"`,
#'   `"C"`, and `"I"`. Fuzzy direct six-threshold sets use `"E1"`,
#'   `"C1"`, `"I1"`, `"I2"`, `"C2"`, and `"E2"`. Fuzzy indirect sets
#'   use `"T1"`, `"T2"`, and so on.
#' @param solution Solution type to monitor. Accepted values are `"all"`,
#'   `"con"` or `"conservative"`, `"par"` or `"parsimonious"`, and `"int"` or
#'   `"intermediate"`.
#' @param include Optional minimization include setting. Currently, this
#'   argument accepts only `NULL`, `""`, or `"?"`.
#' @param which_M Positive integer giving which solution alternative to use
#'   when minimization returns multiple models.
#' @param unit_step Numeric step size used to move thresholds. Supply either
#'   one value applied to all tested sets, one value per condition when
#'   `test.outcome = FALSE`, or one value per `c(conditions, outcome)` when
#'   `test.outcome = TRUE`. If `NULL`, qcaERT computes scale-aware steps from
#'   each set's threshold spacing.
#' @param unit_step_divisor Positive number used to compute `unit_step`
#'   automatically when `unit_step = NULL`. The default is `10`.
#' @param max_steps Maximum number of upward or downward threshold moves to
#'   attempt for each tested anchor.
#' @param incl.cut Inclusion cutoff passed to [QCA::truthTable()].
#' @param n.cut Frequency cutoff passed to [QCA::truthTable()].
#' @param dir.exp Directional expectations used when the monitored solution is
#'   intermediate.
#' @param i_mode Character string controlling intermediate-solution selection.
#'   Accepted values are `"all"` and `"C1P1"`.
#' @param exclude_mode Character string controlling how excluded rows are
#'   handled for parsimonious and intermediate minimization. `"recompute"`
#'   recalculates exclusions from each perturbed truth table using
#'   `exclude_recompute`, `"static"` reuses `exclude_static`, and `"none"`
#'   does not use exclusions.
#' @param exclude_recompute Named list of arguments passed to [QCA::findRows()]
#'   when `exclude_mode = "recompute"`.
#' @param exclude_static Already computed exclusion object reused when
#'   `exclude_mode = "static"`.
#' @param result_shape Layout of the clean `results` table when
#'   `solution = "all"`. `"wide"` keeps one row per tested
#'   set-anchor-direction path with solution-type-specific columns such as
#'   `con_last_safe` and `par_reason`. `"long"` returns one row per tested
#'   set-anchor-direction path and solution type, with a `solution_type` column.
#'   Single-solution-type calls keep the compact single-solution-type layout.
#' @param progress Logical; if `TRUE` and the session is interactive, show a
#'   text progress bar.
#' @param x A `calib_test` object returned by [calib.test()].
#' @param row.names Logical; passed to [print.data.frame()] by
#'   [print.calib_test()].
#' @param ... Additional arguments routed through the QCA workflow. Arguments
#'   matching [QCA::truthTable()] are forwarded to truth table construction;
#'   remaining minimization arguments are filtered and forwarded to
#'   [QCA::minimize()]. The function also looks in `...` for `include`,
#'   `dir.exp`, or `direxp` if those arguments were not supplied explicitly.
#'   In [print.calib_test()], `...` is passed to [print.data.frame()]. In
#'   [as.data.frame.calib_test()], `...` is ignored.
#'
#' @returns An object of class `calib_test` with the following components:
#'   \describe{
#'     \item{`diagnostics`}{A detailed data frame with one row per tested
#'     anchor-direction path. When `solution = "all"`, each monitored solution
#'     type is searched independently, so diagnostics has one row per tested
#'     anchor, direction, and solution type. It includes the starting threshold, the
#'     last safe value, the first failing value, the number of successful
#'     steps, stop reason, and change classification.}
#'     \item{`results`}{A compact data frame with the columns `set`, `role`,
#'     `raw`, `type`, `method`, `anchor`, `direction`, `start`,
#'     `last_safe`, `first_failing`, `step_unit`, `steps`, `total_delta`,
#'     `pct_raw_range`, and `reason`. When `solution = "all"` and
#'     `result_shape = "wide"`, the row unit remains set-anchor-direction,
#'     but the boundary and reason columns are solution-type-specific, using prefixes
#'     `con_`, `par_`, and, when available, `int_`. When
#'     `result_shape = "long"`, `results` has one row per tested path and
#'     solution type.}
#'     \item{`bounds`}{A named list of compact lower/upper bound matrices, one
#'     per tested set. When `solution = "all"`, each tested set contains a
#'     named list of solution-type-specific lower/upper matrices.}
#'     \item{`baseline`}{A list containing the baseline analysis status,
#'     selected solution terms used for comparison, metadata, and
#'     solution-type-specific baseline objects.}
#'     \item{`by_set`}{A named list containing per-set step results, including
#'     path-level traces.}
#'     \item{`settings`}{A list containing the analysis settings used to build
#'     the result object.}
#'   }
#'
#'   `print.calib_test()` prints a concise summary and a compact display table.
#'   In that printed table, `set` and `role` are omitted and the raw source
#'   column is labelled `condition`. `as.data.frame.calib_test()` returns the
#'   stored `results` table.
#'   If `ggplot2` is installed, `plot.calib_test()` provides interval, heatmap,
#'   and trace views; see `?qcaERT_plots`.
#'
#' @details
#' The function rebuilds calibrated condition columns, and the outcome when
#' `test.outcome = TRUE`, from `raw.data` using `calib_spec`. It then compares
#' perturbed analyses against the baseline solution under the selected solution
#' type.
#'
#' When `solution = "all"`, conservative, parsimonious, and, when requested,
#' intermediate paths are searched independently for each tested set, anchor,
#' and direction.
#'
#' Each tested path starts from the supplied threshold for one method-specific
#' anchor of one set and moves in one direction at a time (`"lower"` or
#' `"upper"`), using `unit_step` until one of four things happens:
#' \itemize{
#'   \item the monitored solution changes,
#'   \item minimization fails,
#'   \item the raw-data range or anchor ordering blocks the next move, or
#'   \item `max_steps` is reached.
#' }
#'
#' The shared solution-control, exclusion, calibration-specification, and
#' returned-object conventions are described in `?qcaERT_conventions`.
#'
#' @examples
#' \donttest{
#' library(QCA)
#' library(ggplot2)
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
#' # Common use: test selected calibrated conditions with scale-aware steps.
#' calib_out <- calib.test(
#'   raw.data = LR,
#'   calib.data = dat,
#'   outcome = outcome,
#'   conditions = conditions,
#'   calib_spec = calib_spec,
#'   test.conditions = c("DEV", "URB"),
#'   unit_step = NULL,
#'   unit_step_divisor = 10,
#'   max_steps = 5,
#'   incl.cut = 0.8,
#'   n.cut = 1,
#'   solution = "all",
#'   dir.exp = dir_exp,
#'   progress = TRUE
#' )
#'
#' calib_out
#' as.data.frame(calib_out)
#' calib_out$bounds
#' calib_out$diagnostics
#'
#' plot(calib_out, solution_type = "conservative")
#' plot(calib_out, solution_type = "conservative", type = "heatmap")
#'
#' # Special case: test the calibrated outcome without putting it in
#' # conditions.
#' outcome_out <- calib.test(
#'   raw.data = LR,
#'   calib.data = dat,
#'   outcome = outcome,
#'   conditions = conditions,
#'   calib_spec = calib_spec_outcome,
#'   test.conditions = NULL,
#'   test.outcome = TRUE,
#'   unit_step = NULL,
#'   unit_step_divisor = 10,
#'   max_steps = 5,
#'   incl.cut = 0.8,
#'   n.cut = 1,
#'   solution = "all",
#'   dir.exp = dir_exp,
#'   progress = TRUE
#' )
#'
#' outcome_out
#' as.data.frame(outcome_out)
#' plot(outcome_out, solution_type = "conservative")
#'
#' # Special case: focus on selected anchors in a six-threshold direct
#' # calibration. For DEV, anchors are E1, C1, I1, I2, C2, and E2.
#' dev_anchor_out <- calib.test(
#'   raw.data = LR,
#'   calib.data = dat,
#'   outcome = outcome,
#'   conditions = conditions,
#'   calib_spec = calib_spec,
#'   test.conditions = "DEV",
#'   anchors_to_test = c("E1", "C1", "I1"),
#'   unit_step = NULL,
#'   unit_step_divisor = 10,
#'   max_steps = 5,
#'   incl.cut = 0.8,
#'   n.cut = 1,
#'   solution = "all",
#'   dir.exp = dir_exp,
#'   progress = TRUE
#' )
#'
#' as.data.frame(dev_anchor_out)
#'
#' plot(
#'   dev_anchor_out,
#'   solution_type = "conservative",
#'   type = "trace",
#'   set = "DEV",
#'   anchor = "E1",
#'   direction = "lower"
#' )
#'
#' # Special case: indirect calibration. Indirect anchors are positional:
#' # T1, T2, and so on.
#' thresholds_indirect <- thresholds
#' thresholds_indirect$DEV <- findTh(LR$DEV, groups = 4)
#'
#' dat_indirect <- dat
#' dat_indirect$DEV <- calibrate(
#'   LR$DEV,
#'   type = "fuzzy",
#'   method = "indirect",
#'   thresholds = thresholds_indirect$DEV
#' )
#'
#' calib_spec_indirect <- calib_spec
#' calib_spec_indirect$DEV <- list(
#'   raw = "DEV",
#'   type = "fuzzy",
#'   method = "indirect",
#'   thresholds = thresholds_indirect$DEV
#' )
#'
#' indirect_out <- calib.test(
#'   raw.data = LR,
#'   calib.data = dat_indirect,
#'   outcome = outcome,
#'   conditions = conditions,
#'   calib_spec = calib_spec_indirect,
#'   test.conditions = "DEV",
#'   anchors_to_test = c("T1", "T2"),
#'   unit_step = NULL,
#'   unit_step_divisor = 10,
#'   max_steps = 5,
#'   incl.cut = 0.8,
#'   n.cut = 1,
#'   solution = "all",
#'   dir.exp = dir_exp,
#'   progress = TRUE
#' )
#'
#' as.data.frame(indirect_out)
#' }
#'
#' @seealso [qcaERT_plots], [incl.test()], [ncut.test()], [loo.test()],
#'   [subsample.test()], [altset.test()], [theory.test()], [cluster.test()],
#'   [sol.df()]
#' @export
calib.test <- function(
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
    max_steps = 20,
    incl.cut = 1,
    n.cut = 1,
    dir.exp = NULL,
    i_mode = c("all", "C1P1"),
    exclude_mode = c("recompute", "static", "none"),
    exclude_recompute = list(type = 2),
    exclude_static = NULL,
    result_shape = c("wide", "long"),
    progress = TRUE,
    ...
) {

  .require_qca()

  mc <- match.call(expand.dots = FALSE)
  dots_raw <- list(...)
  caller_env <- parent.frame()

  selection_controls <- .normalize_i_mode(i_mode)
  i_mode <- selection_controls$i_mode
  exclude_mode <- match.arg(exclude_mode)
  result_shape <- match.arg(result_shape)
  .reject_exclusion_controls_in_dots(dots_raw, "calib.test")
  .reject_calibration_inputs_in_dots(dots_raw, "calib.test")
  dots_split <- .split_truth_table_minimize_dots(dots_raw)
  dots_tt <- dots_split$tt
  dots_min <- dots_split$min

  # capture minimize-style conveniences for outcome/conditions/dir.exp ----
  if (!is.null(mc$outcome)) {
    expr <- mc$outcome
    val <- .safe_eval_expr(expr, envir = caller_env)
    if (!inherits(val, "error") && is.character(val) && length(val) == 1) {
      outcome <- val
    } else {
      outcome <- .expr_to_chrvec(expr)
      if (length(outcome) != 1) stop("outcome must resolve to a single outcome name.")
    }
  }

  if (!is.null(mc$conditions)) {
    expr <- mc$conditions
    val <- .safe_eval_expr(expr, envir = caller_env)
    if (!inherits(val, "error") && is.character(val) && length(val) >= 1) {
      conditions <- val
    } else {
      conditions <- .expr_to_chrvec(expr)
    }
  }

  if (!is.null(mc[["dir.exp"]])) {
    expr <- mc[["dir.exp"]]
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
    caller = "calib.test",
    style = "plain"
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
    style = "plain"
  )

  .err_msg <- function(e) {
    if (!inherits(e, "error")) return(NA_character_)
    tryCatch(conditionMessage(e), error = function(...) as.character(e))
  }

  .stage_stop_reason <- function(source) {
    switch(
      source,
      truthTable = "truth_table_build_error",
      exclude = "exclude_recompute_error",
      conservative = "requested_minimize_error",
      parsimonious = "requested_minimize_error",
      intermediate = "requested_minimize_error",
      parsimonious_seed = "requested_minimize_error",
      "run_error"
    )
  }

  .ok_run_info <- function() {
    list(error = FALSE, source = NA_character_, message = NA_character_, reason = NA_character_)
  }

  .stage_error_info <- function(source, e) {
    list(
      error = TRUE,
      source = source,
      message = .err_msg(e),
      reason = .stage_stop_reason(source)
    )
  }

  .selected_solution_missing_info <- function(solution_type, meta) {
    list(
      error = TRUE,
      source = solution_type,
      message = paste0(
        "Requested `which_M = ",
        meta$which_M,
        "` is not available for the ",
        solution_type,
        " baseline solution."
      ),
      reason = "selected_solution_missing"
    )
  }

  .solution_type_selected_solution_missing <- function(run, solution_type) {
    if (!is.list(run) || is.null(run$meta)) return(FALSE)

    if (solution_type == "conservative") {
      return(identical(run$meta$con_hasM, FALSE))
    }

    if (solution_type == "parsimonious") {
      return(identical(run$meta$par_hasM, FALSE))
    }

    if (solution_type == "intermediate") {
      return(isTRUE(run$meta$int_missingM))
    }

    FALSE
  }

  .build_truth_table <- function(data_step) {
    tt_args <- c(
      list(
        data = data_step,
        outcome = outcome,
        incl.cut = incl.cut,
        n.cut = n.cut
      ),
      dots_tt
    )

    if (!is.null(conditions)) {
      tt_args$conditions <- conditions
    }

    tryCatch(
      suppressWarnings(do.call(QCA::truthTable, tt_args)),
      error = function(e) e
    )
  }

  .minimize_from_tt <- function(tt_obj, include_val, dir.exp_val = NULL, exclude_val = NULL, dots_filtered) {
    args <- c(
      list(
        input = tt_obj,
        include = include_val
      ),
      dots_filtered
    )

    if (!is.null(dir.exp_val)) args$dir.exp <- dir.exp_val
    if (!is.null(exclude_val)) args$exclude <- exclude_val

    tryCatch(
      suppressWarnings(do.call(QCA::minimize, args)),
      error = function(e) e
    )
  }

  .minimize_call <- function(data_step, include_val, dir.exp_val = NULL, exclude_val = NULL, dots_filtered) {
    tt_obj <- .build_truth_table(data_step)

    if (inherits(tt_obj, "error")) {
      return(tt_obj)
    }

    .minimize_from_tt(
      tt_obj = tt_obj,
      include_val = include_val,
      dir.exp_val = dir.exp_val,
      exclude_val = exclude_val,
      dots_filtered = dots_filtered
    )
  }

  .compute_exclude <- function(tt_obj) {
    .qcaert_compute_exclude(
      tt_obj = tt_obj,
      exclude_mode = exclude_mode,
      exclude_recompute = exclude_recompute,
      exclude_static = exclude_static
    )
  }

  .run_selected_solution <- function(data_step, solution, which_M, dots_filtered) {
    sig <- list(conservative = NULL, parsimonious = NULL, intermediate = NULL)
    meta <- list(
      which_M = which_M,
      con_nM = NA_integer_,
      par_nM = NA_integer_,
      int_nM_min = NA_integer_,
      con_hasM = NA,
      par_hasM = NA,
      int_missingM = NA
    )
    errs <- list(
      truthTable = NULL,
      conservative = NULL,
      parsimonious = NULL,
      exclude = NULL,
      intermediate = NULL,
      seed = NULL
    )

    r_con <- NULL
    r_par <- NULL
    r_int <- NULL
    r_par0 <- NULL
    exclude_used <- NULL

    tt_obj <- .build_truth_table(data_step)

    if (inherits(tt_obj, "error")) {
      errs$truthTable <- tt_obj
      if (solution == "conservative") {
        r_con <- tt_obj
      } else if (solution == "parsimonious") {
        r_par <- tt_obj
      } else {
        r_int <- tt_obj
      }

      return(list(
        r_con = r_con, r_par = r_par, r_int = r_int,
        exclude_used = exclude_used, sig = sig, meta = meta, errs = errs, r_par0 = r_par0
      ))
    }

    if (solution == "conservative") {
      r_con <- .minimize_from_tt(
        tt_obj = tt_obj,
        include_val = "",
        dots_filtered = dots_filtered
      )

      if (inherits(r_con, "error")) {
        errs$conservative <- r_con
      } else {
        con_info <- .extract_selected_m_standard_sig(r_con, which_M = which_M)
        sig$conservative <- con_info$sig
        meta$con_nM <- as.integer(con_info$n_M)
        meta$con_hasM <- isTRUE(con_info$has_M)
      }

      return(list(
        r_con = r_con, r_par = r_par, r_int = r_int,
        exclude_used = exclude_used, sig = sig, meta = meta, errs = errs, r_par0 = r_par0
      ))
    }

    if (solution == "parsimonious") {
      ex <- .compute_exclude(tt_obj)
      exclude_used <- ex

      if (inherits(ex, "error")) {
        errs$exclude <- ex
        r_par <- ex
      } else {
        r_par <- .minimize_from_tt(
          tt_obj = tt_obj,
          include_val = "?",
          exclude_val = ex,
          dots_filtered = dots_filtered
        )

        if (inherits(r_par, "error")) {
          errs$parsimonious <- r_par
        }
      }

      if (!inherits(r_par, "error")) {
        par_info <- .extract_selected_m_standard_sig(r_par, which_M = which_M)
        sig$parsimonious <- par_info$sig
        meta$par_nM <- as.integer(par_info$n_M)
        meta$par_hasM <- isTRUE(par_info$has_M)
      }

      return(list(
        r_con = r_con, r_par = r_par, r_int = r_int,
        exclude_used = exclude_used, sig = sig, meta = meta, errs = errs, r_par0 = r_par0
      ))
    }

    ex <- .compute_exclude(tt_obj)
    exclude_used <- ex

    if (inherits(ex, "error")) {
      errs$exclude <- ex
      r_int <- ex
    } else {
      r_int <- .minimize_from_tt(
        tt_obj = tt_obj,
        include_val = "?",
        dir.exp_val = dir.exp,
        exclude_val = ex,
        dots_filtered = dots_filtered
      )

      if (inherits(r_int, "error")) {
        errs$intermediate <- r_int
      }
    }

    if (!inherits(r_int, "error")) {
      tmp <- .extract_selected_m_intermediate_sig(r_int$i.sol, which_M = which_M, i_mode = i_mode)
      sig$intermediate <- tmp$sig
      meta$int_nM_min <- as.integer(tmp$n_M_min)
      meta$int_missingM <- isTRUE(tmp$missing_M)
    }

    list(
      r_con = r_con, r_par = r_par, r_int = r_int,
      exclude_used = exclude_used, sig = sig, meta = meta, errs = errs, r_par0 = r_par0
    )
  }

  .baseline_error_info <- function(base, solution, dir.exp) {
    if (!is.null(base$errs$truthTable)) {
      return(.stage_error_info("truthTable", base$errs$truthTable))
    }

    if (solution == "all") {
      baseline_error <- inherits(base$r_con, "error") ||
        inherits(base$r_par, "error") ||
        (!is.null(dir.exp) && (is.null(base$r_int) || inherits(base$r_int, "error")))

      if (!is.null(base$errs$conservative)) {
        return(.stage_error_info("conservative", base$errs$conservative))
      }
      if (!is.null(base$errs$parsimonious)) {
        return(.stage_error_info("parsimonious", base$errs$parsimonious))
      }
      if (!is.null(base$errs$exclude)) {
        return(.stage_error_info("exclude", base$errs$exclude))
      }
      if (!is.null(base$errs$intermediate)) {
        return(.stage_error_info("intermediate", base$errs$intermediate))
      }

      for (solution_type in c("conservative", "parsimonious", "intermediate")) {
        if (.solution_type_selected_solution_missing(base, solution_type)) {
          return(.selected_solution_missing_info(solution_type, base$meta))
        }
      }

      if (!baseline_error) {
        return(.ok_run_info())
      }

      return(list(error = TRUE, source = "unknown", message = NA_character_, reason = "run_error"))
    }

    if (solution == "conservative") {
      baseline_error <- inherits(base$r_con, "error")
      if (baseline_error) {
        return(.stage_error_info("conservative", base$errs$conservative))
      }
      if (.solution_type_selected_solution_missing(base, "conservative")) {
        return(.selected_solution_missing_info("conservative", base$meta))
      }
      return(.ok_run_info())
    }

    if (solution == "parsimonious") {
      baseline_error <- inherits(base$r_par, "error")
      if (!is.null(base$errs$exclude)) {
        return(.stage_error_info("exclude", base$errs$exclude))
      }
      if (baseline_error) {
        return(.stage_error_info("parsimonious", base$errs$parsimonious))
      }
      if (.solution_type_selected_solution_missing(base, "parsimonious")) {
        return(.selected_solution_missing_info("parsimonious", base$meta))
      }
      return(.ok_run_info())
    }

    baseline_error <- inherits(base$r_int, "error")
    if (!is.null(base$errs$seed)) {
      return(.stage_error_info("parsimonious_seed", base$errs$seed))
    }
    if (!is.null(base$errs$exclude)) {
      return(.stage_error_info("exclude", base$errs$exclude))
    }
    if (baseline_error) {
      return(.stage_error_info("intermediate", base$errs$intermediate))
    }
    if (.solution_type_selected_solution_missing(base, "intermediate")) {
      return(.selected_solution_missing_info("intermediate", base$meta))
    }
    .ok_run_info()
  }

  .step_error_info <- function(cur, solution, dir.exp) {
    if (!is.null(cur$errs$truthTable)) {
      return(.stage_error_info("truthTable", cur$errs$truthTable))
    }

    if (solution == "all") {
      any_err <- inherits(cur$r_con, "error") ||
        inherits(cur$r_par, "error") ||
        (!is.null(dir.exp) && !is.null(cur$r_int) && inherits(cur$r_int, "error"))

      if (!any_err) {
        return(.ok_run_info())
      }

      if (!is.null(cur$errs$conservative)) {
        return(.stage_error_info("conservative", cur$errs$conservative))
      }
      if (!is.null(cur$errs$parsimonious)) {
        return(.stage_error_info("parsimonious", cur$errs$parsimonious))
      }
      if (!is.null(cur$errs$exclude)) {
        return(.stage_error_info("exclude", cur$errs$exclude))
      }
      if (!is.null(cur$errs$intermediate)) {
        return(.stage_error_info("intermediate", cur$errs$intermediate))
      }

      return(list(error = TRUE, source = "unknown", message = NA_character_, reason = "run_error"))
    }

    if (solution == "conservative") {
      any_err <- inherits(cur$r_con, "error")
      if (!any_err) return(.ok_run_info())
      return(.stage_error_info("conservative", cur$errs$conservative))
    }

    if (solution == "parsimonious") {
      any_err <- inherits(cur$r_par, "error")
      if (!any_err) return(.ok_run_info())
      if (!is.null(cur$errs$exclude)) {
        return(.stage_error_info("exclude", cur$errs$exclude))
      }
      return(.stage_error_info("parsimonious", cur$errs$parsimonious))
    }

    any_err <- inherits(cur$r_int, "error")
    if (!any_err) return(.ok_run_info())
    if (!is.null(cur$errs$seed)) {
      return(.stage_error_info("parsimonious_seed", cur$errs$seed))
    }
    if (!is.null(cur$errs$exclude)) {
      return(.stage_error_info("exclude", cur$errs$exclude))
    }
    .stage_error_info("intermediate", cur$errs$intermediate)
  }

  .solution_change_info <- function(base, cur, solution, dir.exp) {
    if (solution == "all") {
      ch_con <- .sig_changed(base$sig$conservative, cur$sig$conservative)
      ch_par <- .sig_changed(base$sig$parsimonious, cur$sig$parsimonious)
      ch_int <- (!is.null(dir.exp) && .sig_changed(base$sig$intermediate, cur$sig$intermediate))

      changed <- ch_con || ch_par || ch_int

      changed_types <- if (changed) {
        paste(
          c(
            if (ch_con) "conservative" else NULL,
            if (ch_par) "parsimonious" else NULL,
            if (ch_int) "intermediate" else NULL
          ),
          collapse = ","
        )
      } else {
        NA_character_
      }

      change_kind <- if (changed) {
        paste(
          c(
            if (ch_con) paste0("conservative:", .change_kind_selected_m_sig(base$sig$conservative, cur$sig$conservative)) else NULL,
            if (ch_par) paste0("parsimonious:", .change_kind_selected_m_sig(base$sig$parsimonious, cur$sig$parsimonious)) else NULL,
            if (ch_int) paste0("intermediate:", .change_kind_selected_m_sig(base$sig$intermediate, cur$sig$intermediate)) else NULL
          ),
          collapse = ","
        )
      } else {
        NA_character_
      }

      return(list(
        changed = changed,
        changed_types = changed_types,
        change_kind = change_kind
      ))
    }

    if (solution == "conservative") {
      changed <- .sig_changed(base$sig$conservative, cur$sig$conservative)
      return(list(
        changed = changed,
        changed_types = if (changed) "conservative" else NA_character_,
        change_kind = if (changed) paste0("conservative:", .change_kind_selected_m_sig(base$sig$conservative, cur$sig$conservative)) else NA_character_
      ))
    }

    if (solution == "parsimonious") {
      changed <- .sig_changed(base$sig$parsimonious, cur$sig$parsimonious)
      return(list(
        changed = changed,
        changed_types = if (changed) "parsimonious" else NA_character_,
        change_kind = if (changed) paste0("parsimonious:", .change_kind_selected_m_sig(base$sig$parsimonious, cur$sig$parsimonious)) else NA_character_
      ))
    }

    changed <- .sig_changed(base$sig$intermediate, cur$sig$intermediate)
    list(
      changed = changed,
      changed_types = if (changed) "intermediate" else NA_character_,
      change_kind = if (changed) paste0("intermediate:", .change_kind_selected_m_sig(base$sig$intermediate, cur$sig$intermediate)) else NA_character_
    )
  }

  # validate/align ----
  if (is.null(dim(raw.data)) || is.null(nrow(raw.data)) || nrow(raw.data) < 1L) {
    stop("`raw.data` must be a non-empty data frame with at least one row.")
  }

  if (is.null(dim(calib.data)) || is.null(nrow(calib.data)) || nrow(calib.data) < 1L) {
    stop("`calib.data` must be a non-empty data frame with at least one row.")
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

  calib_context <- .prepare_calib_context(
    conditions = conditions,
    outcome = outcome,
    calib_spec = calib_spec,
    test.outcome = test.outcome,
    raw.data = raw.data,
    unit_step = unit_step,
    unit_step_divisor = unit_step_divisor,
    anchors_to_test = anchors_to_test,
    caller = "calib.test"
  )
  calib_specs <- calib_context$calib_specs
  type_fc <- calib_context$type_fc
  thr_list <- calib_context$thresholds
  unit_step_vec <- calib_context$unit_step_targets
  anchors_to_test <- calib_context$anchors_to_test
  which_M <- .coerce_which_M(which_M)

  tested_sets <- c(test.conditions, if (test.outcome) outcome else character(0))
  tested_roles <- c(rep("condition", length(test.conditions)), if (test.outcome) "outcome" else character(0))
  names(tested_roles) <- tested_sets

  dots_filtered <- dots_min

  # baseline data / baseline model ----
  baseline_data <- .calib_apply_specs_to_data(
    data = calib.data,
    raw.data = raw.data,
    calib_specs = calib_specs,
    conditions = calib_context$spec_targets
  )

  if (solution == "all") {
    baseline_by_solution_type <- setNames(
      lapply(
        monitored_solutions,
        function(solution_type) {
          .run_selected_solution(
            baseline_data,
            solution = solution_type,
            which_M = which_M,
            dots_filtered = dots_filtered
          )
        }
      ),
      monitored_solutions
    )

    baseline_model <- list(
      by_solution_type = baseline_by_solution_type,
      sig = lapply(baseline_by_solution_type, function(x) x$sig),
      meta = lapply(baseline_by_solution_type, function(x) x$meta),
      errs = lapply(baseline_by_solution_type, function(x) x$errs)
    )

    baseline_err_by_solution_type <- setNames(
      lapply(
        monitored_solutions,
        function(solution_type) .baseline_error_info(baseline_by_solution_type[[solution_type]], solution = solution_type, dir.exp = dir.exp)
      ),
      monitored_solutions
    )

    baseline_stop_reason <- NA_character_
    baseline_err_source <- NA_character_
    baseline_err_message <- NA_character_

    baseline_nM_con <- if ("conservative" %in% monitored_solutions) {
      baseline_by_solution_type$conservative$meta$con_nM
    } else {
      NA_integer_
    }
    baseline_nM_par <- if ("parsimonious" %in% monitored_solutions) {
      baseline_by_solution_type$parsimonious$meta$par_nM
    } else {
      NA_integer_
    }
    baseline_nM_int <- if ("intermediate" %in% monitored_solutions) {
      baseline_by_solution_type$intermediate$meta$int_nM_min
    } else {
      NA_integer_
    }

    baseline_error_flags <- vapply(
      baseline_err_by_solution_type,
      function(x) isTRUE(x$error),
      logical(1)
    )
    baseline_status <- if (!any(baseline_error_flags)) {
      "ok"
    } else if (all(baseline_error_flags)) {
      "baseline_error"
    } else {
      "partial"
    }
    first_baseline_error <- which(baseline_error_flags)[1L]
    baseline_error_source <- if (is.na(first_baseline_error)) {
      NA_character_
    } else {
      baseline_err_by_solution_type[[first_baseline_error]]$source
    }
    baseline_error_message <- if (is.na(first_baseline_error)) {
      NA_character_
    } else {
      baseline_err_by_solution_type[[first_baseline_error]]$message
    }
  } else {
    baseline_model <- .run_selected_solution(
      baseline_data,
      solution = solution,
      which_M = which_M,
      dots_filtered = dots_filtered
    )

    baseline_err <- .baseline_error_info(baseline_model, solution = solution, dir.exp = dir.exp)
    baseline_error <- isTRUE(baseline_err$error)
    baseline_stop_reason <- if (baseline_error) paste0("baseline_", baseline_err$reason) else NA_character_

    baseline_nM_con <- baseline_model$meta$con_nM
    baseline_nM_par <- baseline_model$meta$par_nM
    baseline_nM_int <- baseline_model$meta$int_nM_min

    baseline_err_source <- baseline_err$source
    baseline_err_message <- baseline_err$message

    baseline_by_solution_type <- setNames(list(baseline_model), solution)
    baseline_status <- if (baseline_error) "baseline_error" else "ok"
    baseline_error_source <- baseline_err_source
    baseline_error_message <- baseline_err_message
  }

  baseline <- list(
    status = baseline_status,
    solution = solution,
    monitored_solutions = monitored_solutions,
    by_solution_type = baseline_by_solution_type,
    sig = setNames(
      lapply(
        names(baseline_by_solution_type),
        function(solution_type) baseline_by_solution_type[[solution_type]]$sig[[solution_type]]
      ),
      names(baseline_by_solution_type)
    ),
    meta = setNames(
      lapply(
        names(baseline_by_solution_type),
        function(solution_type) baseline_by_solution_type[[solution_type]]$meta
      ),
      names(baseline_by_solution_type)
    ),
    errs = setNames(
      lapply(
        names(baseline_by_solution_type),
        function(solution_type) baseline_by_solution_type[[solution_type]]$errs
      ),
      names(baseline_by_solution_type)
    ),
    error_source = baseline_error_source,
    error_message = baseline_error_message
  )

  # outputs ----
  diag_rows <- list()
  by_set <- list()

  count_paths_one <- function(set_name) {
    n_paths <- length(.effective_calib_anchors(calib_specs[[set_name]], anchors_to_test)) * 2L
    if (solution == "all") {
      n_paths <- n_paths * length(monitored_solutions)
    }
    n_paths
  }
  total_paths <- sum(vapply(tested_sets, count_paths_one, integer(1)))
  progress_state <- .new_qcaert_progress(total = total_paths, progress = progress)
  on.exit(progress_state$close(), add = TRUE)
  .bump_pb <- progress_state$tick

  # main loop ----
  for (set_name in tested_sets) {

    role <- tested_roles[[set_name]]
    spec0 <- calib_specs[[set_name]]
    raw_name <- spec0$raw
    t0 <- spec0$thresholds
    t_fc <- spec0$type
    step_i <- unit_step_vec[[set_name]]

    anchors_eff <- .effective_calib_anchors(spec0, anchors_to_test)
    if (length(anchors_eff) == 0L) {
      stop(
        "`anchors_to_test` did not match any eligible calibration anchors for set '",
        set_name,
        "'. Eligible anchors are: ",
        paste(.calib_anchor_labels(spec0), collapse = ", "),
        "."
      )
    }

    x_raw <- raw.data[[raw_name]]
    xmin <- suppressWarnings(min(x_raw, na.rm = TRUE))
    xmax <- suppressWarnings(max(x_raw, na.rm = TRUE))
    x_range <- xmax - xmin

    set_out <- list(
      set = set_name,
      role = role,
      raw = raw_name,
      type = t_fc,
      method = spec0$method,
      thresholds0 = t0,
      xmin = xmin, xmax = xmax,
      unit_step_used = step_i,
      solution = solution,
      monitored_solutions = monitored_solutions,
      baseline = baseline
    )

    if (!is.na(baseline_stop_reason)) {
      for (a in anchors_eff) for (d in c("lower", "upper")) {
        diag_rows[[length(diag_rows) + 1]] <- data.frame(
          set = set_name,
          role = role,
          raw = raw_name,
          type = t_fc,
          method = spec0$method,
          anchor = a,
          direction = d,
          solution = solution,
          monitored_solutions = paste(monitored_solutions, collapse = ","),
          which_M_tested = which_M,
          n_M_conservative_baseline = baseline_nM_con,
          n_M_parsimonious_baseline = baseline_nM_par,
          n_M_intermediate_min_baseline = baseline_nM_int,
          step_unit_used = step_i,
          start_value = NA_real_,
          last_safe_value = NA_real_,
          failing_value = NA_real_,
          number_of_steps = NA_integer_,
          total_delta_units = NA_real_,
          delta_as_pct_of_raw_range = NA_real_,
          stop_reason = baseline_stop_reason,
          changed_types = NA_character_,
          change_kind = NA_character_,
          error_source = baseline_err_source,
          error_message = baseline_err_message,
          stringsAsFactors = FALSE
        )
        .bump_pb()
      }
      by_set[[set_name]] <- set_out
      next
    }

    step_results <- list()

    search_path <- function(anchor, direction, solution_type = NULL) {
      start_value <- .calib_anchor_value(spec0, anchor)
      solution_i <- if (is.null(solution_type)) solution else solution_type
      monitored_i <- if (is.null(solution_type)) monitored_solutions else solution_type
      baseline_i <- if (is.null(solution_type)) baseline_model else baseline_by_solution_type[[solution_type]]
      baseline_err_i <- if (is.null(solution_type)) {
        list(
          error = !is.na(baseline_stop_reason),
          source = baseline_err_source,
          message = baseline_err_message
        )
      } else {
        baseline_err_by_solution_type[[solution_type]]
      }

      cur_value <- start_value
      steps_done <- 0L
      stop_reason <- NA_character_
      failing_value <- NA_real_
      changed_types <- NA_character_
      change_kind <- NA_character_
      error_source <- NA_character_
      error_message <- NA_character_

      trace <- .empty_change_trace("value", value_type = "numeric")

      if (isTRUE(baseline_err_i$error)) {
        stop_reason <- paste0("baseline_", baseline_err_i$reason)
        error_source <- baseline_err_i$source
        error_message <- baseline_err_i$message

        row <- data.frame(
          set = set_name,
          role = role,
          raw = raw_name,
          type = t_fc,
          method = spec0$method,
          anchor = anchor,
          direction = direction,
          solution = solution,
          monitored_solutions = paste(monitored_i, collapse = ","),
          which_M_tested = which_M,
          n_M_conservative_baseline = baseline_nM_con,
          n_M_parsimonious_baseline = baseline_nM_par,
          n_M_intermediate_min_baseline = baseline_nM_int,
          step_unit_used = step_i,
          start_value = start_value,
          last_safe_value = NA_real_,
          failing_value = NA_real_,
          number_of_steps = NA_integer_,
          total_delta_units = NA_real_,
          delta_as_pct_of_raw_range = NA_real_,
          stop_reason = stop_reason,
          changed_types = NA_character_,
          change_kind = NA_character_,
          error_source = error_source,
          error_message = error_message,
          stringsAsFactors = FALSE
        )

        if (!is.null(solution_type)) {
          row$solution_type <- solution_type
        }

        path <- list(
          anchor = anchor,
          direction = direction,
          solution = solution,
          monitored_solutions = monitored_i,
          solution_type = solution_type,
          method = spec0$method,
          step_unit_used = step_i,
          start_value = start_value,
          last_safe_value = NA_real_,
          failing_value = NA_real_,
          steps_done = NA_integer_,
          stop_reason = stop_reason,
          changed_types = NA_character_,
          change_kind = NA_character_,
          error_source = error_source,
          error_message = error_message,
          trace = trace
        )

        return(list(row = row, path = path))
      }

      for (k in seq_len(max_steps)) {
        next_value <- if (direction == "lower") cur_value - step_i else cur_value + step_i

        if (!.is_calib_anchor_feasible(spec0, anchor, next_value, direction, xmin, xmax)) {
          stop_reason <- "feasibility_boundary"
          failing_value <- next_value
          break
        }

        spec_step <- .replace_calib_anchor(spec0, anchor, next_value)

        data_step <- baseline_data
        data_step[[set_name]] <- .calibrate_from_spec(x_raw, spec_step)

        cur <- .run_selected_solution(
          data_step,
          solution = solution_i,
          which_M = which_M,
          dots_filtered = dots_filtered
        )

        cur_err <- .step_error_info(cur, solution = solution_i, dir.exp = dir.exp)

        if (isTRUE(cur_err$error)) {
          stop_reason <- cur_err$reason
          failing_value <- next_value
          error_source <- cur_err$source
          error_message <- cur_err$message

          trace <- .append_change_trace(
            trace = trace,
            step = k,
            value_col = "value",
            value = next_value,
            changed = NA,
            status = stop_reason,
            change_kind = NA_character_
          )
          break
        }

        change_info <- .solution_change_info(
          baseline_i,
          cur,
          solution = solution_i,
          dir.exp = dir.exp
        )
        changed <- isTRUE(change_info$changed)

        trace <- .append_change_trace(
          trace = trace,
          step = k,
          value_col = "value",
          value = next_value,
          changed = changed,
          status = "ok",
          change_kind = if (changed) change_info$change_kind else NA_character_
        )

        if (changed) {
          stop_reason <- "solution_change"
          failing_value <- next_value
          changed_types <- change_info$changed_types
          change_kind <- change_info$change_kind
          break
        }

        cur_value <- next_value
        steps_done <- steps_done + 1L
      }

      if (is.na(stop_reason)) stop_reason <- "run_budget_exhausted"

      last_safe_value <- cur_value
      total_delta <- last_safe_value - start_value
      pct_delta <- if (is.finite(x_range) && x_range > 0) (abs(total_delta) / x_range) * 100 else NA_real_

      row <- data.frame(
        set = set_name,
        role = role,
        raw = raw_name,
        type = t_fc,
        method = spec0$method,
        anchor = anchor,
        direction = direction,
        solution = solution,
        monitored_solutions = paste(monitored_i, collapse = ","),
        which_M_tested = which_M,
        n_M_conservative_baseline = baseline_nM_con,
        n_M_parsimonious_baseline = baseline_nM_par,
        n_M_intermediate_min_baseline = baseline_nM_int,
        step_unit_used = step_i,
        start_value = start_value,
        last_safe_value = last_safe_value,
        failing_value = failing_value,
        number_of_steps = steps_done,
        total_delta_units = total_delta,
        delta_as_pct_of_raw_range = pct_delta,
        stop_reason = stop_reason,
        changed_types = changed_types,
        change_kind = change_kind,
        error_source = error_source,
        error_message = error_message,
        stringsAsFactors = FALSE
      )

      if (!is.null(solution_type)) {
        row$solution_type <- solution_type
      }

      path <- list(
        anchor = anchor,
        direction = direction,
        solution = solution,
        monitored_solutions = monitored_i,
        solution_type = solution_type,
        method = spec0$method,
        step_unit_used = step_i,
        start_value = start_value,
        last_safe_value = last_safe_value,
        failing_value = failing_value,
        steps_done = steps_done,
        stop_reason = stop_reason,
        changed_types = changed_types,
        change_kind = change_kind,
        error_source = error_source,
        error_message = error_message,
        trace = trace
      )

      list(row = row, path = path)
    }

    for (anchor in anchors_eff) {
      for (direction in c("lower", "upper")) {
        step_key <- paste(anchor, direction, sep = "_")

        if (solution == "all") {
          paths <- list()
          for (solution_type in monitored_solutions) {
            searched <- search_path(anchor, direction, solution_type = solution_type)
            diag_rows[[length(diag_rows) + 1]] <- searched$row
            paths[[solution_type]] <- searched$path
            .bump_pb()
          }

          first_path <- paths[[monitored_solutions[1L]]]
          step_results[[step_key]] <- c(
            list(
              anchor = anchor,
              direction = direction,
              solution = solution,
              monitored_solutions = monitored_solutions,
              by_solution_type = paths
            ),
            first_path[setdiff(names(first_path), c("anchor", "direction", "solution", "monitored_solutions"))]
          )
        } else {
          searched <- search_path(anchor, direction)
          diag_rows[[length(diag_rows) + 1]] <- searched$row
          step_results[[step_key]] <- searched$path
          .bump_pb()
        }
      }
    }

    set_out$steps <- step_results
    by_set[[set_name]] <- set_out
  }

  diagnostics <- .bind_rows_result(diag_rows)

  compact_bounds <- function(diag_df, set_name) {
    dd <- diag_df[diag_df$set == set_name, , drop = FALSE]
    if (nrow(dd) == 0) return(NULL)

    if ("solution_type" %in% names(dd)) {
      solution_types <- .result_solution_type_order(dd$solution_type)
      out <- setNames(vector("list", length(solution_types)), solution_types)

      for (solution_type in solution_types) {
        dd_solution_type <- dd[dd$solution_type == solution_type, , drop = FALSE]
        anchors <- sort(unique(dd_solution_type$anchor))
        mat <- matrix(
          NA_real_,
          nrow = 2,
          ncol = length(anchors),
          dimnames = list(c("Lower", "Upper"), anchors)
        )

        for (a in anchors) {
          lo <- dd_solution_type[dd_solution_type$anchor == a & dd_solution_type$direction == "lower", , drop = FALSE]
          up <- dd_solution_type[dd_solution_type$anchor == a & dd_solution_type$direction == "upper", , drop = FALSE]
          if (nrow(lo) == 1) mat["Lower", a] <- lo$last_safe_value
          if (nrow(up) == 1) mat["Upper", a] <- up$last_safe_value
        }

        out[[solution_type]] <- mat
      }

      return(out)
    }

    anchors <- sort(unique(dd$anchor))
    out <- matrix(NA_real_, nrow = 2, ncol = length(anchors),
                  dimnames = list(c("Lower", "Upper"), anchors))
    for (a in anchors) {
      lo <- dd[dd$anchor == a & dd$direction == "lower", , drop = FALSE]
      up <- dd[dd$anchor == a & dd$direction == "upper", , drop = FALSE]
      if (nrow(lo) == 1) out["Lower", a] <- lo$last_safe_value
      if (nrow(up) == 1) out["Upper", a] <- up$last_safe_value
    }
    out
  }

  bounds_by_set <- lapply(names(by_set), function(nm) compact_bounds(diagnostics, nm))
  names(bounds_by_set) <- names(by_set)

  .new_result_object(
    "calib_test",
    diagnostics = diagnostics,
    results = .make_calib_results(diagnostics, result_shape = result_shape),
    bounds = bounds_by_set,
    baseline = baseline,
    by_set = by_set,
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
      max_steps = max_steps,
      incl.cut = incl.cut,
      n.cut = n.cut,
      dir.exp = dir.exp,
      i_mode = i_mode,
      exclude_mode = exclude_mode,
      exclude_recompute = exclude_recompute,
      exclude_static = exclude_static,
      result_shape = result_shape
      )
  )
}

#' Print a `calib_test` object
#'
#' @rdname calib.test
#' @export
print.calib_test <- function(x, row.names = FALSE, ...) {
  results <- x$results
  display_results <- .calib_results_for_print(results)
  settings <- x$settings

  .print_qcaert_heading("calib_test", "calibration robustness", settings)
  if (!is.null(settings$test.conditions) && length(settings$test.conditions) > 0L) {
    cat("Test conditions: ", paste(settings$test.conditions, collapse = ", "), "\n", sep = "")
  }
  if (isTRUE(settings$test.outcome)) {
    cat("Test outcome: ", settings$outcome, "\n", sep = "")
  }
  if (!is.null(settings$anchors_to_test)) {
    cat("Anchors tested: ", paste(settings$anchors_to_test, collapse = ", "), "\n", sep = "")
  }
  if (!is.null(settings$max_steps)) {
    cat("Max steps: ", settings$max_steps, "\n", sep = "")
  }

  .print_qcaert_table(display_results, "Result", row.names = row.names, ...)

  cat("\nSummary\n")
  if (is.data.frame(display_results) && nrow(display_results) > 0L) {
    cat(" Rows: ", nrow(display_results), "\n", sep = "")
    if ("condition" %in% names(display_results)) {
      cat(" Conditions covered: ", paste(unique(display_results$condition), collapse = ", "), "\n", sep = "")
    }
    cat(" Bounds are available in x$bounds.\n", sep = "")
  }

  invisible(x)
}

.calib_results_for_print <- function(results) {
  if (!is.data.frame(results)) {
    return(results)
  }

  out <- results
  out <- out[, setdiff(names(out), c("set", "role")), drop = FALSE]
  raw_idx <- match("raw", names(out))
  if (!is.na(raw_idx)) {
    names(out)[raw_idx] <- "condition"
  }

  out
}

#' Return the main results table from a `calib_test` object
#'
#' @rdname calib.test
#' @export
as.data.frame.calib_test <- function(x, ...) {
  .as.data.frame_results(x, ...)
}
