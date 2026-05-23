#' Subsample robustness test for QCA solutions
#'
#' Draws repeated subsamples without replacement, reruns the QCA analysis on
#' each subsample, and records whether the monitored solution changes, whether
#' selected fit measures change, and whether a subsample run ends with an
#' error. The function can sample without stratification, stratify by the
#' baseline outcome, or stratify by a grouping column. This is a stringent,
#' punishing robustness check and is most useful when sample-composition
#' sensitivity is substantively important.
#'
#' @param data A data frame object containing the outcome and condition
#'   columns used in the QCA analysis. `data` must have at least
#'   three rows.
#' @param outcome Name of the outcome. This must be a single
#'   non-empty character string.
#' @param conditions Optional character vector of condition names. If `NULL`,
#'   the condition set is left to [QCA::truthTable()]. When
#'   `calib = "recompute"`, `conditions` must be supplied explicitly.
#' @param calib Calibration handling for subsample runs. `"fixed"` uses the
#'   calibrated values already present in `data`. `"recompute"` recalibrates
#'   the outcome and conditions within each subsample using `raw.data` and
#'   `calib_spec`.
#' @param raw.data Raw-data frame object used when `calib = "recompute"`.
#'   It must have the same number of rows as `data`.
#' @param calib_spec Calibration specification used when
#'   `calib = "recompute"`. This must be a named list keyed by
#'   `c(outcome, conditions)`. Each entry must describe the raw source column,
#'   the set type, and the calibration inputs used to rebuild the calibrated
#'   set. Use the same `calib_spec` structure described for [calib.test()]. An
#'   entry may additionally contain `findTh`, a named list of [QCA::findTh()]
#'   arguments; when supplied, thresholds are re-estimated within each
#'   subsample instead of reusing the baseline `thresholds`.
#' @param sample_n Integer subsample size. Supply exactly one of `sample_n` or
#'   `sample_prop`.
#' @param sample_prop Proportion used to determine the realized subsample size.
#'   Supply exactly one of `sample_n` or `sample_prop`. The realized sample
#'   size is `round(sample_prop * nrow(data))`.
#' @param reps Number of subsample replications.
#' @param stratify Stratification mode. `"none"` samples from the full dataset
#'   without stratification, `"outcome"` stratifies on the baseline analysis
#'   outcome, and `"user"` stratifies on `strata`.
#' @param strata Optional stratification column used when
#'   `stratify = "user"`. It must have length `nrow(data)` and contain no
#'   missing values.
#' @param seed Optional integer seed passed to [set.seed()] before drawing the
#'   subsamples.
#' @param case_labels Optional character vector of case labels with length
#'   `nrow(data)`. If `NULL`, the function uses `rownames(data)` when
#'   available; otherwise it uses row numbers converted to character.
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
#'   recalculates exclusions from each subsample truth table, `"static"`
#'   reuses `exclude_static`, and `"none"` does not use exclusions.
#' @param exclude_recompute Named list of arguments passed to [QCA::findRows()]
#'   when `exclude_mode = "recompute"`.
#' @param exclude_static Already computed exclusion object reused when
#'   `exclude_mode = "static"`.
#' @param fit_measures Character vector of fit-measure names to compare between
#'   the baseline run and each subsample run. Use `NULL` to disable fit
#'   comparison.
#' @param fit_tol Non-negative tolerance used when deciding whether fit values
#'   changed.
#' @param progress Logical; if `TRUE` and the session is interactive, show a
#'   text progress bar.
#' @param x A `subsample_test` object returned by [subsample.test()].
#' @param row.names Logical; passed to [print.data.frame()] by
#'   [print.subsample_test()].
#' @param ... Additional arguments. In [subsample.test()], named arguments
#'   matching [QCA::truthTable()] formals are passed to [QCA::truthTable()],
#'   and the remaining named arguments are forwarded to [QCA::minimize()]
#'   after removing names reserved by `subsample.test()`. The function
#'   also looks in `...` for `include`, `dir.exp`, or `direxp` if those
#'   arguments were not supplied explicitly. In [print.subsample_test()],
#'   `...` is passed to [print.data.frame()]. In
#'   [as.data.frame.subsample_test()], `...` is ignored.
#'
#' @returns An object of class `subsample_test` with the following components:
#'   \describe{
#'     \item{`diagnostics`}{A detailed data frame with one row per subsample
#'     run. It records the replication number, subsample size, holdout size,
#'     run status, solution-change classification, fit-change classification,
#'     exact-match status relative to the baseline solution, term-set Jaccard
#'     similarity to the baseline solution, and error information when a run
#'     fails.}
#'     \item{`results`}{A compact data frame with the columns
#'     `rep`, `n_sample`, `n_holdout`, `status`, `exact_match_baseline`,
#'     `term_jaccard_baseline`, `solution_change`, `fit_changed_types`,
#'     `n_fit_deltas`, and `max_abs_fit_delta`.}
#'     \item{`summary`}{A list with summary objects named `exact_solution`,
#'     `term_stability`, `fit_stability`, `similarity`, and `calibration`.}
#'     \item{`baseline`}{A list containing the baseline analysis built from the
#'     full dataset, including the truth table, minimization output,
#'     selected solution terms used for comparison, fit values, exclusion
#'     information, and calibration information.}
#'     \item{`by_run`}{A named list with one entry per subsample run. Each
#'     entry stores the sampled and held-out cases, run status, baseline and
#'     subsample analyses, change summaries, fit deltas, exclusion
#'     information, calibration information, and any error information for
#'     that run.}
#'     \item{`settings`}{A list containing the analysis settings used to build
#'     the result object.}
#'   }
#'
#'   `print.subsample_test()` prints a concise summary and the `results`
#'   table. `as.data.frame.subsample_test()` returns the `results` table.
#'
#' @details
#' The function first runs the baseline analysis on the full dataset. It then
#' draws `reps` subsamples without replacement and reruns the analysis on each
#' subsample.
#'
#' Exactly one of `sample_n` or `sample_prop` must be supplied. The realized
#' subsample size must be at least `2` and strictly smaller than
#' `nrow(data)`.
#'
#' When `stratify = "outcome"`, the function stratifies on the baseline
#' analysis outcome and therefore requires that outcome to be crisp/binary
#' `0/1`. When `stratify = "user"`, the function stratifies on the supplied
#' `strata` vector.
#'
#' When `calib = "fixed"`, the subsample runs use the calibrated values
#' already present in `data`. When `calib = "recompute"`, the function rebuilds
#' the calibrated outcome and condition columns within each subsample using
#' [QCA::findTh()] and [QCA::calibrate()] as specified in `calib_spec`.
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
#' out <- subsample.test(
#'   data = dat,
#'   outcome = outcome,
#'   conditions = conditions,
#'   calib = "fixed",
#'   sample_prop = 0.8,
#'   reps = 50,
#'   seed = 123,
#'   case_labels = rownames(dat),
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
#' @seealso [calib.test()], [incl.test()], [ncut.test()], [loo.test()],
#'   [altset.test()], [theory.test()], [cluster.test()], [sol.df()]
#' @export
subsample.test <- function(
    data,
    outcome,
    conditions = NULL,
    calib = c("fixed", "recompute"),
    raw.data = NULL,
    calib_spec = NULL,
    sample_n = NULL,
    sample_prop = NULL,
    reps = 100,
    stratify = c("none", "outcome", "user"),
    strata = NULL,
    seed = NULL,
    case_labels = NULL,
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
  stratify <- match.arg(stratify)
  calib <- match.arg(calib)

  if (is.null(dim(data)) || is.null(nrow(data)) || nrow(data) < 3L) {
    stop("`data` must be a data frame object with at least 3 rows.")
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

  if (!is.numeric(reps) || length(reps) != 1L || !is.finite(reps) || reps < 1) {
    stop("`reps` must be a single integer >= 1.")
  }
  reps <- as.integer(reps)

  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed)) {
      stop("`seed` must be NULL or a single finite numeric value.")
    }
    seed <- as.integer(seed)
  }

  if (xor(is.null(sample_n), is.null(sample_prop)) == FALSE) {
    stop("Supply exactly one of `sample_n` or `sample_prop`.")
  }

  n_total <- nrow(data)

  if (!is.null(sample_n)) {
    if (!is.numeric(sample_n) || length(sample_n) != 1L || !is.finite(sample_n)) {
      stop("`sample_n` must be a single finite number.")
    }
    sample_n <- as.integer(sample_n)
  } else {
    if (!is.numeric(sample_prop) || length(sample_prop) != 1L || !is.finite(sample_prop) ||
        sample_prop <= 0 || sample_prop >= 1) {
      stop("`sample_prop` must be a single finite number in (0, 1).")
    }
    sample_n <- as.integer(round(sample_prop * n_total))
  }

  if (sample_n < 2L || sample_n >= n_total) {
    stop("The realized sample size must be at least 2 and strictly smaller than `nrow(data)`.")
  }

  sample_prop_realized <- sample_n / n_total

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
  .reject_exclusion_controls_in_dots(dots_raw, "subsample.test")
  .reject_calibration_inputs_in_dots(dots_raw, "subsample.test")

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
    caller = "subsample.test",
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
  } else {
    analysis_vars <- unique(c(outcome, conditions))
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
      include_analysis_data = TRUE
    )
  }

  .is_binary01 <- function(x) {
    x <- unique(x[!is.na(x)])
    length(x) > 0L && all(x %in% c(0, 1))
  }

  .alloc_by_strata <- function(group_sizes, n_total) {
    if (sum(group_sizes) < n_total) {
      stop("Internal error: requested sample size exceeds total available size.")
    }

    props <- group_sizes / sum(group_sizes)
    raw_alloc <- props * n_total
    alloc <- pmin(group_sizes, floor(raw_alloc))

    remainder <- n_total - sum(alloc)
    if (remainder > 0L) {
      frac <- raw_alloc - floor(raw_alloc)
      capacity <- group_sizes - alloc

      ord <- order(frac, decreasing = TRUE)
      for (i in ord) {
        if (remainder <= 0L) break
        if (capacity[i] > 0L) {
          alloc[i] <- alloc[i] + 1L
          capacity[i] <- capacity[i] - 1L
          remainder <- remainder - 1L
        }
      }
    }

    if (sum(alloc) != n_total) {
      capacity <- group_sizes - alloc
      ord <- order(capacity, decreasing = TRUE)
      for (i in ord) {
        if (sum(alloc) >= n_total) break
        if (capacity[i] > 0L) {
          take <- min(capacity[i], n_total - sum(alloc))
          alloc[i] <- alloc[i] + take
        }
      }
    }

    if (sum(alloc) != n_total) {
      stop("Could not allocate the requested sample size across strata.")
    }

    alloc
  }

  .sig_term_set <- function(sig) {
    if (is.null(sig)) return(character(0))
    x <- as.character(sig)
    x <- x[!is.na(x)]
    x <- trimws(x)
    x <- x[nzchar(x)]
    if (length(x) == 0L) return(character(0))

    x <- gsub("^.*:", "", x)
    out <- unlist(strsplit(x, "\\+", fixed = FALSE), use.names = FALSE)
    out <- .sanitize_terms(out)
    out[!grepl("^<.*>$", out)]
  }

  .run_term_set <- function(run_obj, monitored_solutions) {
    out <- character(0)
    for (solution_type in monitored_solutions) {
      out <- c(out, .sig_term_set(run_obj$sig[[solution_type]]))
    }
    sort(unique(out))
  }

  .solution_type_term_set <- function(run_obj, solution_type) {
    sort(unique(.sig_term_set(run_obj$sig[[solution_type]])))
  }

  .jaccard <- function(a, b) {
    a <- sort(unique(a))
    b <- sort(unique(b))
    if (length(a) == 0L && length(b) == 0L) return(1)
    u <- union(a, b)
    if (length(u) == 0L) return(NA_real_)
    length(intersect(a, b)) / length(u)
  }

  case_ref <- .reduced_resolve_case_labels(data = data, case_labels = case_labels)

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

  if (baseline_invalid) {
    diagnostics <- data.frame(
      rep = seq_len(reps),
      status = if (baseline$status != "ok") paste0("baseline_", baseline$status) else if (baseline_missing) "baseline_selected_solution_missing" else "baseline_no_comparable_solution",
      n_sample = rep(sample_n, reps),
      n_holdout = rep(n_total - sample_n, reps),
      changed = NA,
      solution_changed = NA,
      changed_types = NA_character_,
      change_kind = NA_character_,
      fit_changed = NA,
      fit_changed_types = NA_character_,
      n_fit_deltas = NA_integer_,
      max_abs_fit_delta = NA_real_,
      exact_match_baseline = NA,
      term_jaccard_baseline = NA_real_,
      error_source = baseline$error_source,
      error_message = baseline$error_message,
      stringsAsFactors = FALSE
    )

    return(.new_result_object(
      "subsample_test",
      diagnostics = diagnostics,
      results = .make_subsample_results(diagnostics),
      summary = list(),
      baseline = baseline,
      by_run = setNames(vector("list", reps), paste0("R", seq_len(reps))),
      settings = list(
        outcome = outcome,
        conditions = conditions,
        calib = calib,
        sample_n = sample_n,
        sample_prop_requested = sample_prop,
        sample_prop_realized = sample_prop_realized,
        reps = reps,
        stratify = stratify,
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
        fit_tol = fit_tol,
        seed = seed
      )
    ))
  }

  baseline_analysis_data <- baseline$analysis_data

  if (stratify == "outcome") {
    yb <- baseline_analysis_data[[outcome]]
    if (!.is_binary01(yb)) {
      stop("`stratify = \"outcome\"` requires the baseline analysis outcome to be crisp/binary 0/1.")
    }
    strata_use <- yb
  } else if (stratify == "user") {
    if (is.null(strata)) {
      stop("`stratify = \"user\"` requires `strata`.")
    }
    if (length(strata) != n_total) {
      stop("`strata` must have length `nrow(data)`.")
    }
    strata_use <- as.character(strata)
  } else {
    strata_use <- rep("all", n_total)
  }

  strata_use <- as.character(strata_use)
  if (any(is.na(strata_use))) {
    stop("`strata` / baseline outcome strata contain missing values.")
  }

  strata_levels <- unique(strata_use)
  strata_index <- split(seq_len(n_total), strata_use)
  strata_sizes <- vapply(strata_index, length, integer(1))
  sample_alloc <- .alloc_by_strata(group_sizes = strata_sizes, n_total = sample_n)
  names(sample_alloc) <- names(strata_sizes)

  sampling_plan <- vector("list", reps)

  if (!is.null(seed)) set.seed(seed)

  for (r in seq_len(reps)) {
    idx_sample <- integer(0)
    realized_counts <- integer(0)
    names(realized_counts) <- character(0)

    for (lev in names(strata_index)) {
      idx_pool <- strata_index[[lev]]
      n_take <- sample_alloc[[lev]]
      if (n_take > 0L) {
        draw <- sample(idx_pool, size = n_take, replace = FALSE)
        idx_sample <- c(idx_sample, draw)
        realized_counts <- c(realized_counts, n_take)
        names(realized_counts)[length(realized_counts)] <- lev
      }
    }

    idx_sample <- sort(idx_sample)
    idx_holdout <- setdiff(seq_len(n_total), idx_sample)

    sampling_plan[[r]] <- list(
      rep = r,
      sample_index = idx_sample,
      holdout_index = idx_holdout,
      sample_labels = case_ref$case_label[idx_sample],
      holdout_labels = case_ref$case_label[idx_holdout],
      strata_counts = realized_counts
    )
  }

  progress_state <- .new_qcaert_progress(total = reps, progress = progress)
  on.exit(progress_state$close(), add = TRUE)
  .bump_pb <- progress_state$tick

  diag_rows <- vector("list", reps)
  by_run <- vector("list", reps)
  names(by_run) <- paste0("R", seq_len(reps))

  baseline_term_set_all <- .run_term_set(baseline, baseline_solution_types)

  for (r in seq_len(reps)) {
    plan_r <- sampling_plan[[r]]
    idx_sample <- plan_r$sample_index
    idx_holdout <- plan_r$holdout_index

    data_sample <- data[idx_sample, , drop = FALSE]
    raw_sample <- if (calib == "recompute") raw_data_work[idx_sample, , drop = FALSE] else NULL

    cur <- .run_once(
      data_step = data_sample,
      raw_step = raw_sample
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

      diag_rows[[r]] <- data.frame(
        rep = r,
        status = status_info$status,
        n_sample = length(idx_sample),
        n_holdout = length(idx_holdout),
        changed = NA,
        solution_changed = NA,
        changed_types = NA_character_,
        change_kind = NA_character_,
        fit_changed = NA,
        fit_changed_types = NA_character_,
        n_fit_deltas = NA_integer_,
        max_abs_fit_delta = NA_real_,
        exact_match_baseline = NA,
        term_jaccard_baseline = NA_real_,
        error_source = status_info$error_source,
        error_message = status_info$error_message,
        stringsAsFactors = FALSE
      )

      by_run[[r]] <- list(
        rep = r,
        sample_index = idx_sample,
        sample_labels = plan_r$sample_labels,
        holdout_index = idx_holdout,
        holdout_labels = plan_r$holdout_labels,
        strata_counts = plan_r$strata_counts,
        status = status_info$status,
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
        exact_match_baseline = NA,
        term_jaccard_baseline = NA_real_,
        exclude_baseline = baseline$exclude_used,
        exclude_reduced = cur$exclude_used,
        calibration_baseline = baseline$calibration,
        calibration_run = cur$calibration,
        baseline_fit = baseline$fit,
        reduced_fit = cur$fit,
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

    exact_match_baseline <- !isTRUE(sol_info$changed)
    term_set_cur <- .run_term_set(cur, baseline_solution_types)
    term_jaccard_baseline <- .jaccard(baseline_term_set_all, term_set_cur)
    changed_any <- isTRUE(sol_info$changed) || isTRUE(fit_info$changed)

    diag_rows[[r]] <- data.frame(
      rep = r,
      status = "ok",
      n_sample = length(idx_sample),
      n_holdout = length(idx_holdout),
      changed = changed_any,
      solution_changed = isTRUE(sol_info$changed),
      changed_types = sol_info$changed_types,
      change_kind = sol_info$change_kind,
      fit_changed = isTRUE(fit_info$changed),
      fit_changed_types = fit_info$changed_types,
      n_fit_deltas = as.integer(fit_info$n_changed),
      max_abs_fit_delta = fit_info$max_abs_delta,
      exact_match_baseline = exact_match_baseline,
      term_jaccard_baseline = term_jaccard_baseline,
      error_source = NA_character_,
      error_message = NA_character_,
      stringsAsFactors = FALSE
    )

    by_run[[r]] <- list(
      rep = r,
      sample_index = idx_sample,
      sample_labels = plan_r$sample_labels,
      holdout_index = idx_holdout,
      holdout_labels = plan_r$holdout_labels,
      strata_counts = plan_r$strata_counts,
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
      exact_match_baseline = exact_match_baseline,
      term_jaccard_baseline = term_jaccard_baseline,
      exclude_baseline = baseline$exclude_used,
      exclude_reduced = cur$exclude_used,
      calibration_baseline = baseline$calibration,
      calibration_run = cur$calibration,
      baseline_fit = baseline$fit,
      reduced_fit = cur$fit,
      error_source = NA_character_,
      error_message = NA_character_
    )

    .bump_pb()
  }

  diagnostics <- .bind_rows_result_sorted(diag_rows, order_by = "rep")

  .sig_to_key <- function(sig) {
    if (is.null(sig)) return(NA_character_)
    if (length(sig) == 1L) return(as.character(sig))
    paste(sort(as.character(sig)), collapse = " || ")
  }

  .build_exact_solution_summary <- function(by_run, baseline, monitored_solutions) {
    out <- vector("list", length(monitored_solutions))
    names(out) <- monitored_solutions

    for (solution_type in monitored_solutions) {
      ok_runs <- by_run[vapply(
        by_run,
        function(x) identical(x$status, "ok") && identical(x$reduced$solution_type_status[[solution_type]], "ok"),
        logical(1)
      )]
      baseline_sig <- baseline$sig[[solution_type]]
      baseline_key <- .sig_to_key(baseline_sig)

      keys <- vapply(
        ok_runs,
        function(x) .sig_to_key(x$reduced$sig[[solution_type]]),
        character(1)
      )

      freq <- sort(table(keys), decreasing = TRUE)

      out[[solution_type]] <- list(
        baseline_solution_expression = baseline_sig,
        baseline_key = baseline_key,
        n_successful = length(ok_runs),
        n_exact = sum(keys == baseline_key, na.rm = TRUE),
        prop_exact = if (length(ok_runs) == 0L) NA_real_ else mean(keys == baseline_key, na.rm = TRUE),
        frequencies = freq
      )
    }

    out
  }

  .build_term_stability_summary <- function(by_run, baseline, monitored_solutions) {
    out <- vector("list", length(monitored_solutions))
    names(out) <- monitored_solutions

    for (solution_type in monitored_solutions) {
      ok_runs <- by_run[vapply(
        by_run,
        function(x) identical(x$status, "ok") && identical(x$reduced$solution_type_status[[solution_type]], "ok"),
        logical(1)
      )]
      baseline_terms <- .solution_type_term_set(baseline, solution_type)

      run_terms <- lapply(ok_runs, function(x) .solution_type_term_set(x$reduced, solution_type))
      all_terms <- sort(unique(c(baseline_terms, unlist(run_terms, use.names = FALSE))))

      if (length(all_terms) == 0L) {
        out[[solution_type]] <- data.frame(
          term = character(0),
          baseline = logical(0),
          appearance_rate = numeric(0),
          stringsAsFactors = FALSE
        )
      } else {
        appearance <- vapply(
          all_terms,
          function(term) {
            if (length(run_terms) == 0L) return(NA_real_)
            mean(vapply(run_terms, function(x) term %in% x, logical(1)))
          },
          numeric(1)
        )

        out[[solution_type]] <- data.frame(
          term = all_terms,
          baseline = all_terms %in% baseline_terms,
          appearance_rate = appearance,
          stringsAsFactors = FALSE
        )
      }
    }

    out
  }

  .build_fit_summary <- function(by_run, baseline, monitored_solutions) {
    if (length(fit_measures) == 0L) return(list())

    out <- vector("list", length(monitored_solutions))
    names(out) <- monitored_solutions

    for (solution_type in monitored_solutions) {
      ok_runs <- by_run[vapply(
        by_run,
        function(x) identical(x$status, "ok") && identical(x$reduced$solution_type_status[[solution_type]], "ok"),
        logical(1)
      )]
      base_fit <- baseline$fit[[solution_type]]
      fit_names <- sort(unique(c(names(base_fit), unlist(lapply(ok_runs, function(x) names(x$reduced$fit[[solution_type]])), use.names = FALSE))))

      if (length(fit_names) == 0L) {
        out[[solution_type]] <- data.frame(
          measure = character(0),
          baseline = numeric(0),
          median = numeric(0),
          q25 = numeric(0),
          q75 = numeric(0),
          median_delta = numeric(0),
          q25_delta = numeric(0),
          q75_delta = numeric(0),
          prop_changed = numeric(0),
          stringsAsFactors = FALSE
        )
        next
      }

      rows <- lapply(
        fit_names,
        function(nm) {
          vals <- vapply(ok_runs, function(x) {
            v <- .fit_get(x$reduced$fit[[solution_type]], nm)
            if (is.null(v)) NA_real_ else as.numeric(v)
          }, numeric(1))

          base_v <- .fit_get(base_fit, nm)
          if (is.null(base_v)) base_v <- NA_real_
          deltas <- vals - base_v

          data.frame(
            measure = nm,
            baseline = as.numeric(base_v),
            median = if (all(is.na(vals))) NA_real_ else stats::median(vals, na.rm = TRUE),
            q25 = if (all(is.na(vals))) NA_real_ else as.numeric(stats::quantile(vals, probs = 0.25, na.rm = TRUE, names = FALSE)),
            q75 = if (all(is.na(vals))) NA_real_ else as.numeric(stats::quantile(vals, probs = 0.75, na.rm = TRUE, names = FALSE)),
            median_delta = if (all(is.na(deltas))) NA_real_ else stats::median(deltas, na.rm = TRUE),
            q25_delta = if (all(is.na(deltas))) NA_real_ else as.numeric(stats::quantile(deltas, probs = 0.25, na.rm = TRUE, names = FALSE)),
            q75_delta = if (all(is.na(deltas))) NA_real_ else as.numeric(stats::quantile(deltas, probs = 0.75, na.rm = TRUE, names = FALSE)),
            prop_changed = if (length(vals) == 0L) NA_real_ else mean(abs(deltas) > fit_tol, na.rm = TRUE),
            stringsAsFactors = FALSE
          )
        }
      )

      out[[solution_type]] <- do.call(rbind, rows)
      rownames(out[[solution_type]]) <- NULL
    }

    out
  }

  .build_similarity_summary <- function(by_run, baseline, monitored_solutions) {
    rows <- lapply(
      monitored_solutions,
      function(solution_type) {
        ok_runs <- by_run[vapply(
          by_run,
          function(x) identical(x$status, "ok") && identical(x$reduced$solution_type_status[[solution_type]], "ok"),
          logical(1)
        )]
        base_terms <- .solution_type_term_set(baseline, solution_type)
        run_term_sets <- lapply(ok_runs, function(x) .solution_type_term_set(x$reduced, solution_type))

        baseline_j <- vapply(run_term_sets, function(x) .jaccard(base_terms, x), numeric(1))

        pairwise_avg <- NA_real_
        if (length(run_term_sets) >= 2L) {
          combs <- utils::combn(seq_along(run_term_sets), 2L)
          pair_vals <- apply(
            combs,
            2L,
            function(ii) .jaccard(run_term_sets[[ii[1L]]], run_term_sets[[ii[2L]]])
          )
          pairwise_avg <- mean(pair_vals, na.rm = TRUE)
        }

        data.frame(
          solution_type = solution_type,
          n_successful = length(ok_runs),
          mean_baseline_jaccard = if (length(baseline_j) == 0L) NA_real_ else mean(baseline_j, na.rm = TRUE),
          median_baseline_jaccard = if (length(baseline_j) == 0L) NA_real_ else stats::median(baseline_j, na.rm = TRUE),
          avg_pairwise_jaccard = pairwise_avg,
          stringsAsFactors = FALSE
        )
      }
    )

    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    out
  }

  .build_calibration_summary <- function(by_run, baseline, analysis_vars) {
    if (calib != "recompute") return(NULL)

    ok_runs <- by_run[vapply(by_run, function(x) identical(x$status, "ok"), logical(1))]
    rows <- list()

    for (nm in analysis_vars) {
      base_th <- baseline$calibration$thresholds_used[[nm]]
      run_th_list <- lapply(ok_runs, function(x) x$calibration_run$thresholds_used[[nm]])

      all_lengths <- c(length(base_th), vapply(run_th_list, length, integer(1)))
      kmax <- max(all_lengths, 0L)
      if (kmax == 0L) next

      th_names <- names(base_th)
      if (is.null(th_names) || length(th_names) != length(base_th) || any(!nzchar(th_names))) {
        th_names <- paste0("T", seq_len(max(length(base_th), kmax)))
      }
      if (length(th_names) < kmax) {
        th_names <- c(th_names, paste0("T", seq(from = length(th_names) + 1L, to = kmax)))
      }

      for (k in seq_len(kmax)) {
        vals <- vapply(
          run_th_list,
          function(x) {
            if (length(x) < k) return(NA_real_)
            as.numeric(x[[k]])
          },
          numeric(1)
        )

        base_v <- if (length(base_th) >= k) as.numeric(base_th[[k]]) else NA_real_

        rows[[length(rows) + 1L]] <- data.frame(
          variable = nm,
          threshold = th_names[k],
          baseline = base_v,
          median = if (all(is.na(vals))) NA_real_ else stats::median(vals, na.rm = TRUE),
          q25 = if (all(is.na(vals))) NA_real_ else as.numeric(stats::quantile(vals, probs = 0.25, na.rm = TRUE, names = FALSE)),
          q75 = if (all(is.na(vals))) NA_real_ else as.numeric(stats::quantile(vals, probs = 0.75, na.rm = TRUE, names = FALSE)),
          prop_diff_baseline = if (length(vals) == 0L || is.na(base_v)) NA_real_ else mean(abs(vals - base_v) > 0, na.rm = TRUE),
          n_successful = length(ok_runs),
          stringsAsFactors = FALSE
        )
      }
    }

    if (length(rows) == 0L) return(NULL)
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    out
  }

  summary <- list(
    exact_solution = .build_exact_solution_summary(by_run, baseline, baseline_solution_types),
    term_stability = .build_term_stability_summary(by_run, baseline, baseline_solution_types),
    fit_stability = .build_fit_summary(by_run, baseline, baseline_fit_solution_types),
    similarity = .build_similarity_summary(by_run, baseline, baseline_solution_types),
    calibration = .build_calibration_summary(by_run, baseline, analysis_vars)
  )

  .new_result_object(
    "subsample_test",
    diagnostics = diagnostics,
    results = .make_subsample_results(diagnostics),
    summary = summary,
    baseline = baseline,
    by_run = by_run,
    settings = list(
      outcome = outcome,
      conditions = conditions,
      calib = calib,
      sample_n = sample_n,
      sample_prop_requested = sample_prop,
      sample_prop_realized = sample_prop_realized,
      reps = reps,
      stratify = stratify,
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
      fit_tol = fit_tol,
      seed = seed
    )
  )
}

#' Print a `subsample_test` object
#'
#' @rdname subsample.test
#' @export
print.subsample_test <- function(x, row.names = FALSE, ...) {
  results <- x$results
  settings <- x$settings
  summary <- x$summary

  .print_qcaert_heading("subsample_test", "subsample robustness", settings)
  if (!is.null(settings$reps)) {
    cat("Replications: ", settings$reps, "\n", sep = "")
  }
  if (!is.null(settings$sample_n)) {
    cat("Sample size: ", settings$sample_n, "\n", sep = "")
  }
  if (!is.null(settings$sample_prop_realized)) {
    cat("Sample proportion: ", format(settings$sample_prop_realized, trim = TRUE), "\n", sep = "")
  }
  if (!is.null(settings$stratify)) {
    cat("Stratification: ", settings$stratify, "\n", sep = "")
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
    cat(" Runs with any solution change: ", n_solution_changed, "\n", sep = "")
    cat(" Runs with any fit change: ", n_fit_changed, "\n", sep = "")
    cat(" Non-ok runs: ", n_errors, "\n", sep = "")

    if (any(!is.na(results$term_jaccard_baseline))) {
      cat(
        " Mean combined term-set Jaccard vs baseline: ",
        format(mean(results$term_jaccard_baseline, na.rm = TRUE), trim = TRUE),
        "\n",
        sep = ""
      )
    }
  }

  if (!is.null(summary$exact_solution) && length(summary$exact_solution) > 0L) {
    exact_rows <- lapply(
      names(summary$exact_solution),
      function(solution_type) {
        sx <- summary$exact_solution[[solution_type]]
        data.frame(
          solution_type = .compact_solution_type_names(solution_type),
          exact = as.integer(sx$n_exact),
          successful = as.integer(sx$n_successful),
          proportion = as.numeric(sx$prop_exact),
          stringsAsFactors = FALSE
        )
      }
    )

    exact_table <- do.call(rbind, exact_rows)
    rownames(exact_table) <- NULL
    .print_qcaert_table(exact_table, "Exact baseline matches by solution type", row.names = FALSE)
  }

  invisible(x)
}

#' Return the main results table from a `subsample_test` object
#'
#' @rdname subsample.test
#' @export
as.data.frame.subsample_test <- function(x, ...) {
  .as.data.frame_results(x, ...)
}
