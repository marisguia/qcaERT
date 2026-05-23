.reduced_error_message <- function(e) {
  if (!inherits(e, "error")) return(NA_character_)
  tryCatch(conditionMessage(e), error = function(...) as.character(e))
}

.reduced_extract_standard_fit <- function(res, solution_type, which_M, fit_measures) {
  if (length(fit_measures) == 0L) {
    return(list(
      fit = NULL,
      fit_missing = FALSE,
      fit_available = character(0),
      fit_missing_measures = character(0),
      meta = list(n_fit_rows = NA_integer_)
    ))
  }

  ic <- res$IC
  tbl <- if (is.list(ic) && "sol.incl.cov" %in% names(ic)) ic$sol.incl.cov else NULL

  if (is.null(tbl) || !is.data.frame(tbl) || nrow(tbl) == 0L) {
    return(list(
      fit = NULL,
      fit_missing = TRUE,
      fit_available = character(0),
      fit_missing_measures = fit_measures,
      meta = list(n_fit_rows = 0L)
    ))
  }

  available <- intersect(fit_measures, colnames(tbl))
  missing_measures <- setdiff(fit_measures, available)

  if (nrow(tbl) < which_M) {
    return(list(
      fit = NULL,
      fit_missing = TRUE,
      fit_available = available,
      fit_missing_measures = fit_measures,
      meta = list(n_fit_rows = nrow(tbl))
    ))
  }

  vals <- suppressWarnings(as.numeric(tbl[which_M, available, drop = TRUE]))
  names(vals) <- paste0(solution_type, ".", available)

  list(
    fit = vals,
    fit_missing = FALSE,
    fit_available = available,
    fit_missing_measures = missing_measures,
    meta = list(n_fit_rows = nrow(tbl))
  )
}

.reduced_extract_intermediate_fit <- function(res, solution_type, which_M, i_mode, fit_measures) {
  if (length(fit_measures) == 0L) {
    return(list(
      fit = NULL,
      fit_missing = FALSE,
      fit_available = character(0),
      fit_missing_measures = character(0),
      meta = list(n_i = NA_integer_, n_fit_rows = NA_integer_)
    ))
  }

  i.sol <- res$i.sol

  if (is.null(i.sol) || length(i.sol) == 0L) {
    return(list(
      fit = NULL,
      fit_missing = TRUE,
      fit_available = character(0),
      fit_missing_measures = fit_measures,
      meta = list(n_i = 0L, n_fit_rows = 0L)
    ))
  }

  nm <- .normalize_i_solution_names(i.sol)

  if (i_mode == "C1P1") {
    idx <- which(nm == "C1P1")
    if (length(idx) == 0L) {
      return(list(
        fit = NULL,
        fit_missing = TRUE,
        fit_available = character(0),
        fit_missing_measures = fit_measures,
        meta = list(n_i = length(i.sol), n_fit_rows = 0L)
      ))
    }
    i.sol <- i.sol[idx[1L]]
    nm <- nm[idx[1L]]
  }

  out <- numeric(0)
  missing_any <- FALSE
  avail_any <- character(0)
  fit_rows <- integer(0)

  for (k in seq_along(i.sol)) {
    el <- i.sol[[k]]
    ic <- if (is.list(el) && "IC" %in% names(el)) el$IC else NULL
    tbl <- if (is.list(ic) && "sol.incl.cov" %in% names(ic)) ic$sol.incl.cov else NULL

    if (is.null(tbl) || !is.data.frame(tbl) || nrow(tbl) == 0L) {
      missing_any <- TRUE
      fit_rows <- c(fit_rows, 0L)
      next
    }

    fit_rows <- c(fit_rows, nrow(tbl))
    available <- intersect(fit_measures, colnames(tbl))
    avail_any <- union(avail_any, available)

    if (nrow(tbl) < which_M) {
      missing_any <- TRUE
    } else {
      vals <- suppressWarnings(as.numeric(tbl[which_M, available, drop = TRUE]))
      names(vals) <- paste0(solution_type, ".", nm[k], ".", available)
      out <- c(out, vals)
    }
  }

  list(
    fit = if (length(out) == 0L) NULL else out,
    fit_missing = missing_any && length(out) == 0L,
    fit_available = avail_any,
    fit_missing_measures = setdiff(fit_measures, avail_any),
    meta = list(n_i = length(i.sol), n_fit_rows = fit_rows)
  )
}

.reduced_extract_solution_type_fit <- function(res, solution_type, which_M, i_mode, fit_measures) {
  if (solution_type %in% c("conservative", "parsimonious")) {
    out <- .reduced_extract_standard_fit(
      res = res,
      solution_type = solution_type,
      which_M = which_M,
      fit_measures = fit_measures
    )
    out$solution_type <- solution_type
    return(out)
  }

  out <- .reduced_extract_intermediate_fit(
    res,
    solution_type = solution_type,
    which_M = which_M,
    i_mode = i_mode,
    fit_measures = fit_measures
  )
  out$solution_type <- solution_type
  out
}

.reduced_as_df <- function(x) {
  if (is.data.frame(x)) return(x)
  as.data.frame(x, stringsAsFactors = FALSE)
}

.reduced_validate_calib_spec <- function(calib_spec, analysis_vars, raw.data, data) {
  if (!is.character(analysis_vars) || length(analysis_vars) < 2L) {
    stop("When `calib = \"recompute\"`, `analysis_vars` must contain the outcome and at least one condition.")
  }

  raw_df <- .reduced_as_df(raw.data)
  data_df <- .reduced_as_df(data)

  for (nm in analysis_vars) {
    if (!nm %in% colnames(data_df)) {
      stop(sprintf("Calibrated set '%s' is not a column in `data`.", nm))
    }
  }

  findTh_specs <- stats::setNames(vector("list", length(analysis_vars)), analysis_vars)
  calib_spec_shared <- calib_spec

  if (!is.null(calib_spec_shared) && is.list(calib_spec_shared) && !is.data.frame(calib_spec_shared)) {
    for (nm in names(calib_spec_shared)) {
      spec <- calib_spec_shared[[nm]]
      if (!is.list(spec) || is.data.frame(spec)) {
        next
      }

      if ("findTh" %in% names(spec)) {
        if (!is.null(spec$findTh) && !is.list(spec$findTh)) {
          stop(sprintf("`calib_spec[['%s']]$findTh` must be NULL or a list of `QCA::findTh()` arguments.", nm))
        }
        if (nm %in% analysis_vars) {
          findTh_specs[[nm]] <- spec$findTh
        }
        spec$findTh <- NULL
        calib_spec_shared[[nm]] <- spec
      }
    }
  }

  outcome <- analysis_vars[[1L]]
  conditions <- analysis_vars[-1L]

  calib_spec_shared <- .normalize_calib_specs(
    conditions = conditions,
    outcome = outcome,
    calib_spec = calib_spec_shared,
    test.outcome = TRUE,
    caller = "reduced-run recalibration"
  )
  calib_spec_shared <- calib_spec_shared[analysis_vars]

  raw_sources <- vapply(calib_spec_shared, function(x) x$raw, character(1))
  missing_raw <- setdiff(raw_sources, colnames(raw_df))
  if (length(missing_raw) > 0L) {
    stop(
      "All raw columns referenced by the calibration specification must exist in `raw.data`. Missing: ",
      paste(missing_raw, collapse = ", "),
      "."
    )
  }

  for (nm in analysis_vars) {
    calib_spec_shared[[nm]]$findTh <- findTh_specs[[nm]]
  }

  calib_spec_shared
}

.reduced_thresholds_from_findTh <- function(raw_x, spec, set_name) {
  ft_args <- c(list(x = raw_x), spec$findTh)
  thresholds_used <- tryCatch(
    suppressWarnings(do.call(QCA::findTh, ft_args)),
    error = function(e) e
  )

  if (inherits(thresholds_used, "error")) {
    return(thresholds_used)
  }

  tryCatch(
    .normalize_calib_thresholds(
      thresholds = thresholds_used,
      type_fc = spec$type,
      method = spec$method,
      condition = set_name,
      caller = "reduced-run recalibration"
    ),
    error = function(e) e
  )
}

.reduced_calibrate_from_spec <- function(raw_x, spec, thresholds) {
  args <- list(
    x = raw_x,
    type = spec$qca_type,
    thresholds = unname(thresholds)
  )

  if (identical(spec$type, "f")) {
    args$method <- spec$method
  }

  do.call(QCA::calibrate, c(args, spec$calibrate))
}

.reduced_recalibrate_dataset <- function(data_step, raw_step, analysis_vars, calib_spec) {
  data_out <- .reduced_as_df(data_step)
  raw_step <- .reduced_as_df(raw_step)

  realized_thresholds <- stats::setNames(vector("list", length(analysis_vars)), analysis_vars)
  realized_calls <- stats::setNames(vector("list", length(analysis_vars)), analysis_vars)

  for (nm in analysis_vars) {
    spec <- calib_spec[[nm]]
    raw_x <- raw_step[[spec$raw]]
    thresholds_used <- spec$thresholds

    if (!is.null(spec$findTh)) {
      thresholds_used <- .reduced_thresholds_from_findTh(
        raw_x = raw_x,
        spec = spec,
        set_name = nm
      )

      if (inherits(thresholds_used, "error")) {
        return(list(
          error = thresholds_used,
          error_source = paste0("findTh:", nm),
          error_message = .reduced_error_message(thresholds_used),
          data = NULL,
          thresholds = realized_thresholds,
          calls = realized_calls
        ))
      }
    }

    cal_res <- tryCatch(
      suppressWarnings(
        .reduced_calibrate_from_spec(
          raw_x = raw_x,
          spec = spec,
          thresholds = thresholds_used
        )
      ),
      error = function(e) e
    )

    if (inherits(cal_res, "error")) {
      return(list(
        error = cal_res,
        error_source = paste0("calibrate:", nm),
        error_message = .reduced_error_message(cal_res),
        data = NULL,
        thresholds = realized_thresholds,
        calls = realized_calls
      ))
    }

    data_out[[nm]] <- cal_res
    realized_thresholds[[nm]] <- thresholds_used
    realized_calls[[nm]] <- list(
      raw = spec$raw,
      type = spec$type,
      method = spec$method,
      findTh = spec$findTh,
      thresholds = thresholds_used,
      calibrate = spec$calibrate
    )
  }

  list(
    error = NULL,
    error_source = NA_character_,
    error_message = NA_character_,
    data = data_out,
    thresholds = realized_thresholds,
    calls = realized_calls
  )
}

.reduced_blank_payload <- function() {
  list(
    res = list(conservative = NULL, parsimonious = NULL, intermediate = NULL),
    sig = list(conservative = NULL, parsimonious = NULL, intermediate = NULL),
    selected_solution_missing = c(conservative = NA, parsimonious = NA, intermediate = NA),
    fit = list(conservative = NULL, parsimonious = NULL, intermediate = NULL),
    fit_missing = c(conservative = NA, parsimonious = NA, intermediate = NA),
    meta = list(conservative = NULL, parsimonious = NULL, intermediate = NULL),
    solution_type_status = c(conservative = NA_character_, parsimonious = NA_character_, intermediate = NA_character_),
    solution_type_error_source = c(conservative = NA_character_, parsimonious = NA_character_, intermediate = NA_character_),
    solution_type_error_message = c(conservative = NA_character_, parsimonious = NA_character_, intermediate = NA_character_)
  )
}

.reduced_make_run_result <- function(
    status,
    tt = NULL,
    exclude_used = NULL,
    calibration = NULL,
    analysis_data = NULL,
    include_analysis_data = FALSE,
    payload = NULL,
    error_source = NA_character_,
    error_message = NA_character_
) {
  if (is.null(payload)) payload <- .reduced_blank_payload()

  head <- list(
    status = status,
    tt = tt,
    exclude_used = exclude_used,
    calibration = calibration
  )

  if (isTRUE(include_analysis_data)) {
    head$analysis_data <- analysis_data
  }

  c(
    head,
    payload,
    list(
      error_source = error_source,
      error_message = error_message
    )
  )
}

.reduced_payload_status <- function(payload, monitored_solutions) {
  solution_type_status <- payload$solution_type_status[monitored_solutions]

  if (any(solution_type_status == "ok", na.rm = TRUE)) {
    return(list(
      status = "ok",
      error_source = NA_character_,
      error_message = NA_character_
    ))
  }

  first_bad <- which(!is.na(solution_type_status) & nzchar(solution_type_status))[1L]

  if (is.na(first_bad)) {
    return(list(
      status = "run_error",
      error_source = NA_character_,
      error_message = NA_character_
    ))
  }

  solution_type <- monitored_solutions[first_bad]

  list(
    status = unname(payload$solution_type_status[[solution_type]]),
    error_source = unname(payload$solution_type_error_source[[solution_type]]),
    error_message = unname(payload$solution_type_error_message[[solution_type]])
  )
}

.reduced_compute_exclude <- function(tt_obj, exclude_mode, exclude_recompute, exclude_static) {
  .qcaert_compute_exclude(
    tt_obj = tt_obj,
    exclude_mode = exclude_mode,
    exclude_recompute = exclude_recompute,
    exclude_static = exclude_static
  )
}

.reduced_run_solution_type <- function(
    tt_obj,
    solution_type,
    ex,
    dots_min,
    dir.exp,
    which_M,
    i_mode,
    fit_measures
) {
  args <- c(
    list(
      input = tt_obj,
      include = if (solution_type == "conservative") "" else "?"
    ),
    dots_min
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
      selected_solution_missing = NA,
      fit = NULL,
      fit_missing = NA,
      meta = NULL
    ))
  }

  sig_info <- .extract_solution_type_sig(
    res,
    solution_type = solution_type,
    which_M = which_M,
    i_mode = i_mode
  )

  fit_info <- .reduced_extract_solution_type_fit(
    res,
    solution_type = solution_type,
    which_M = which_M,
    i_mode = i_mode,
    fit_measures = fit_measures
  )

  list(
    error = NULL,
    res = res,
    sig = sig_info$sig,
    selected_solution_missing = isTRUE(sig_info$selected_solution_missing),
    fit = fit_info$fit,
    fit_missing = isTRUE(fit_info$fit_missing),
    meta = list(
      solution = sig_info$meta,
      fit = fit_info$meta,
      fit_available = fit_info$fit_available,
      fit_missing_measures = fit_info$fit_missing_measures
    )
  )
}

.reduced_run_once <- function(
    data_step,
    raw_step = NULL,
    calib,
    recalibrate_dataset,
    build_truth_table,
    monitored_solutions,
    exclude_mode,
    exclude_recompute,
    exclude_static,
    dots_min,
    dir.exp,
    which_M,
    i_mode,
    fit_measures,
    include_analysis_data = FALSE
) {
  analysis_data <- .reduced_as_df(data_step)
  calibration_info <- list(
    mode = calib,
    status = if (calib == "fixed") "fixed" else "ok",
    thresholds_used = NULL,
    calls_used = NULL
  )

  if (calib == "recompute") {
    rc <- recalibrate_dataset(data_step = data_step, raw_step = raw_step)

    if (!is.null(rc$error)) {
      calibration_info$status <- "error"
      calibration_info$thresholds_used <- rc$thresholds
      calibration_info$calls_used <- rc$calls

      return(.reduced_make_run_result(
        status = "calibration_error",
        tt = NULL,
        exclude_used = NULL,
        calibration = calibration_info,
        analysis_data = NULL,
        include_analysis_data = include_analysis_data,
        error_source = rc$error_source,
        error_message = rc$error_message
      ))
    }

    analysis_data <- rc$data
    calibration_info$status <- "ok"
    calibration_info$thresholds_used <- rc$thresholds
    calibration_info$calls_used <- rc$calls
  }

  tt_obj <- tryCatch(
    suppressWarnings(build_truth_table(analysis_data)),
    error = function(e) e
  )

  if (inherits(tt_obj, "error")) {
    return(.reduced_make_run_result(
      status = "truth_table_error",
      tt = NULL,
      exclude_used = NULL,
      calibration = calibration_info,
      analysis_data = analysis_data,
      include_analysis_data = include_analysis_data,
      error_source = "truthTable",
      error_message = .reduced_error_message(tt_obj)
    ))
  }

  need_exclude <- any(monitored_solutions %in% c("parsimonious", "intermediate")) &&
    exclude_mode != "none"

  ex <- NULL
  ex_error <- NULL
  if (need_exclude) {
    ex <- .reduced_compute_exclude(
      tt_obj = tt_obj,
      exclude_mode = exclude_mode,
      exclude_recompute = exclude_recompute,
      exclude_static = exclude_static
    )

    if (inherits(ex, "error")) {
      ex_error <- ex
      ex <- NULL
    }
  }

  payload <- .reduced_blank_payload()

  for (solution_type in monitored_solutions) {
    if (solution_type %in% c("parsimonious", "intermediate") && inherits(ex_error, "error")) {
      payload$solution_type_status[[solution_type]] <- "exclude_error"
      payload$solution_type_error_source[[solution_type]] <- "exclude"
      payload$solution_type_error_message[[solution_type]] <- .reduced_error_message(ex_error)
      next
    }

    rr <- .reduced_run_solution_type(
      tt_obj = tt_obj,
      solution_type = solution_type,
      ex = ex,
      dots_min = dots_min,
      dir.exp = dir.exp,
      which_M = which_M,
      i_mode = i_mode,
      fit_measures = fit_measures
    )

    if (!is.null(rr$error)) {
      payload$solution_type_status[[solution_type]] <- "minimize_error"
      payload$solution_type_error_source[[solution_type]] <- solution_type
      payload$solution_type_error_message[[solution_type]] <- .reduced_error_message(rr$error)
      next
    }

    payload$res[[solution_type]] <- rr$res
    payload$sig[[solution_type]] <- rr$sig
    payload$selected_solution_missing[[solution_type]] <- isTRUE(rr$selected_solution_missing)
    payload$fit[[solution_type]] <- rr$fit
    payload$fit_missing[[solution_type]] <- isTRUE(rr$fit_missing)
    payload$meta[[solution_type]] <- rr$meta
    payload$solution_type_status[[solution_type]] <- "ok"
    payload$solution_type_error_source[[solution_type]] <- NA_character_
    payload$solution_type_error_message[[solution_type]] <- NA_character_
  }

  run_status <- .reduced_payload_status(payload, monitored_solutions)

  .reduced_make_run_result(
    status = run_status$status,
    tt = tt_obj,
    exclude_used = ex,
    calibration = calibration_info,
    analysis_data = analysis_data,
    include_analysis_data = include_analysis_data,
    payload = payload,
    error_source = run_status$error_source,
    error_message = run_status$error_message
  )
}

.reduced_comparable_solution_types <- function(base, cur, monitored_solutions) {
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

.reduced_baseline_solution_types <- function(base, monitored_solutions, fit_measures = character(0), require_fit = FALSE) {
  if (is.null(base$solution_type_status)) {
    if (!identical(base$status, "ok")) {
      return(character(0))
    }
    status_ok <- rep(TRUE, length(monitored_solutions))
  } else {
    status_ok <- base$solution_type_status[monitored_solutions] == "ok"
    status_ok[is.na(status_ok)] <- FALSE
  }

  selected_solution_ok <- !(base$selected_solution_missing[monitored_solutions] %in% TRUE)
  selected_solution_ok[is.na(selected_solution_ok)] <- FALSE

  out <- monitored_solutions[status_ok & selected_solution_ok]

  if (isTRUE(require_fit) && length(fit_measures) > 0L) {
    fit_ok <- !(base$fit_missing[out] %in% TRUE)
    fit_ok[is.na(fit_ok)] <- FALSE
    out <- out[fit_ok]
  }

  out
}

.reduced_no_comparable_status <- function(cur, candidate_solution_types) {
  if (!identical(cur$status, "ok")) {
    return(list(
      status = .reduced_stop_reason_from_status(cur$status),
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
        status = .reduced_stop_reason_from_status(cur$solution_type_status[[solution_type]]),
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

.reduced_solution_change_info <- function(base, cur, monitored_solutions) {
  comparable_solutions <- .reduced_comparable_solution_types(
    base = base,
    cur = cur,
    monitored_solutions = monitored_solutions
  )

  if (length(comparable_solutions) == 0L) {
    return(list(
      changed = FALSE,
      changed_types = NA_character_,
      change_kind = NA_character_
    ))
  }

  changed_flags <- vapply(
    comparable_solutions,
    function(solution_type) .sig_changed(base$sig[[solution_type]], cur$sig[[solution_type]]),
    logical(1)
  )

  changed <- any(changed_flags)

  if (!changed) {
    return(list(
      changed = FALSE,
      changed_types = NA_character_,
      change_kind = NA_character_
    ))
  }

  changed_types <- paste(comparable_solutions[changed_flags], collapse = ",")
  change_kind <- paste(
    vapply(
      comparable_solutions[changed_flags],
      function(solution_type) {
        paste0(solution_type, ":", .change_kind_sig(base$sig[[solution_type]], cur$sig[[solution_type]]))
      },
      character(1)
    ),
    collapse = ","
  )

  list(
    changed = TRUE,
    changed_types = changed_types,
    change_kind = change_kind
  )
}

.reduced_fit_change_info <- function(base, cur, monitored_solutions, fit_tol, fit_measures) {
  if (length(fit_measures) == 0L) {
    return(list(
      changed = FALSE,
      changed_types = NA_character_,
      changed_measures = character(0),
      delta = numeric(0),
      n_changed = 0L,
      max_abs_delta = NA_real_
    ))
  }

  comparable_solutions <- .reduced_comparable_solution_types(
    base = base,
    cur = cur,
    monitored_solutions = monitored_solutions
  )

  if (length(comparable_solutions) == 0L) {
    return(list(
      changed = FALSE,
      changed_types = NA_character_,
      changed_measures = character(0),
      delta = numeric(0),
      n_changed = 0L,
      max_abs_delta = NA_real_
    ))
  }

  changed_flags <- vapply(
    comparable_solutions,
    function(solution_type) .fit_changed(base$fit[[solution_type]], cur$fit[[solution_type]], tol = fit_tol),
    logical(1)
  )

  changed <- any(changed_flags)

  if (!changed) {
    return(list(
      changed = FALSE,
      changed_types = NA_character_,
      changed_measures = character(0),
      delta = numeric(0),
      n_changed = 0L,
      max_abs_delta = 0
    ))
  }

  changed_types <- paste(comparable_solutions[changed_flags], collapse = ",")

  changed_measures <- unlist(
    lapply(
      comparable_solutions[changed_flags],
      function(solution_type) .fit_change_names(base$fit[[solution_type]], cur$fit[[solution_type]], tol = fit_tol)
    ),
    use.names = FALSE
  )

  delta_vec <- unlist(
    lapply(
      comparable_solutions[changed_flags],
      function(solution_type) {
        keep_names <- .fit_change_names(base$fit[[solution_type]], cur$fit[[solution_type]], tol = fit_tol)
        dv <- .fit_delta_vec(base$fit[[solution_type]], cur$fit[[solution_type]])
        dv[names(dv) %in% keep_names]
      }
    ),
    use.names = TRUE
  )

  list(
    changed = TRUE,
    changed_types = changed_types,
    changed_measures = sort(unique(changed_measures)),
    delta = delta_vec,
    n_changed = length(unique(changed_measures)),
    max_abs_delta = .fit_abs_max(delta_vec)
  )
}

.reduced_stop_reason_from_status <- function(status, baseline = FALSE) {
  out <- switch(
    status,
    calibration_error = "calibration_error",
    truth_table_error = "truth_table_build_error",
    exclude_error = "exclude_recompute_error",
    minimize_error = "requested_minimize_error",
    status
  )

  if (isTRUE(baseline)) {
    out <- paste0("baseline_", out)
  }

  out
}

.reduced_resolve_case_labels <- function(data, case_labels = NULL) {
  n <- nrow(data)

  full_labels <- if (!is.null(case_labels)) {
    if (!is.character(case_labels) || length(case_labels) != n) {
      stop("`case_labels` must be NULL or a character vector of length `nrow(data)`.")
    }
    as.character(case_labels)
  } else if (!is.null(rownames(data)) && length(rownames(data)) == n) {
    as.character(rownames(data))
  } else {
    as.character(seq_len(n))
  }

  data.frame(
    row_index = seq_len(n),
    case_label = full_labels,
    stringsAsFactors = FALSE
  )
}

.reduced_resolve_case_reference <- function(cases, data, case_labels = NULL) {
  n <- nrow(data)
  all_cases <- .reduced_resolve_case_labels(data = data, case_labels = case_labels)
  full_labels <- all_cases$case_label

  if (is.null(cases)) {
    idx <- seq_len(n)
  } else if (is.numeric(cases)) {
    idx <- suppressWarnings(as.integer(cases))
    if (any(is.na(idx)) || any(idx < 1L | idx > n)) {
      stop("Numeric `cases` must be valid row indices.")
    }
  } else if (is.character(cases)) {
    idx <- match(cases, full_labels)
    if (any(is.na(idx))) {
      stop("Character `cases` contain labels not found in the available case labels / row names.")
    }
  } else {
    stop("`cases` must be NULL, a numeric vector of row indices, or a character vector of case labels.")
  }

  idx <- unique(idx)

  data.frame(
    row_index = idx,
    case_label = full_labels[idx],
    stringsAsFactors = FALSE
  )
}
