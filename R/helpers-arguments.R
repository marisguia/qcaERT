.dot_get <- function(dots, key) {
  if (length(dots) == 0L) return(NULL)
  nms <- names(dots)
  if (is.null(nms)) return(NULL)
  i <- which(!is.na(pmatch(nms, key)))
  if (length(i) == 0L) return(NULL)
  dots[[i[1L]]]
}

.require_qca <- function() {
  if (!requireNamespace("QCA", quietly = TRUE)) {
    stop("Package 'QCA' is required.")
  }

  invisible(TRUE)
}

.safe_eval_expr <- function(expr, envir = parent.frame()) {
  tryCatch(eval(expr, envir = envir), error = function(e) e)
}

.expr_to_chrvec <- function(expr) {
  if (is.null(expr)) return(NULL)
  if (is.character(expr)) return(as.character(expr))
  if (is.name(expr)) return(as.character(expr))
  if (is.call(expr) && identical(expr[[1L]], as.name("c"))) {
    args <- as.list(expr)[-1L]
    out <- unlist(lapply(args, .expr_to_chrvec), use.names = FALSE)
    return(out)
  }
  gsub("\\s+", "", paste(deparse(expr), collapse = ""))
}

.filter_named_list <- function(x, reserved) {
  if (length(x) == 0L) return(x)
  nms <- names(x)
  if (is.null(nms)) return(x)
  m <- pmatch(nms, reserved, duplicates.ok = TRUE)
  x[is.na(m)]
}

.minimize_reserved_args <- function() {
  c(
    "input", "data", "tt",
    "outcome", "conditions",
    "incl.cut", "n.cut",
    "include", "dir.exp", "direxp",
    "exclude", "omit"
  )
}

.filter_dots_for_minimize <- function(dots) {
  .filter_named_list(dots, .minimize_reserved_args())
}

.truth_table_forwarded_args <- function() {
  setdiff(
    names(formals(QCA::truthTable)),
    c("data", "outcome", "conditions", "incl.cut", "n.cut", "show.cases", "use.labels")
  )
}

.split_truth_table_minimize_dots <- function(dots) {
  if (length(dots) == 0L) {
    return(list(tt = dots, min = dots))
  }

  nms <- names(dots)
  if (is.null(nms)) {
    return(list(tt = list(), min = dots))
  }

  is_tt <- !is.na(match(nms, .truth_table_forwarded_args()))
  dots_tt <- dots[is_tt]
  dots_rest <- dots[!is_tt]
  dots_min <- .filter_dots_for_minimize(dots_rest)

  list(tt = dots_tt, min = dots_min)
}

.reject_exclusion_controls_in_dots <- function(dots, caller) {
  if (length(dots) == 0L) return(invisible(TRUE))
  nms <- names(dots)
  if (is.null(nms)) return(invisible(TRUE))

  blocked <- intersect(nms, c("exclude", "omit", "exclude_spec"))
  if (length(blocked) > 0L) {
    stop(
      "`", caller, "()` does not accept `",
      blocked[1L],
      "` through `...`. Use `exclude_static` for an already computed exclusion object ",
      "or `exclude_recompute` for a `QCA::findRows()` specification."
    )
  }

  invisible(TRUE)
}

.reject_calibration_inputs_in_dots <- function(dots, caller) {
  if (length(dots) == 0L) return(invisible(TRUE))
  nms <- names(dots)
  if (is.null(nms)) return(invisible(TRUE))

  blocked <- intersect(
    nms,
    c(
      "thresholds",
      "type",
      "raw_conditions",
      "raw.conditions",
      "raw_condition",
      "raw.condition",
      "raw_outcome",
      "raw.outcome",
      "calibrate_args",
      "calibrate.args",
      "calib_args",
      "calib.args"
    )
  )
  if (length(blocked) > 0L) {
    stop(
      "`", caller, "()` does not accept `",
      blocked[1L],
      "` through `...`. Calibration inputs must be supplied through `calib_spec`."
    )
  }

  invisible(TRUE)
}

.qcaert_compute_exclude <- function(tt_obj, exclude_mode, exclude_recompute, exclude_static) {
  if (exclude_mode == "none") {
    return(NULL)
  }

  if (exclude_mode == "static") {
    return(exclude_static)
  }

  fr_args <- exclude_recompute
  fr_args$obj <- tt_obj

  tryCatch(
    suppressWarnings(do.call(QCA::findRows, fr_args)),
    error = function(e) e
  )
}

.validate_exclusion_controls <- function(
    exclude_mode,
    exclude_recompute,
    exclude_static,
    exclude_recompute_supplied = FALSE,
    exclude_static_supplied = FALSE,
    monitored_solutions = NULL,
    solution = NULL,
    style = c("std", "plain")
) {
  style <- match.arg(style)

  if (!is.null(monitored_solutions)) {
    needs_exclusion <- any(monitored_solutions %in% c("parsimonious", "intermediate"))
  } else if (!is.null(solution)) {
    needs_exclusion <- solution %in% c("all", "parsimonious", "intermediate")
  } else {
    needs_exclusion <- TRUE
  }

  if (identical(exclude_mode, "none")) {
    if (isTRUE(exclude_recompute_supplied) || isTRUE(exclude_static_supplied) || !is.null(exclude_static)) {
      stop("`exclude_mode = \"none\"` cannot be combined with `exclude_recompute` or `exclude_static`.")
    }
    return(invisible(TRUE))
  }

  if (identical(exclude_mode, "static")) {
    if (isTRUE(exclude_recompute_supplied)) {
      stop("`exclude_mode = \"static\"` uses `exclude_static`, not `exclude_recompute`.")
    }
    if (isTRUE(needs_exclusion) && is.null(exclude_static)) {
      stop("`exclude_mode = \"static\"` requires `exclude_static` when parsimonious or intermediate solutions are monitored.")
    }
    return(invisible(TRUE))
  }

  if (!identical(exclude_mode, "recompute")) {
    return(invisible(TRUE))
  }

  if (!is.null(exclude_static)) {
    stop("`exclude_mode = \"recompute\"` uses `exclude_recompute`, not `exclude_static`.")
  }

  if (!needs_exclusion) return(invisible(TRUE))

  if (style == "std") {
    list_msg <- "`exclude_mode = \"recompute\"` requires `exclude_recompute` as a list of `QCA::findRows()` arguments."
    expr_msg <- "For `exclude_recompute$type` 0 or 1, `exclude_recompute$expression` must be supplied."
  } else {
    list_msg <- "exclude_mode='recompute' requires exclude_recompute (a list of arguments for QCA::findRows)."
    expr_msg <- "exclude_recompute$expression is required for findRows type 0 or 1."
  }

  if (is.null(exclude_recompute) || !is.list(exclude_recompute)) {
    stop(list_msg)
  }

  if (!is.null(exclude_recompute$type) &&
      is.numeric(exclude_recompute$type) &&
      any(exclude_recompute$type %in% c(0, 1)) &&
      is.null(exclude_recompute$expression)) {
    stop(expr_msg)
  }

  invisible(TRUE)
}
