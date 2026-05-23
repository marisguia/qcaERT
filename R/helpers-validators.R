.as_integerish_scalar <- function(x, name, min = NULL) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x)) {
    stop(sprintf("`%s` must be a single finite integer-like number.", name))
  }
  xi <- suppressWarnings(as.integer(x))
  if (is.na(xi) || abs(x - xi) > sqrt(.Machine$double.eps)) {
    stop(sprintf("`%s` must be integer-like.", name))
  }
  if (!is.null(min) && xi < min) {
    stop(sprintf("`%s` must be >= %s.", name, min))
  }
  xi
}

.coerce_which_M <- function(which_M) {
  .as_integerish_scalar(which_M, "which_M", min = 1L)
}

.coerce_unit_step <- function(
    unit_step,
    conds,
    raw.data,
    unit_step_divisor,
    calib_specs
) {
  if (!is.null(unit_step)) {
    if (!is.numeric(unit_step)) stop("unit_step must be numeric (scalar or vector aligned to conditions), or NULL to compute automatically.")
    if (length(unit_step) == 1L) {
      if (!is.finite(unit_step) || unit_step <= 0) stop("Scalar unit_step must be a finite number > 0.")
      out <- rep(as.numeric(unit_step), length(conds))
      names(out) <- conds
      return(out)
    }
    if (length(unit_step) != length(conds)) {
      stop("If unit_step is a vector, it must have length equal to the number of calibrated sets being stepped and be aligned to that order.")
    }
    if (any(!is.finite(unit_step)) || any(unit_step <= 0)) {
      stop("All per-set unit_step values must be finite numbers > 0.")
    }
    out <- as.numeric(unit_step)
    names(out) <- conds
    return(out)
  }

  if (is.null(unit_step_divisor)) {
    stop("If unit_step is NULL, unit_step_divisor must be provided.")
  }
  if (!is.numeric(unit_step_divisor) || length(unit_step_divisor) != 1L ||
      !is.finite(unit_step_divisor) || unit_step_divisor <= 0) {
    stop("unit_step_divisor must be a single finite number > 0.")
  }

  out <- numeric(length(conds))
  names(out) <- conds

  for (i in seq_along(conds)) {
    cond_name <- conds[i]
    spec <- calib_specs[[cond_name]]
    raw_name <- spec$raw
    thr <- as.numeric(spec$thresholds)
    type_i <- spec$type

    x_raw <- raw.data[[raw_name]]
    xmin <- suppressWarnings(min(x_raw, na.rm = TRUE))
    xmax <- suppressWarnings(max(x_raw, na.rm = TRUE))

    if (!is.finite(xmin) || !is.finite(xmax)) {
      stop("Cannot compute automatic unit_step: raw column '", raw_name, "' must have finite min and max.")
    }

    if (type_i == "f") {
      if (length(thr) < 1L || any(!is.finite(thr))) {
        stop("Automatic unit_step for fuzzy sets requires finite thresholds for set '", cond_name, "'.")
      }
      gaps <- diff(sort(unique(thr)))
      if (length(gaps) < 1L || any(gaps <= 0)) {
        stop("Automatic unit_step for fuzzy sets requires distinct finite thresholds for set '", cond_name, "'.")
      }
      base_span <- min(gaps)
    } else {
      if (length(thr) != 1L || !is.finite(thr)) {
        stop("Automatic unit_step for crisp sets requires one finite threshold for set '", cond_name, "'.")
      }
      if (thr < xmin || thr > xmax) {
        stop("Automatic unit_step for crisp set '", cond_name, "' requires the threshold to lie within the raw-data range.")
      }
      base_span <- min(thr - xmin, xmax - thr)
    }

    if (!is.finite(base_span) || base_span <= 0) {
      stop("Automatic unit_step produced a non-positive base span for set '", cond_name, "'.")
    }

    out[i] <- base_span / unit_step_divisor
  }

  out
}

.validate_qca_min <- function(obj, name) {
  if (is.null(obj)) return(invisible(TRUE))
  if (!inherits(obj, "QCA_min")) {
    stop("`", name, "` must be a `QCA_min` object or NULL.")
  }
  invisible(TRUE)
}

.validate_outcome_conditions_distinct <- function(outcome, conditions) {
  if (is.null(outcome) || length(outcome) < 1L) return(invisible(TRUE))
  if (is.null(conditions)) return(invisible(TRUE))
  if (any(outcome %in% conditions)) {
    stop("`outcome` must not be included in `conditions`; the outcome is handled separately from the causal conditions.")
  }
  invisible(TRUE)
}
