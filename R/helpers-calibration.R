.normalize_calib_type <- function(type, name = "type") {
  if (!is.character(type) || length(type) != 1L || is.na(type)) {
    stop("`", name, "` must be 'f', 'fuzzy', 'c', or 'crisp'.")
  }

  type <- tolower(trimws(type))
  if (type %in% c("f", "fuzzy")) return("f")
  if (type %in% c("c", "crisp")) return("c")

  stop("`", name, "` must be 'f', 'fuzzy', 'c', or 'crisp'.")
}

.qca_calib_type <- function(type_fc) {
  if (identical(type_fc, "c")) "crisp" else "fuzzy"
}

.normalize_calib_method <- function(method, type_fc, caller) {
  if (identical(type_fc, "c")) return("direct")
  if (is.null(method)) method <- "direct"
  if (!is.character(method) || length(method) != 1L || is.na(method)) {
    stop("`method` in calibration specifications must be 'direct' or 'indirect'.")
  }

  method <- tolower(trimws(method))
  if (!method %in% c("direct", "indirect")) {
    stop(
      caller,
      " supports threshold perturbation for QCA::calibrate() methods ",
      "'direct' and 'indirect'."
    )
  }

  method
}

.as_numeric_thresholds <- function(x, label) {
  nm <- names(x)
  out <- suppressWarnings(as.numeric(x))
  if (length(out) != length(x) || anyNA(out) || any(!is.finite(out))) {
    stop(label, " must contain finite numeric thresholds.")
  }
  names(out) <- nm
  out
}

.named_thresholds_are <- function(nms, expected) {
  if (is.null(nms) || any(!nzchar(nms))) return(FALSE)
  nms <- tolower(nms)
  length(nms) == length(expected) &&
    length(unique(nms)) == length(expected) &&
    all(expected %in% nms)
}

.normalize_calib_thresholds <- function(thresholds, type_fc, method, condition, caller) {
  label <- paste0("`calib_spec[['", condition, "']]$thresholds`")
  if (is.null(thresholds)) {
    stop(label, " must be supplied.")
  }

  thresholds <- .as_numeric_thresholds(thresholds, label)
  nms <- names(thresholds)

  if (identical(type_fc, "c")) {
    if (length(thresholds) != 1L) {
      stop(label, " must contain one threshold for crisp calibration.")
    }
    names(thresholds) <- "T"
    return(thresholds)
  }

  if (identical(method, "direct")) {
    if (!length(thresholds) %in% c(3L, 6L)) {
      stop(label, " must contain three or six thresholds for fuzzy direct calibration.")
    }

    if (length(thresholds) == 3L) {
      expected <- c("e", "c", "i")
      if (.named_thresholds_are(nms, expected)) {
        thresholds <- thresholds[match(expected, tolower(nms))]
      } else if (!is.null(nms) && any(nzchar(nms))) {
        stop(label, " names must be E, C, and I for fuzzy direct three-threshold calibration.")
      }
      names(thresholds) <- c("E", "C", "I")
    } else {
      expected <- c("e1", "c1", "i1", "i2", "c2", "e2")
      if (.named_thresholds_are(nms, expected)) {
        thresholds <- thresholds[match(expected, tolower(nms))]
      } else if (!is.null(nms) && any(nzchar(nms))) {
        stop(
          label,
          " names must be E1, C1, I1, I2, C2, and E2 ",
          "for fuzzy direct six-threshold calibration."
        )
      }
      names(thresholds) <- c("E1", "C1", "I1", "I2", "C2", "E2")
    }
  } else {
    if (length(thresholds) < 1L) {
      stop(label, " must contain at least one threshold for fuzzy indirect calibration.")
    }
    thresholds <- sort(unname(thresholds))
    names(thresholds) <- paste0("T", seq_along(thresholds))
  }

  if (!.valid_calib_thresholds(thresholds, type_fc, method)) {
    stop(label, " are not ordered consistently with ", caller, " calibration method '", method, "'.")
  }

  thresholds
}

.valid_calib_thresholds <- function(thresholds, type_fc, method) {
  thresholds <- as.numeric(thresholds)
  if (any(!is.finite(thresholds))) return(FALSE)

  if (identical(type_fc, "c")) {
    return(length(thresholds) == 1L)
  }

  if (identical(method, "indirect")) {
    return(length(thresholds) >= 1L && all(diff(thresholds) > 0))
  }

  if (length(thresholds) == 3L) {
    return(
      (thresholds[1] < thresholds[2] && thresholds[2] < thresholds[3]) ||
        (thresholds[3] < thresholds[2] && thresholds[2] < thresholds[1])
    )
  }

  if (length(thresholds) == 6L) {
    e1 <- thresholds[1]
    c1 <- thresholds[2]
    i1 <- thresholds[3]
    i2 <- thresholds[4]
    c2 <- thresholds[5]
    e2 <- thresholds[6]

    increasing <- e1 < c1 && c1 < i1 && i1 <= i2 && i2 < c2 && c2 < e2
    decreasing <- i1 < c1 && c1 < e1 && e1 <= e2 && e2 < c2 && c2 < i2
    return(increasing || decreasing)
  }

  FALSE
}

.normalize_calibrate_options <- function(calibrate, caller) {
  if (is.null(calibrate)) return(list())
  if (!is.list(calibrate)) {
    stop("`calibrate` must be a named list of additional QCA::calibrate() arguments.")
  }
  if (length(calibrate) == 0L) return(calibrate)

  nms <- names(calibrate)
  if (is.null(nms) || any(!nzchar(nms))) {
    stop("`calibrate` must be a named list.")
  }

  reserved <- c("x", "type", "thresholds")
  used <- intersect(nms, reserved)
  if (length(used) > 0L) {
    stop(
      "`calibrate` must not contain ",
      paste0("`", used, "`", collapse = ", "),
      "; ",
      caller,
      " supplies those arguments from the calibration specification."
    )
  }

  calibrate
}

.normalize_test_outcome <- function(test.outcome) {
  if (!is.logical(test.outcome) || length(test.outcome) != 1L || is.na(test.outcome)) {
    stop("`test.outcome` must be a single TRUE/FALSE value.")
  }
  isTRUE(test.outcome)
}

.normalize_test_conditions <- function(test.conditions, conditions, test.outcome = FALSE) {
  test.outcome <- .normalize_test_outcome(test.outcome)

  if (is.null(test.conditions)) {
    if (!test.outcome) {
      stop("`test.conditions` can be NULL only when `test.outcome = TRUE`.")
    }
    return(character(0))
  }

  if (!is.character(test.conditions) || length(test.conditions) < 1L) {
    stop("`test.conditions` must be NULL or a non-empty character vector.")
  }

  test.conditions <- unique(trimws(test.conditions))
  test.conditions <- test.conditions[nzchar(test.conditions)]
  if (length(test.conditions) < 1L) {
    stop("`test.conditions` must include at least one non-empty condition name, or be NULL when `test.outcome = TRUE`.")
  }
  if (!all(test.conditions %in% conditions)) {
    stop("All `test.conditions` must be contained in `conditions`.")
  }

  test.conditions
}

.calib_spec_targets <- function(conditions, outcome = NULL, test.outcome = FALSE) {
  test.outcome <- .normalize_test_outcome(test.outcome)

  if (!test.outcome) return(conditions)
  if (is.null(outcome) || !is.character(outcome) || length(outcome) != 1L || !nzchar(outcome)) {
    stop("`outcome` must be supplied when `test.outcome = TRUE`.")
  }

  .validate_outcome_conditions_distinct(outcome, conditions)
  c(conditions, outcome)
}

.normalize_calib_specs <- function(
    conditions,
    outcome = NULL,
    calib_spec,
    test.outcome = FALSE,
    caller = "qcaERT"
) {
  if (!is.character(conditions) || length(conditions) < 1L) {
    stop("`conditions` must be a non-empty character vector.")
  }

  test.outcome <- .normalize_test_outcome(test.outcome)
  spec_targets <- .calib_spec_targets(
    conditions = conditions,
    outcome = outcome,
    test.outcome = test.outcome
  )
  if (is.null(calib_spec)) {
    stop("`calib_spec` must be supplied.")
  }
  if (!is.list(calib_spec)) {
    stop("`calib_spec` must be a named list.")
  }
  if (is.null(names(calib_spec)) || any(!nzchar(names(calib_spec)))) {
    if (test.outcome) {
      stop("`calib_spec` must be named by `conditions` plus `outcome` when `test.outcome = TRUE`.")
    }
    stop("`calib_spec` must be named by `conditions`.")
  }
  if (!setequal(names(calib_spec), spec_targets) || length(calib_spec) != length(spec_targets)) {
    if (test.outcome) {
      stop("`calib_spec` must contain exactly one entry for each condition and the outcome when `test.outcome = TRUE`.")
    }
    stop("`calib_spec` must contain exactly one entry for each condition.")
  }

  calib_spec <- calib_spec[spec_targets]
  out <- setNames(vector("list", length(spec_targets)), spec_targets)

  for (cond in spec_targets) {
    spec <- calib_spec[[cond]]
    if (!is.list(spec)) {
      stop("`calib_spec[['", cond, "']]` must be a list.")
    }
    if (is.null(spec$raw) || !is.character(spec$raw) || length(spec$raw) != 1L || !nzchar(spec$raw)) {
      stop("`calib_spec[['", cond, "']]$raw` must be a single non-empty character string.")
    }
    if (is.null(spec$type)) {
      stop("`calib_spec[['", cond, "']]$type` must be supplied.")
    }

    type_fc <- .normalize_calib_type(spec$type, paste0("calib_spec[['", cond, "']]$type"))
    cal_extra <- if (is.null(spec$calibrate)) list() else spec$calibrate
    cal_extra <- .normalize_calibrate_options(cal_extra, caller)
    method <- spec$method
    if (is.null(method) && "method" %in% names(cal_extra)) {
      method <- cal_extra$method
      cal_extra$method <- NULL
    }
    method <- .normalize_calib_method(method, type_fc, caller)

    out[[cond]] <- list(
      raw = spec$raw,
      type = type_fc,
      qca_type = .qca_calib_type(type_fc),
      method = method,
      thresholds = .normalize_calib_thresholds(spec$thresholds, type_fc, method, cond, caller),
      calibrate = cal_extra
    )
  }

  out
}

.calib_anchor_labels <- function(spec) {
  names(spec$thresholds)
}

.normalize_calib_anchors_to_test <- function(anchors_to_test) {
  if (is.null(anchors_to_test)) return(NULL)
  if (!is.character(anchors_to_test) || length(anchors_to_test) < 1L) {
    stop("`anchors_to_test` must be NULL or a non-empty character vector.")
  }
  toupper(trimws(anchors_to_test))
}

.effective_calib_anchors <- function(spec, anchors_to_test) {
  anchors <- .calib_anchor_labels(spec)
  if (is.null(anchors_to_test)) return(anchors)
  intersect(anchors_to_test, anchors)
}

.calib_anchor_value <- function(spec, anchor) {
  anchor <- toupper(anchor)
  idx <- match(anchor, .calib_anchor_labels(spec))
  if (is.na(idx)) {
    stop("Unknown calibration anchor '", anchor, "'.")
  }
  unname(spec$thresholds[idx])
}

.replace_calib_anchor <- function(spec, anchor, value, validate = TRUE) {
  anchor <- toupper(anchor)
  idx <- match(anchor, .calib_anchor_labels(spec))
  if (is.na(idx)) {
    stop("Unknown calibration anchor '", anchor, "'.")
  }
  spec$thresholds[idx] <- value

  if (isTRUE(validate) && !.valid_calib_thresholds(spec$thresholds, spec$type, spec$method)) {
    stop("Replacing calibration anchor '", anchor, "' produced invalid thresholds.")
  }

  spec
}

.is_calib_anchor_feasible <- function(spec, anchor, value, direction, xmin, xmax) {
  direction <- match.arg(direction, c("lower", "upper"))
  if (!is.finite(value)) return(FALSE)
  if (is.finite(xmin) && value < xmin) return(FALSE)
  if (is.finite(xmax) && value > xmax) return(FALSE)

  spec_try <- .replace_calib_anchor(spec, anchor, value, validate = FALSE)
  .valid_calib_thresholds(spec_try$thresholds, spec_try$type, spec_try$method)
}

.calib_zero_delta_steps <- function(spec) {
  stats::setNames(rep(0L, length(.calib_anchor_labels(spec))), .calib_anchor_labels(spec))
}

.calibrate_from_spec <- function(raw_x, spec) {
  args <- list(
    x = raw_x,
    type = spec$qca_type,
    thresholds = unname(spec$thresholds)
  )

  if (identical(spec$type, "f")) {
    args$method <- spec$method
  }

  do.call(QCA::calibrate, c(args, spec$calibrate))
}

.prepare_calib_context <- function(
    conditions,
    outcome = NULL,
    calib_spec,
    test.outcome = FALSE,
    raw.data,
    unit_step,
    unit_step_divisor,
    anchors_to_test,
    caller
) {
  calib_specs <- .normalize_calib_specs(
    conditions = conditions,
    outcome = outcome,
    calib_spec = calib_spec,
    test.outcome = test.outcome,
    caller = caller
  )

  raw_sources <- vapply(calib_specs, function(x) x$raw, character(1))
  if (!all(raw_sources %in% colnames(raw.data))) {
    stop("All raw columns referenced by the calibration specification must exist in `raw.data`.")
  }

  spec_targets <- names(calib_specs)
  condition_specs <- calib_specs[conditions]
  outcome_spec <- if (isTRUE(test.outcome)) calib_specs[[outcome]] else NULL

  unit_step_targets <- .coerce_unit_step(
    unit_step = unit_step,
    conds = spec_targets,
    raw.data = raw.data,
    unit_step_divisor = unit_step_divisor,
    calib_specs = calib_specs
  )

  list(
    calib_specs = calib_specs,
    condition_specs = condition_specs,
    outcome_spec = outcome_spec,
    spec_targets = spec_targets,
    raw_sources = raw_sources[conditions],
    raw_targets = raw_sources,
    type_fc = vapply(condition_specs, function(x) x$type, character(1)),
    type_targets = vapply(calib_specs, function(x) x$type, character(1)),
    thresholds = lapply(condition_specs, function(x) x$thresholds),
    thresholds_targets = lapply(calib_specs, function(x) x$thresholds),
    unit_step = unit_step_targets[conditions],
    unit_step_targets = unit_step_targets,
    anchors_to_test = .normalize_calib_anchors_to_test(anchors_to_test)
  )
}

.calib_apply_specs_to_data <- function(data, raw.data, calib_specs, conditions) {
  data_out <- data

  for (cond in conditions) {
    spec <- calib_specs[[cond]]
    data_out[[cond]] <- .calibrate_from_spec(
      raw_x = raw.data[[spec$raw]],
      spec = spec
    )
  }

  data_out
}

.calib_thresholds_in_range <- function(spec, xmin, xmax) {
  vals <- as.numeric(spec$thresholds)
  all(is.finite(vals)) && all(vals >= xmin) && all(vals <= xmax)
}

.altset_build_calib_candidate <- function(
    set_name,
    role,
    calib_specs,
    raw.data,
    unit_step_vec,
    anchors_to_test,
    calib_max_steps
) {
  spec0 <- calib_specs[[set_name]]
  raw_name <- spec0$raw
  t0 <- spec0$thresholds
  step_i <- unit_step_vec[[set_name]]

  anchors_all <- .calib_anchor_labels(spec0)
  anchors_eff <- .effective_calib_anchors(spec0, anchors_to_test)

  if (length(anchors_eff) == 0L) {
    stop(
      "`anchors_to_test` did not match any eligible calibration anchors for set '",
      set_name,
      "'. Eligible anchors are: ",
      paste(anchors_all, collapse = ", "),
      "."
    )
  }

  x_raw <- raw.data[[raw_name]]
  xmin <- suppressWarnings(min(x_raw, na.rm = TRUE))
  xmax <- suppressWarnings(max(x_raw, na.rm = TRUE))

  if (!is.finite(xmin) || !is.finite(xmax)) {
    stop("Raw column '", raw_name, "' must have finite min and max.")
  }

  if (!.calib_thresholds_in_range(spec0, xmin, xmax)) {
    stop("Baseline thresholds for set '", set_name, "' must lie within the raw-data range.")
  }

  delta_ranges <- stats::setNames(vector("list", length(anchors_all)), anchors_all)
  for (anchor in anchors_all) {
    if (!anchor %in% anchors_eff) {
      delta_ranges[[anchor]] <- 0L
      next
    }

    baseline_value <- .calib_anchor_value(spec0, anchor)
    lower_by_range <- ceiling((xmin - baseline_value) / step_i)
    upper_by_range <- floor((xmax - baseline_value) / step_i)
    lower <- max(-calib_max_steps, lower_by_range)
    upper <- min(calib_max_steps, upper_by_range)

    if (!is.finite(lower) || !is.finite(upper) || lower > upper) {
      stop("No admissible threshold moves were found for anchor '", anchor, "' in set '", set_name, "'.")
    }

    delta_ranges[[anchor]] <- seq.int(as.integer(lower), as.integer(upper))
  }

  list(
    set = set_name,
    role = role,
    raw = raw_name,
    type = spec0$type,
    method = spec0$method,
    baseline = t0,
    anchors = anchors_all,
    sampled_anchors = anchors_eff,
    delta_ranges = delta_ranges,
    unit_step = step_i,
    xmin = xmin,
    xmax = xmax,
    spec = spec0,
    max_attempts = max(1000L, 200L * length(anchors_eff))
  )
}

.altset_sample_calib_candidate <- function(info) {
  for (attempt in seq_len(info$max_attempts)) {
    delta_steps <- stats::setNames(integer(length(info$anchors)), info$anchors)

    for (anchor in info$sampled_anchors) {
      choices <- info$delta_ranges[[anchor]]
      delta_steps[anchor] <- if (length(choices) == 1L) choices else sample(choices, size = 1L)
    }

    spec_new <- info$spec
    for (anchor in info$anchors) {
      delta <- delta_steps[anchor]
      if (delta == 0L) next
      next_value <- .calib_anchor_value(info$spec, anchor) + delta * info$unit_step
      spec_new <- .replace_calib_anchor(spec_new, anchor, next_value, validate = FALSE)
    }

    if (
      .calib_thresholds_in_range(spec_new, info$xmin, info$xmax) &&
      .valid_calib_thresholds(spec_new$thresholds, spec_new$type, spec_new$method)
    ) {
      return(list(
        spec = spec_new,
        delta_steps = delta_steps,
        changed = any(delta_steps != 0L)
      ))
    }
  }

  stop(
    "Could not generate an admissible random calibration candidate for set '",
    info$set,
    "' after ",
    info$max_attempts,
    " attempts. Consider reducing `calib_max_steps`, reducing `unit_step`, ",
    "or narrowing `anchors_to_test`."
  )
}

.altset_sample_calibration_draw <- function(candidate_map, calib.data, raw.data, thr_list) {
  data_step <- calib.data
  thresholds_step <- thr_list
  calib_changes <- list()
  any_changed <- FALSE

  for (set_name in names(candidate_map)) {
    info <- candidate_map[[set_name]]
    sampled <- .altset_sample_calib_candidate(info)

    thr_new <- sampled$spec$thresholds
    thr_old <- info$baseline
    delta_steps <- sampled$delta_steps
    changed <- sampled$changed

    thresholds_step[[set_name]] <- thr_new
    data_step[[set_name]] <- .calibrate_from_spec(raw.data[[info$raw]], sampled$spec)
    calib_changes[[set_name]] <- list(
      set = info$set,
      role = info$role,
      raw = info$raw,
      type = info$type,
      method = info$method,
      thresholds_baseline = thr_old,
      thresholds_draw = thr_new,
      delta_steps = delta_steps,
      changed = changed
    )
    any_changed <- any_changed || changed
  }

  list(
    data = data_step,
    thresholds = thresholds_step,
    changes = calib_changes,
    changed = any_changed
  )
}
