.boundary_error_message <- function(e) {
  if (!inherits(e, "error")) return(NA_character_)
  tryCatch(conditionMessage(e), error = function(...) as.character(e))
}

.boundary_compute_exclude <- function(tt_obj, exclude_mode, exclude_recompute, exclude_static) {
  .qcaert_compute_exclude(
    tt_obj = tt_obj,
    exclude_mode = exclude_mode,
    exclude_recompute = exclude_recompute,
    exclude_static = exclude_static
  )
}

.boundary_run_solution_type <- function(
    tt_obj,
    solution_type,
    ex,
    dots_filtered,
    dir.exp,
    which_M,
    i_mode
) {
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

  list(
    error = NULL,
    res = res,
    sig = sig_info$sig,
    selected_solution_missing = isTRUE(sig_info$selected_solution_missing),
    meta = sig_info$meta
  )
}

.boundary_empty_solution_type_list <- function() {
  list(conservative = NULL, parsimonious = NULL, intermediate = NULL)
}

.boundary_empty_selected_solution_missing <- function() {
  c(conservative = NA, parsimonious = NA, intermediate = NA)
}

.boundary_run_once <- function(
    value,
    build_truth_table,
    monitored_solutions,
    exclude_mode,
    exclude_recompute,
    exclude_static,
    dots_filtered,
    dir.exp,
    which_M,
    i_mode
) {
  tt_obj <- tryCatch(
    suppressWarnings(build_truth_table(value)),
    error = function(e) e
  )

  if (inherits(tt_obj, "error")) {
    return(list(
      status = "truth_table_error",
      tt = NULL,
      exclude_used = NULL,
      res = .boundary_empty_solution_type_list(),
      sig = .boundary_empty_solution_type_list(),
      selected_solution_missing = .boundary_empty_selected_solution_missing(),
      meta = .boundary_empty_solution_type_list(),
      error_source = "truthTable",
      error_message = .boundary_error_message(tt_obj)
    ))
  }

  need_exclude <- any(monitored_solutions %in% c("parsimonious", "intermediate")) &&
    exclude_mode != "none"

  ex <- NULL
  if (need_exclude) {
    ex <- .boundary_compute_exclude(
      tt_obj = tt_obj,
      exclude_mode = exclude_mode,
      exclude_recompute = exclude_recompute,
      exclude_static = exclude_static
    )

    if (inherits(ex, "error")) {
      return(list(
        status = "exclude_error",
        tt = tt_obj,
        exclude_used = NULL,
        res = .boundary_empty_solution_type_list(),
        sig = .boundary_empty_solution_type_list(),
        selected_solution_missing = .boundary_empty_selected_solution_missing(),
        meta = .boundary_empty_solution_type_list(),
        error_source = "exclude",
        error_message = .boundary_error_message(ex)
      ))
    }
  }

  res_out <- .boundary_empty_solution_type_list()
  sig_out <- .boundary_empty_solution_type_list()
  selected_solution_out <- .boundary_empty_selected_solution_missing()
  meta_out <- .boundary_empty_solution_type_list()

  for (solution_type in monitored_solutions) {
    rr <- .boundary_run_solution_type(
      tt_obj = tt_obj,
      solution_type = solution_type,
      ex = ex,
      dots_filtered = dots_filtered,
      dir.exp = dir.exp,
      which_M = which_M,
      i_mode = i_mode
    )

    if (!is.null(rr$error)) {
      return(list(
        status = "minimize_error",
        tt = tt_obj,
        exclude_used = ex,
        res = res_out,
        sig = sig_out,
        selected_solution_missing = selected_solution_out,
        meta = meta_out,
        error_source = solution_type,
        error_message = .boundary_error_message(rr$error)
      ))
    }

    res_out[[solution_type]] <- rr$res
    sig_out[[solution_type]] <- rr$sig
    selected_solution_out[[solution_type]] <- isTRUE(rr$selected_solution_missing)
    meta_out[[solution_type]] <- rr$meta
  }

  list(
    status = "ok",
    tt = tt_obj,
    exclude_used = ex,
    res = res_out,
    sig = sig_out,
    selected_solution_missing = selected_solution_out,
    meta = meta_out,
    error_source = NA_character_,
    error_message = NA_character_
  )
}

.boundary_solution_change_info <- function(base, cur, monitored_solutions) {
  changed_flags <- vapply(
    monitored_solutions,
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
    changed_types = changed_types,
    change_kind = change_kind
  )
}

.boundary_exclude_count <- function(x) {
  if (is.null(x)) return(0L)
  length(x)
}

.boundary_exclude_chr <- function(x) {
  if (is.null(x)) return(NA_character_)
  if (length(x) == 0L) return("")
  paste(as.character(x), collapse = ",")
}

.boundary_stop_reason_from_status <- function(status, direction = NULL, baseline = FALSE) {
  out <- switch(
    status,
    truth_table_error = "truth_table_build_error",
    exclude_error = "exclude_recompute_error",
    minimize_error = "requested_minimize_error",
    boundary = if (!is.null(direction)) paste0("search_boundary_", direction) else "search_boundary",
    run_budget_exhausted = "search_budget_exhausted",
    status
  )

  if (isTRUE(baseline)) {
    out <- paste0("baseline_", out)
  }

  out
}

.boundary_cast_value <- function(value, value_type) {
  value_type <- match.arg(value_type, c("numeric", "integer"))

  if (length(value) == 0L || is.na(value)) {
    return(switch(value_type, numeric = NA_real_, integer = NA_integer_))
  }

  switch(
    value_type,
    numeric = as.numeric(value),
    integer = as.integer(value)
  )
}

.boundary_make_search_row <- function(
    direction,
    solution,
    solution_types,
    solution_type,
    i_mode,
    which_M,
    value_name,
    value_type,
    start_value,
    last_safe_value,
    failing_value,
    steps_done,
    total_delta,
    stop_reason,
    changed_types,
    change_kind,
    error_source,
    error_message,
    baseline_exclude,
    last_safe_exclude,
    failing_exclude,
    extra_row
) {
  value_na <- .boundary_cast_value(NA, value_type)

  row_fields <- list(
    direction = direction,
    solution = solution,
    monitored_solutions = paste(solution_types, collapse = ","),
    i_mode = if ("intermediate" %in% solution_types) i_mode else NA_character_,
    which_M = which_M
  )
  row_fields[[paste0(value_name, "_start")]] <- .boundary_cast_value(start_value, value_type)
  row_fields[[paste0(value_name, "_last_safe")]] <- if (is.null(last_safe_value)) value_na else .boundary_cast_value(last_safe_value, value_type)
  row_fields[[paste0(value_name, "_first_failing")]] <- if (is.null(failing_value)) value_na else .boundary_cast_value(failing_value, value_type)
  row_fields$number_of_steps <- steps_done
  row_fields$total_delta <- if (is.null(total_delta)) value_na else .boundary_cast_value(total_delta, value_type)
  row_fields$stop_reason <- stop_reason
  row_fields$changed_types <- changed_types
  row_fields$change_kind <- change_kind
  row_fields$error_source <- error_source
  row_fields$error_message <- error_message
  row_fields$n_exclude_baseline <- .boundary_exclude_count(baseline_exclude)
  row_fields$n_exclude_last_safe <- if (is.null(last_safe_value)) NA_integer_ else .boundary_exclude_count(last_safe_exclude)
  row_fields$n_exclude_first_failing <- if (is.null(failing_value)) NA_integer_ else .boundary_exclude_count(failing_exclude)
  row_fields$exclude_baseline <- .boundary_exclude_chr(baseline_exclude)
  row_fields$exclude_last_safe <- if (is.null(last_safe_value)) NA_character_ else .boundary_exclude_chr(last_safe_exclude)
  row_fields$exclude_first_failing <- if (is.null(failing_value)) NA_character_ else .boundary_exclude_chr(failing_exclude)
  row_fields <- c(row_fields, extra_row)

  row <- do.call(data.frame, c(row_fields, list(stringsAsFactors = FALSE)))

  if (!is.null(solution_type)) {
    row$solution_type <- solution_type
  }

  row
}

.boundary_search_direction <- function(
    direction,
    value_name,
    value_type,
    start_value,
    step,
    max_steps,
    lower_limit,
    upper_limit,
    run_once,
    solution,
    solution_types,
    baseline,
    solution_type,
    i_mode,
    which_M,
    extra_row = list(),
    extra_path = list()
) {
  value_type <- match.arg(value_type, c("numeric", "integer"))
  value_na <- .boundary_cast_value(NA, value_type)
  cur_value <- .boundary_cast_value(start_value, value_type)
  steps_done <- 0L
  last_safe_value <- .boundary_cast_value(start_value, value_type)
  last_safe_exclude <- baseline$exclude_used

  failing_value <- value_na
  failing_exclude <- NULL

  stop_reason <- NA_character_
  changed_types <- NA_character_
  change_kind <- NA_character_
  error_source <- NA_character_
  error_message <- NA_character_

  trace <- .empty_change_trace(value_name, value_type = value_type)

  baseline_missing <- FALSE
  if (baseline$status == "ok") {
    baseline_missing <- any(unname(baseline$selected_solution_missing[solution_types]) %in% TRUE)
  }

  baseline_invalid <- baseline$status != "ok" || baseline_missing

  if (baseline_invalid) {
    stop_reason <- if (baseline$status != "ok") {
      .boundary_stop_reason_from_status(baseline$status, baseline = TRUE)
    } else {
      "baseline_selected_solution_missing"
    }
    error_source <- baseline$error_source
    error_message <- baseline$error_message

    row <- .boundary_make_search_row(
      direction = direction,
      solution = solution,
      solution_types = solution_types,
      solution_type = solution_type,
      i_mode = i_mode,
      which_M = which_M,
      value_name = value_name,
      value_type = value_type,
      start_value = start_value,
      last_safe_value = NULL,
      failing_value = NULL,
      steps_done = NA_integer_,
      total_delta = NULL,
      stop_reason = stop_reason,
      changed_types = NA_character_,
      change_kind = NA_character_,
      error_source = error_source,
      error_message = error_message,
      baseline_exclude = baseline$exclude_used,
      last_safe_exclude = NULL,
      failing_exclude = NULL,
      extra_row = extra_row
    )

    path <- c(
      list(
        direction = direction,
        solution = solution,
        monitored_solutions = solution_types,
        solution_type = solution_type,
        trace = trace,
        stop_reason = stop_reason,
        changed_types = NA_character_,
        change_kind = NA_character_,
        last_safe_value = value_na,
        failing_value = value_na,
        baseline = baseline,
        last_safe_exclude = NULL,
        failing_exclude = NULL
      ),
      extra_path
    )

    return(list(row = row, path = path))
  }

  for (k in seq_len(max_steps)) {
    next_value <- if (direction == "lower") cur_value - step else cur_value + step

    if (next_value < lower_limit || next_value > upper_limit) {
      stop_reason <- .boundary_stop_reason_from_status("boundary", direction = direction)
      failing_value <- .boundary_cast_value(next_value, value_type)
      break
    }

    cur <- run_once(next_value, solution_types = solution_types)

    if (cur$status != "ok") {
      stop_reason <- .boundary_stop_reason_from_status(cur$status, direction = direction)
      failing_value <- .boundary_cast_value(next_value, value_type)
      failing_exclude <- cur$exclude_used
      error_source <- cur$error_source
      error_message <- cur$error_message

      trace <- .append_change_trace(
        trace = trace,
        step = k,
        value_col = value_name,
        value = .boundary_cast_value(next_value, value_type),
        changed = NA,
        status = cur$status,
        change_kind = NA_character_
      )
      break
    }

    change_info <- .boundary_solution_change_info(
      base = baseline,
      cur = cur,
      monitored_solutions = solution_types
    )

    changed <- isTRUE(change_info$changed)

    trace <- .append_change_trace(
      trace = trace,
      step = k,
      value_col = value_name,
      value = .boundary_cast_value(next_value, value_type),
      changed = changed,
      status = cur$status,
      change_kind = if (changed) change_info$change_kind else NA_character_
    )

    if (changed) {
      stop_reason <- "solution_change"
      changed_types <- change_info$changed_types
      change_kind <- change_info$change_kind
      failing_value <- .boundary_cast_value(next_value, value_type)
      failing_exclude <- cur$exclude_used
      break
    }

    cur_value <- .boundary_cast_value(next_value, value_type)
    last_safe_value <- .boundary_cast_value(next_value, value_type)
    last_safe_exclude <- cur$exclude_used
    steps_done <- steps_done + 1L
  }

  if (is.na(stop_reason)) {
    stop_reason <- .boundary_stop_reason_from_status("run_budget_exhausted")
  }

  row <- .boundary_make_search_row(
    direction = direction,
    solution = solution,
    solution_types = solution_types,
    solution_type = solution_type,
    i_mode = i_mode,
    which_M = which_M,
    value_name = value_name,
    value_type = value_type,
    start_value = start_value,
    last_safe_value = last_safe_value,
    failing_value = failing_value,
    steps_done = as.integer(steps_done),
    total_delta = last_safe_value - start_value,
    stop_reason = stop_reason,
    changed_types = changed_types,
    change_kind = change_kind,
    error_source = error_source,
    error_message = error_message,
    baseline_exclude = baseline$exclude_used,
    last_safe_exclude = last_safe_exclude,
    failing_exclude = failing_exclude,
    extra_row = extra_row
  )

  path <- c(
    list(
      direction = direction,
      solution = solution,
      monitored_solutions = solution_types,
      solution_type = solution_type,
      trace = trace,
      stop_reason = stop_reason,
      changed_types = changed_types,
      change_kind = change_kind,
      last_safe_value = .boundary_cast_value(last_safe_value, value_type),
      failing_value = .boundary_cast_value(failing_value, value_type),
      baseline = baseline,
      last_safe_exclude = last_safe_exclude,
      failing_exclude = failing_exclude
    ),
    extra_path
  )

  list(row = row, path = path)
}

.boundary_search <- function(
    value_name,
    value_type,
    start_value,
    step,
    max_steps,
    lower_limit,
    upper_limit,
    run_once,
    solution,
    monitored_solutions,
    i_mode,
    which_M,
    progress_tick,
    extra_row = list(),
    extra_path = list()
) {
  diag_rows <- list()
  by_direction <- list()

  if (solution == "all") {
    baseline_by_solution_type <- setNames(
      lapply(monitored_solutions, function(solution_type) run_once(start_value, solution_types = solution_type)),
      monitored_solutions
    )

    baseline <- list(
      by_solution_type = baseline_by_solution_type,
      status = vapply(baseline_by_solution_type, function(x) x$status, character(1)),
      exclude_used = lapply(baseline_by_solution_type, function(x) x$exclude_used),
      sig = lapply(baseline_by_solution_type, function(x) x$sig),
      selected_solution_missing = lapply(baseline_by_solution_type, function(x) x$selected_solution_missing),
      meta = lapply(baseline_by_solution_type, function(x) x$meta)
    )

    for (direction in c("lower", "upper")) {
      paths <- list()
      for (solution_type in monitored_solutions) {
        searched <- .boundary_search_direction(
          direction = direction,
          value_name = value_name,
          value_type = value_type,
          start_value = start_value,
          step = step,
          max_steps = max_steps,
          lower_limit = lower_limit,
          upper_limit = upper_limit,
          run_once = run_once,
          solution = solution,
          solution_types = solution_type,
          baseline = baseline_by_solution_type[[solution_type]],
          solution_type = solution_type,
          i_mode = i_mode,
          which_M = which_M,
          extra_row = extra_row,
          extra_path = extra_path
        )
        diag_rows[[length(diag_rows) + 1L]] <- searched$row
        paths[[solution_type]] <- searched$path
        progress_tick()
      }

      first_path <- paths[[monitored_solutions[1L]]]
      by_direction[[direction]] <- c(
        list(
          direction = direction,
          solution = solution,
          monitored_solutions = monitored_solutions,
          by_solution_type = paths
        ),
        first_path[setdiff(names(first_path), c("direction", "solution", "monitored_solutions"))]
      )
    }
  } else {
    baseline <- run_once(start_value)

    for (direction in c("lower", "upper")) {
      searched <- .boundary_search_direction(
        direction = direction,
        value_name = value_name,
        value_type = value_type,
        start_value = start_value,
        step = step,
        max_steps = max_steps,
        lower_limit = lower_limit,
        upper_limit = upper_limit,
        run_once = run_once,
        solution = solution,
        solution_types = monitored_solutions,
        baseline = baseline,
        solution_type = NULL,
        i_mode = i_mode,
        which_M = which_M,
        extra_row = extra_row,
        extra_path = extra_path
      )
      diag_rows[[length(diag_rows) + 1L]] <- searched$row
      by_direction[[direction]] <- searched$path
      progress_tick()
    }
  }

  list(
    diagnostics = .bind_rows_result(diag_rows),
    baseline = baseline,
    by_direction = by_direction
  )
}
