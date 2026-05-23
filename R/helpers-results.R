.bind_rows_result <- function(rows) {
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

.bind_rows_result_sorted <- function(rows, order_by) {
  out <- do.call(rbind, rows)
  out <- out[order(out[[order_by]]), , drop = FALSE]
  rownames(out) <- NULL
  out
}

.new_result_object <- function(class, ...) {
  structure(
    list(...),
    class = class
  )
}

.print_diagnostics_result <- function(x, row.names = FALSE, ...) {
  print.data.frame(x$diagnostics, row.names = row.names, ...)
  invisible(x)
}

.as.data.frame_diagnostics_result <- function(x, ...) {
  x$diagnostics
}

.as.data.frame_results <- function(x, ...) {
  x$results
}

.as.data.frame_cluster_overview <- function(x, ...) {
  x$results$overview
}

.qcaert_progress_messages <- function() {
  c(
    "This might take a while... Maybe go grab a coffee?",
    "This might take a while... Perfect moment to stretch.",
    "This might take a while... Please resist the urge to stare directly at the progress bar.",
    "This might take a while... The Boolean gods are being consulted.",
    "This might take a while... Fun fact: in qcaERT, multi-value support is currently a logical remainder.",
    "This might take a while... The minimization elves are hard at work.",
    "This might take a while... The truth is out there... in a table, apparently.",
    "This might take a while... Maybe hydrate before the next robustness check.",
    "This might take a while... Santa only brings presents to good counterfactuals.",
    "This might take a while... Causal complexity refuses to be rushed.",
    "This might take a while... Please enjoy this brief intermission of set-theoretic suspense.",
    "This might take a while... The remainders are a bit illogical today.",
    "This might take a while... Another day, another battle with limited diversity.",
    "This might take a while... Your computer is discovering causal complexity the hard way.",
    "This might take a while... Limited diversity has entered the chat.",
    "This might take a while... 'Less is more', said the parsimonious solution.",
    "This might take a while... One of the remainders is having an identity crisis.",
    "This might take a while... The truth table has some truths to work through.",
    "This might take a while... 'Am I not sufficient for you?' - said the necessary condition.",
    "This might take a while... Proportional reduction in impatience is not guaranteed.",
    "This might take a while... Inclusion and coverage are in a complicated relationship.",
    "This might take a while... Some contradictory configurations need to calm down first.",
    "This might take a while... The process of logical minimization doesn't minimize anxiety.",
    "This might take a while... Equifinality, asymmetry and conjunctural causation walk into a bar...",
    "This might take a while... Insufficient but necessary conditions are unionizing to become jointly sufficient.",
    "This might take a while... 'Realists', 'idealists' and the regularity theory of causation walk into a bar..."
  )
}

.new_qcaert_progress <- function(total, progress = TRUE) {
  pb <- NULL
  done <- 0L
  total <- as.integer(total)
  if (length(total) != 1L || is.na(total)) {
    total <- 0L
  }

  if (isTRUE(progress) && interactive() && is.finite(total) && total > 0L) {
    cat(sample(.qcaert_progress_messages(), size = 1L), "\n")
    pb <- utils::txtProgressBar(min = 0, max = total, style = 3)
  }

  list(
    tick = function(step = 1L) {
      if (!is.null(pb)) {
        done <<- min(total, done + as.integer(step))
        utils::setTxtProgressBar(pb, done)
      }
      invisible(done)
    },
    set = function(value) {
      if (!is.null(pb)) {
        done <<- max(0L, min(total, as.integer(value)))
        utils::setTxtProgressBar(pb, done)
      }
      invisible(done)
    },
    close = function() {
      if (!is.null(pb)) {
        try(close(pb), silent = TRUE)
        pb <<- NULL
      }
      invisible(NULL)
    }
  )
}

.print_qcaert_solution_settings <- function(settings) {
  if (!is.null(settings$solution)) {
    cat("Solution: ", settings$solution, "\n", sep = "")
  }
  if (!is.null(settings$monitored_solutions)) {
    cat("Monitored solutions: ", paste(settings$monitored_solutions, collapse = ", "), "\n", sep = "")
  }
  if (!is.null(settings$which_M)) {
    cat("which_M: ", settings$which_M, "\n", sep = "")
  }
  if (!is.null(settings$i_mode) &&
      !is.null(settings$monitored_solutions) &&
      any(settings$monitored_solutions %in% "intermediate")) {
    cat("i_mode: ", settings$i_mode, "\n", sep = "")
  }
}

.print_qcaert_heading <- function(class, label, settings) {
  cat("<", class, ">\n", sep = "")
  cat("Outcome test: ", label, "\n", sep = "")
  .print_qcaert_solution_settings(settings)
}

.print_qcaert_table <- function(x, title, row.names = FALSE, ...) {
  cat("\n", title, "\n", sep = "")
  print(x, row.names = row.names, ...)
}

.title_case_word <- function(x) {
  paste0(toupper(substr(x, 1L, 1L)), substring(x, 2L))
}

.print_boundary_summary <- function(x) {
  cat("\nSummary\n")
  if (!is.null(x$bounds) && is.matrix(x$bounds)) {
    intervals <- data.frame(
      solution_type = vapply(colnames(x$bounds), .compact_solution_type_names, character(1)),
      lower = unname(x$bounds["Lower", ]),
      upper = unname(x$bounds["Upper", ]),
      width = unname(x$bounds["Upper", ] - x$bounds["Lower", ]),
      stringsAsFactors = FALSE
    )
    .print_qcaert_table(intervals, "Stable intervals", row.names = FALSE)
  } else {
    lower <- if (!is.null(x$bounds)) unname(x$bounds[["Lower"]]) else NA_real_
    upper <- if (!is.null(x$bounds)) unname(x$bounds[["Upper"]]) else NA_real_

    if (!is.na(lower) && !is.na(upper)) {
      cat(" Stable interval: [", format(lower, trim = TRUE), ", ", format(upper, trim = TRUE), "]\n", sep = "")
      cat(" Total width: ", format(upper - lower, trim = TRUE), "\n", sep = "")
    }
  }
}

.count_present <- function(x) {
  if (is.null(x)) {
    return(0L)
  }
  sum(!is.na(x), na.rm = TRUE)
}

.require_result_columns <- function(diagnostics, needed, label) {
  if (!is.data.frame(diagnostics)) {
    stop("Cannot build ", label, " `results`; `diagnostics` must be a data frame.")
  }

  missing_cols <- setdiff(needed, names(diagnostics))
  if (length(missing_cols) > 0L) {
    stop(
      "Cannot build ", label, " `results`; missing columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  invisible(TRUE)
}

.select_result_columns <- function(diagnostics, from, to = from, label) {
  .require_result_columns(diagnostics, from, label)

  out <- diagnostics[, from, drop = FALSE]
  names(out) <- to
  out
}

.finish_result_table <- function(out, columns = NULL) {
  if (!is.null(columns)) {
    out <- out[, columns, drop = FALSE]
  }

  rownames(out) <- NULL
  out
}

.result_reason_vector <- function(stop_reason, change_kind) {
  vapply(
    seq_along(stop_reason),
    function(i) .compact_result_reason(stop_reason[i], change_kind[i]),
    character(1)
  )
}

.result_solution_change_vector <- function(change_kind) {
  vapply(
    seq_along(change_kind),
    function(i) {
      if (is.na(change_kind[i])) {
        return(NA_character_)
      }
      .compact_change_kind(change_kind[i])
    },
    character(1)
  )
}

.result_fit_changed_types_vector <- function(fit_changed_types) {
  vapply(
    seq_along(fit_changed_types),
    function(i) {
      if (is.na(fit_changed_types[i])) {
        return(NA_character_)
      }
      .compact_solution_type_names(fit_changed_types[i])
    },
    character(1)
  )
}

.add_solution_and_fit_columns <- function(out) {
  out$solution_change <- .result_solution_change_vector(out$change_kind)
  out$fit_changed_types <- .result_fit_changed_types_vector(out$fit_changed_types)
  out
}

.result_solution_type_order <- function(solution_types) {
  solution_types <- as.character(solution_types)
  solution_types <- solution_types[!is.na(solution_types)]
  solution_types <- unique(solution_types[nzchar(solution_types)])
  canonical_order <- c("conservative", "parsimonious", "intermediate")
  c(canonical_order[canonical_order %in% solution_types], setdiff(solution_types, canonical_order))
}

.result_solution_type_prefix <- function(solution_type) {
  switch(
    solution_type,
    conservative = "con",
    parsimonious = "par",
    intermediate = "int",
    gsub("[^A-Za-z0-9]+", "_", tolower(solution_type))
  )
}

.boundary_bounds <- function(diagnostics, value_name) {
  value_col <- paste0(value_name, "_last_safe")

  if ("solution_type" %in% names(diagnostics)) {
    solution_types <- .result_solution_type_order(diagnostics$solution_type)
    out <- matrix(
      NA_real_,
      nrow = 2L,
      ncol = length(solution_types),
      dimnames = list(c("Lower", "Upper"), solution_types)
    )

    for (solution_type in solution_types) {
      for (direction in c("lower", "upper")) {
        dd <- diagnostics[
          diagnostics$solution_type == solution_type & diagnostics$direction == direction,
          ,
          drop = FALSE
        ]
        if (nrow(dd) > 0L) {
          out[.title_case_word(direction), solution_type] <- dd[[value_col]][1L]
        }
      }
    }

    return(out)
  }

  c(
    Lower = diagnostics[[value_col]][diagnostics$direction == "lower"][1],
    Upper = diagnostics[[value_col]][diagnostics$direction == "upper"][1]
  )
}

.compact_change_kind <- function(x) {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) {
    return(NA_character_)
  }

  x <- as.character(x)
  x <- x[!is.na(x)]
  x <- trimws(x)
  x <- x[nzchar(x)]

  if (length(x) == 0L) {
    return(NA_character_)
  }

  x <- gsub("\\s+", "", x)
  x <- gsub("conservative:", "CON:", x, fixed = TRUE)
  x <- gsub("parsimonious:", "PAR:", x, fixed = TRUE)
  x <- gsub("intermediate:", "INT:", x, fixed = TRUE)
  x <- gsub(",", " | ", x, fixed = TRUE)

  paste(x, collapse = " | ")
}

.compact_result_reason <- function(stop_reason, change_kind) {
  ck <- .compact_change_kind(change_kind)

  if (!is.na(ck) && nzchar(ck)) {
    return(ck)
  }

  if (is.null(stop_reason) || length(stop_reason) == 0L || all(is.na(stop_reason))) {
    return(NA_character_)
  }

  sr <- as.character(stop_reason[[1L]])
  sr <- trimws(sr)

  if (!nzchar(sr)) {
    return(NA_character_)
  }

  sr
}

.make_boundary_results <- function(diagnostics, value_name, result_shape = c("wide", "long")) {
  result_shape <- match.arg(result_shape)

  if ("solution_type" %in% names(diagnostics)) {
    needed <- c(
      "direction",
      "solution_type",
      paste0(value_name, "_start"),
      paste0(value_name, "_last_safe"),
      paste0(value_name, "_first_failing"),
      "number_of_steps",
      "total_delta",
      "stop_reason",
      "change_kind"
    )
    .require_result_columns(diagnostics, needed, "boundary-test")

    if (identical(result_shape, "long")) {
      out <- .select_result_columns(
        diagnostics,
        from = needed,
        to = c(
          "direction",
          "solution_type",
          "start",
          "last_safe",
          "first_failing",
          "steps",
          "total_delta",
          "stop_reason",
          "change_kind"
        ),
        label = "boundary-test"
      )
      out$reason <- .result_reason_vector(out$stop_reason, out$change_kind)

      out$solution_type <- factor(as.character(out$solution_type), levels = .result_solution_type_order(out$solution_type))
      out$direction <- factor(as.character(out$direction), levels = c("lower", "upper"))
      out <- out[order(out$solution_type, out$direction), , drop = FALSE]
      out$solution_type <- as.character(out$solution_type)
      out$direction <- as.character(out$direction)

      return(.finish_result_table(out, c(
        "solution_type",
        "direction",
        "start",
        "last_safe",
        "first_failing",
        "steps",
        "total_delta",
        "reason"
      )))
    }

    directions <- unique(as.character(diagnostics$direction))
    solution_types <- .result_solution_type_order(diagnostics$solution_type)
    rows <- lapply(
      directions,
      function(direction) {
        dd_direction <- diagnostics[diagnostics$direction == direction, , drop = FALSE]
        row <- data.frame(
          direction = direction,
          start = dd_direction[[paste0(value_name, "_start")]][1L],
          stringsAsFactors = FALSE
        )

        for (solution_type in solution_types) {
          dd <- dd_direction[dd_direction$solution_type == solution_type, , drop = FALSE]
          prefix <- .result_solution_type_prefix(solution_type)
          if (nrow(dd) == 0L) {
            row[[paste0(prefix, "_last_safe")]] <- NA_real_
            row[[paste0(prefix, "_first_failing")]] <- NA_real_
            row[[paste0(prefix, "_steps")]] <- NA_integer_
            row[[paste0(prefix, "_total_delta")]] <- NA_real_
            row[[paste0(prefix, "_reason")]] <- NA_character_
          } else {
            reason <- .compact_result_reason(dd$stop_reason[1L], dd$change_kind[1L])
            row[[paste0(prefix, "_last_safe")]] <- dd[[paste0(value_name, "_last_safe")]][1L]
            row[[paste0(prefix, "_first_failing")]] <- dd[[paste0(value_name, "_first_failing")]][1L]
            row[[paste0(prefix, "_steps")]] <- dd$number_of_steps[1L]
            row[[paste0(prefix, "_total_delta")]] <- dd$total_delta[1L]
            row[[paste0(prefix, "_reason")]] <- reason
          }
        }

        row
      }
    )

    return(.finish_result_table(.bind_rows_result(rows)))
  }

  needed <- c(
    "direction",
    paste0(value_name, "_start"),
    paste0(value_name, "_last_safe"),
    paste0(value_name, "_first_failing"),
    "number_of_steps",
    "total_delta",
    "stop_reason",
    "change_kind"
  )

  out <- .select_result_columns(
    diagnostics,
    from = needed,
    to = c(
      "direction",
      "start",
      "last_safe",
      "first_failing",
      "steps",
      "total_delta",
      "stop_reason",
      "change_kind"
    ),
    label = "boundary-test"
  )

  out$reason <- .result_reason_vector(out$stop_reason, out$change_kind)

  .finish_result_table(out, c(
    "direction",
    "start",
    "last_safe",
    "first_failing",
    "steps",
    "total_delta",
    "reason"
  ))
}

.make_calib_results <- function(diagnostics, result_shape = c("wide", "long")) {
  result_shape <- match.arg(result_shape)

  if ("solution_type" %in% names(diagnostics)) {
    needed <- c(
      "set",
      "role",
      "raw",
      "type",
      "method",
      "anchor",
      "direction",
      "solution_type",
      "start_value",
      "last_safe_value",
      "failing_value",
      "step_unit_used",
      "number_of_steps",
      "total_delta_units",
      "delta_as_pct_of_raw_range",
      "stop_reason",
      "change_kind"
    )
    .require_result_columns(diagnostics, needed, "calibration-test")

    if (identical(result_shape, "long")) {
      out <- .select_result_columns(
        diagnostics,
        from = needed,
        to = c(
          "set",
          "role",
          "raw",
          "type",
          "method",
          "anchor",
          "direction",
          "solution_type",
          "start",
          "last_safe",
          "first_failing",
          "step_unit",
          "steps",
          "total_delta",
          "pct_raw_range",
          "stop_reason",
          "change_kind"
        ),
        label = "calibration-test"
      )
      out$reason <- .result_reason_vector(out$stop_reason, out$change_kind)
      out$.row_order <- seq_len(nrow(out))

      out$solution_type <- factor(as.character(out$solution_type), levels = .result_solution_type_order(out$solution_type))
      out$direction <- factor(as.character(out$direction), levels = c("lower", "upper"))
      out <- out[order(out$solution_type, out$.row_order), , drop = FALSE]
      out$solution_type <- as.character(out$solution_type)
      out$direction <- as.character(out$direction)

      return(.finish_result_table(out, c(
        "solution_type",
        "set",
        "role",
        "raw",
        "type",
        "method",
        "anchor",
        "direction",
        "start",
        "last_safe",
        "first_failing",
        "step_unit",
        "steps",
        "total_delta",
        "pct_raw_range",
        "reason"
      )))
    }

    key_cols <- c("set", "role", "raw", "type", "method", "anchor", "direction")
    keys <- unique(diagnostics[, key_cols, drop = FALSE])
    solution_types <- .result_solution_type_order(diagnostics$solution_type)

    rows <- lapply(
      seq_len(nrow(keys)),
      function(i) {
        key <- keys[i, , drop = FALSE]
        keep <- rep(TRUE, nrow(diagnostics))
        for (col in key_cols) {
          keep <- keep & diagnostics[[col]] == key[[col]]
        }

        dd_key <- diagnostics[keep, , drop = FALSE]
        row <- key
        row$start <- dd_key$start_value[1L]
        row$step_unit <- dd_key$step_unit_used[1L]

        for (solution_type in solution_types) {
          dd <- dd_key[dd_key$solution_type == solution_type, , drop = FALSE]
          prefix <- .result_solution_type_prefix(solution_type)
          if (nrow(dd) == 0L) {
            row[[paste0(prefix, "_last_safe")]] <- NA_real_
            row[[paste0(prefix, "_first_failing")]] <- NA_real_
            row[[paste0(prefix, "_steps")]] <- NA_integer_
            row[[paste0(prefix, "_total_delta")]] <- NA_real_
            row[[paste0(prefix, "_pct_raw_range")]] <- NA_real_
            row[[paste0(prefix, "_reason")]] <- NA_character_
          } else {
            reason <- .compact_result_reason(dd$stop_reason[1L], dd$change_kind[1L])
            row[[paste0(prefix, "_last_safe")]] <- dd$last_safe_value[1L]
            row[[paste0(prefix, "_first_failing")]] <- dd$failing_value[1L]
            row[[paste0(prefix, "_steps")]] <- dd$number_of_steps[1L]
            row[[paste0(prefix, "_total_delta")]] <- dd$total_delta_units[1L]
            row[[paste0(prefix, "_pct_raw_range")]] <- dd$delta_as_pct_of_raw_range[1L]
            row[[paste0(prefix, "_reason")]] <- reason
          }
        }

        row
      }
    )

    return(.finish_result_table(.bind_rows_result(rows)))
  }

  needed <- c(
    "set",
    "role",
    "raw",
    "type",
    "method",
    "anchor",
    "direction",
    "start_value",
    "last_safe_value",
    "failing_value",
    "step_unit_used",
    "number_of_steps",
    "total_delta_units",
    "delta_as_pct_of_raw_range",
    "stop_reason",
    "change_kind"
  )

  out <- .select_result_columns(
    diagnostics,
    from = needed,
    to = c(
      "set",
      "role",
      "raw",
      "type",
      "method",
      "anchor",
      "direction",
      "start",
      "last_safe",
      "first_failing",
      "step_unit",
      "steps",
      "total_delta",
      "pct_raw_range",
      "stop_reason",
      "change_kind"
    ),
    label = "calibration-test"
  )

  out$reason <- .result_reason_vector(out$stop_reason, out$change_kind)

  .finish_result_table(out, c(
    "set",
    "role",
    "raw",
    "type",
    "method",
    "anchor",
    "direction",
    "start",
    "last_safe",
    "first_failing",
    "step_unit",
    "steps",
    "total_delta",
    "pct_raw_range",
    "reason"
  ))
}

.compact_solution_type_names <- function(x) {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) {
    return(NA_character_)
  }

  x <- as.character(x)
  x <- x[!is.na(x)]
  x <- trimws(x)
  x <- x[nzchar(x)]

  if (length(x) == 0L) {
    return(NA_character_)
  }

  x <- gsub("\\s+", "", x)
  x <- gsub("conservative", "CON", x, fixed = TRUE)
  x <- gsub("parsimonious", "PAR", x, fixed = TRUE)
  x <- gsub("intermediate", "INT", x, fixed = TRUE)
  x <- gsub(",", " | ", x, fixed = TRUE)

  paste(x, collapse = " | ")
}

.make_loo_results <- function(diagnostics) {
  needed <- c(
    "row_index",
    "case_label",
    "status",
    "change_kind",
    "fit_changed_types",
    "n_fit_deltas",
    "max_abs_fit_delta"
  )

  out <- .select_result_columns(diagnostics, needed, label = "leave-one-out")
  out <- .add_solution_and_fit_columns(out)

  .finish_result_table(out, c(
    "row_index",
    "case_label",
    "status",
    "solution_change",
    "fit_changed_types",
    "n_fit_deltas",
    "max_abs_fit_delta"
  ))
}

.make_subsample_results <- function(diagnostics) {
  needed <- c(
    "rep",
    "n_sample",
    "n_holdout",
    "status",
    "change_kind",
    "fit_changed_types",
    "n_fit_deltas",
    "max_abs_fit_delta",
    "exact_match_baseline",
    "term_jaccard_baseline"
  )

  out <- .select_result_columns(diagnostics, needed, label = "subsample-test")
  out <- .add_solution_and_fit_columns(out)

  .finish_result_table(out, c(
    "rep",
    "n_sample",
    "n_holdout",
    "status",
    "exact_match_baseline",
    "term_jaccard_baseline",
    "solution_change",
    "fit_changed_types",
    "n_fit_deltas",
    "max_abs_fit_delta"
  ))
}

.make_altset_results <- function(diagnostics) {
  needed <- c(
    "draw",
    "incl.cut",
    "n.cut",
    "status",
    "change_kind",
    "fit_changed_types",
    "n_fit_deltas",
    "max_abs_fit_delta",
    "changed_sets",
    "changed_roles",
    "n_changed_sets"
  )

  out <- .select_result_columns(diagnostics, needed, label = "altset-test")
  out <- .add_solution_and_fit_columns(out)

  out$changed_sets <- as.character(out$changed_sets)
  out$changed_sets[out$changed_sets %in% c("NA", "")] <- NA_character_
  out$changed_sets <- ifelse(
    is.na(out$changed_sets),
    NA_character_,
    gsub(",", ", ", out$changed_sets, fixed = TRUE)
  )

  out$changed_roles <- as.character(out$changed_roles)
  out$changed_roles[out$changed_roles %in% c("NA", "")] <- NA_character_
  out$changed_roles <- ifelse(
    is.na(out$changed_roles),
    NA_character_,
    gsub(",", ", ", out$changed_roles, fixed = TRUE)
  )

  .finish_result_table(out, c(
    "draw",
    "incl.cut",
    "n.cut",
    "status",
    "n_changed_sets",
    "changed_sets",
    "changed_roles",
    "solution_change",
    "fit_changed_types",
    "n_fit_deltas",
    "max_abs_fit_delta"
  ))
}

.make_cluster_results <- function(diagnostics, by_cluster, by_unit) {
  needed <- c(
    "solution_type",
    "configuration_key",
    "component_count",
    "status",
    "pooled_consistency",
    "pooled_coverage",
    "max_abs_delta_consistency",
    "max_abs_delta_coverage",
    "worst_cluster_consistency_id",
    "worst_cluster_coverage_id",
    "within_available",
    "n_units_repeated"
  )

  .require_result_columns(diagnostics, needed, "cluster-test")
  overview <- diagnostics[, needed, drop = FALSE]
  overview$solution_type <- vapply(overview$solution_type, .compact_solution_type_names, character(1))

  names(overview) <- c(
    "solution_type",
    "configuration",
    "components",
    "status",
    "pooled_consistency",
    "pooled_coverage",
    "max_abs_cluster_delta_consistency",
    "max_abs_cluster_delta_coverage",
    "worst_cluster_consistency_id",
    "worst_cluster_coverage_id",
    "within_available",
    "n_units_repeated"
  )

  overview <- .finish_result_table(overview)

  clusters <- NULL
  if (!is.null(by_cluster) && length(by_cluster) > 0L) {
    cluster_rows <- lapply(by_cluster, function(x) {
      d <- x$clusters
      if (is.null(d) || !is.data.frame(d) || nrow(d) == 0L) {
        return(NULL)
      }

      data.frame(
        solution_type = .compact_solution_type_names(x$solution_type),
        configuration = x$configuration_key,
        solution_expression = x$solution_expression,
        component = as.character(d$component),
        cluster_id = as.character(d$cluster_id),
        cluster_size = as.integer(d$cluster_size),
        consistency = as.numeric(d$consistency),
        coverage = as.numeric(d$coverage),
        delta_consistency = as.numeric(d$delta_consistency),
        delta_coverage = as.numeric(d$delta_coverage),
        stringsAsFactors = FALSE
      )
    })

    cluster_rows <- Filter(Negate(is.null), cluster_rows)
    if (length(cluster_rows) > 0L) {
      clusters <- do.call(rbind, cluster_rows)
      rownames(clusters) <- NULL
    }
  }

  units <- NULL
  if (!is.null(by_unit) && length(by_unit) > 0L) {
    unit_rows <- lapply(by_unit, function(x) {
      d <- x$units
      if (is.null(d) || !is.data.frame(d) || nrow(d) == 0L) {
        return(NULL)
      }

      data.frame(
        solution_type = .compact_solution_type_names(x$solution_type),
        configuration = x$configuration_key,
        solution_expression = x$solution_expression,
        component = as.character(d$component),
        unit_id = as.character(d$unit_id),
        n_clusters = as.integer(d$n_clusters),
        consistency = as.numeric(d$consistency),
        coverage = as.numeric(d$coverage),
        delta_consistency = as.numeric(d$delta_consistency),
        delta_coverage = as.numeric(d$delta_coverage),
        stringsAsFactors = FALSE
      )
    })

    unit_rows <- Filter(Negate(is.null), unit_rows)
    if (length(unit_rows) > 0L) {
      units <- do.call(rbind, unit_rows)
      rownames(units) <- NULL
    }
  }

  list(
    overview = overview,
    clusters = clusters,
    units = units
  )
}
