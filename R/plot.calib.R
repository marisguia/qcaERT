#' @rdname qcaERT_plots
#' @name qcaERT_plots
#' @method plot calib_test
#' @export
plot.calib_test <- function(
    x,
    type = c("interval", "heatmap", "trace"),
    sets = NULL,
    roles = NULL,
    anchors = NULL,
    directions = c("lower", "upper"),
    stop_reason = NULL,
    changed_types = NULL,
    solution_type = NULL,
    solution = NULL,
    monitored_solutions = NULL,
    metric = c("raw", "pct", "steps"),
    value = c("delta", "last_safe", "failing"),
    abs_delta = FALSE,
    cell = c("anchor_direction", "anchor"),
    show_text = FALSE,
    set = NULL,
    anchor = NULL,
    direction = NULL,
    show_stop = TRUE,
    order_sets = c("input", "most_sensitive", "least_sensitive"),
    legend = TRUE,
    theme = .plot_theme(),
    ...
) {
  .plot_require_ggplot2()

  type <- match.arg(type)
  dots_plot <- list(...)
  .plot_reject_solution_args(solution, monitored_solutions, "plot.calib_test")
  .plot_reject_solution_dots(dots_plot, "plot.calib_test")
  metric <- match.arg(metric)
  value <- match.arg(value)
  cell <- match.arg(cell)
  order_sets <- match.arg(order_sets)

  if (is.null(x$diagnostics) || !is.data.frame(x$diagnostics)) {
    stop("x must contain a data frame in x$diagnostics.", call. = FALSE)
  }

  required <- c(
    "set",
    "role",
    "raw",
    "type",
    "method",
    "anchor",
    "direction",
    "solution",
    "monitored_solutions",
    "start_value",
    "last_safe_value",
    "failing_value",
    "number_of_steps",
    "total_delta_units",
    "delta_as_pct_of_raw_range",
    "stop_reason",
    "changed_types"
  )
  missing <- setdiff(required, names(x$diagnostics))
  if (length(missing)) {
    stop(
      "x$diagnostics is missing required column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  df0 <- x$diagnostics
  df0$anchor <- toupper(df0$anchor)

  stop_no_rows <- function() {
    lines <- c("No rows left to plot after filtering.")

    if (!is.null(sets)) lines <- c(lines, paste0("Requested sets: ", paste(sets, collapse = ", ")))
    if (!is.null(roles)) lines <- c(lines, paste0("Requested roles: ", paste(roles, collapse = ", ")))
    if (!is.null(anchors)) lines <- c(lines, paste0("Requested anchors: ", paste(anchors, collapse = ", ")))
    if (!is.null(directions)) lines <- c(lines, paste0("Requested directions: ", paste(directions, collapse = ", ")))
    if (!is.null(stop_reason)) lines <- c(lines, paste0("Requested stop_reason: ", paste(stop_reason, collapse = ", ")))
    if (!is.null(changed_types)) lines <- c(lines, paste0("Requested changed_types: ", paste(changed_types, collapse = ", ")))
    if (!is.null(solution_type)) lines <- c(lines, paste0("Requested solution_type: ", paste(solution_type, collapse = ", ")))

    available_df <- df0
    available_df$anchor <- toupper(available_df$anchor)
    for (column in c("set", "role", "anchor", "direction", "solution_type", "stop_reason", "changed_types")) {
      available <- sort(table(.plot_as_chr0(available_df[[column]])), decreasing = TRUE)
      available <- available[nzchar(names(available))]
      label <- paste0("Available ", column, ": ")
      if (length(available)) {
        lines <- c(lines, paste0(label, .plot_fmt_counts(available)))
      } else {
        lines <- c(lines, paste0(label, "<none>"))
      }
    }

    stop(paste(lines, collapse = "\n"), call. = FALSE)
  }

  directions <- .plot_normalize_directions(directions)
  if (!is.null(anchors)) {
    anchors <- toupper(trimws(as.character(anchors)))
    anchors <- anchors[nzchar(anchors)]
  }
  selected_solution <- .plot_select_solution_type(df0, solution_type)
  df0 <- selected_solution$data
  df0$anchor <- toupper(df0$anchor)
  df <- df0
  solution_type <- selected_solution$solution_type

  if (!is.null(sets)) df <- df[df$set %in% sets, , drop = FALSE]
  if (!is.null(roles)) df <- df[df$role %in% roles, , drop = FALSE]
  if (!is.null(anchors)) df <- df[df$anchor %in% anchors, , drop = FALSE]
  if (!is.null(directions)) df <- df[df$direction %in% directions, , drop = FALSE]
  if (!is.null(stop_reason)) df <- df[df$stop_reason %in% stop_reason, , drop = FALSE]
  if (!is.null(changed_types)) {
    keep <- .plot_match_solution_tokens(df$changed_types, changed_types, arg = "changed_types")
    df <- df[keep, , drop = FALSE]
  }

  if (!nrow(df)) stop_no_rows()

  set_levels <- unique(df$set)
  if (order_sets != "input") {
    changed <- df[df$stop_reason == "solution_change", , drop = FALSE]
    if (nrow(changed)) {
      sensitivity <- tapply(
        abs(changed$delta_as_pct_of_raw_range),
        changed$set,
        function(value) suppressWarnings(min(value, na.rm = TRUE))
      )
      sensitivity[is.na(sensitivity)] <- Inf

      all_sets <- unique(df$set)
      ordered <- rep(Inf, length(all_sets))
      names(ordered) <- all_sets
      ordered[names(sensitivity)] <- sensitivity

      order_index <- if (order_sets == "most_sensitive") {
        order(ordered, na.last = TRUE)
      } else {
        order(ordered, decreasing = TRUE, na.last = TRUE)
      }
      set_levels <- all_sets[order_index]
    }
  }
  anchor_levels <- unique(df$anchor)

  df$set <- factor(df$set, levels = set_levels)
  df$anchor <- factor(df$anchor, levels = anchor_levels)

  if (type == "trace") {
    if (is.null(set) || is.null(anchor) || is.null(direction)) {
      stop("For type = 'trace', supply `set`, `anchor`, and `direction`.", call. = FALSE)
    }

    anchor <- toupper(trimws(as.character(anchor)))
    direction <- .plot_normalize_directions(direction, arg = "direction")
    if (length(anchor) != 1L || !nzchar(anchor)) {
      stop("For type = 'trace', `anchor` must be a single anchor name.", call. = FALSE)
    }
    if (length(direction) != 1L) {
      stop("For type = 'trace', `direction` must be a single value.", call. = FALSE)
    }

    trace_row <- df[
      df$set == set & df$anchor == anchor & df$direction == direction,
      ,
      drop = FALSE
    ]
    if (!nrow(trace_row)) {
      available <- df[df$set == set, c("anchor", "direction"), drop = FALSE]
      if (nrow(available)) {
        available <- unique(available)
        available_paths <- paste0(available$anchor, "/", available$direction)
        available_msg <- paste(available_paths, collapse = ", ")
      } else {
        available_msg <- "<none for this set after filtering>"
      }

      stop(
        "No trace path found for set = '", set,
        "', anchor = '", anchor,
        "', direction = '", direction,
        "'. Available anchor/direction paths for this set: ",
        available_msg,
        call. = FALSE
      )
    }

    if (is.null(x$by_set) || is.null(x$by_set[[set]])) {
      stop("set not found in x$by_set.", call. = FALSE)
    }

    step_key <- paste(anchor, direction, sep = "_")
    path <- x$by_set[[set]]$steps[[step_key]]
    if (!is.null(path$by_solution_type)) {
      row_solution_type <- as.character(trace_row$solution_type[1L])
      if (!is.null(path$by_solution_type[[row_solution_type]])) {
        path <- path$by_solution_type[[row_solution_type]]
      }
    }
    if (is.null(path) || is.null(path$trace) || !is.data.frame(path$trace)) {
      stop("Trace not found for this set/anchor/direction.", call. = FALSE)
    }

    trace <- path$trace
    trace_required <- c("step", "value", "changed", "status")
    missing_trace <- setdiff(trace_required, names(trace))
    if (length(missing_trace)) {
      stop(
        "Trace is missing required column(s): ",
        paste(missing_trace, collapse = ", "),
        call. = FALSE
      )
    }

    trace$preserved <- ifelse(is.na(trace$changed), "Unknown", ifelse(trace$changed, "No", "Yes"))
    trace$preserved <- factor(trace$preserved, levels = c("Yes", "No", "Unknown"))

    trace_row <- trace_row[1L, , drop = FALSE]
    start_value <- trace_row$start_value
    failing_value <- trace_row$failing_value

    fail_point <- NULL
    if (is.finite(failing_value) && !is.na(failing_value)) {
      next_step <- if (nrow(trace) == 0L) 1L else max(trace$step, na.rm = TRUE) + 1L
      fail_point <- data.frame(
        step = next_step,
        value = failing_value,
        preserved = factor("No", levels = levels(trace$preserved))
      )
    }

    line_df <- trace[, c("step", "value"), drop = FALSE]
    if (!is.null(fail_point)) {
      present <- nrow(line_df) && any(line_df$step == fail_point$step & line_df$value == fail_point$value)
      if (!present) line_df <- rbind(line_df, fail_point[, c("step", "value"), drop = FALSE])
      line_df <- line_df[order(line_df$step), , drop = FALSE]
    }

    p <- ggplot2::ggplot() +
      ggplot2::geom_line(
        data = line_df,
        ggplot2::aes(x = .data[["step"]], y = .data[["value"]]),
        ...
      ) +
      ggplot2::geom_point(
        data = trace,
        ggplot2::aes(
          x = .data[["step"]],
          y = .data[["value"]],
          shape = .data[["preserved"]],
          colour = .data[["preserved"]]
        ),
        size = 4,
        alpha = 0.8,
        ...
      ) +
      ggplot2::scale_shape_manual(name = "Preserved", values = c(Yes = 19, No = 15, Unknown = 17)) +
      ggplot2::scale_colour_manual(name = "Preserved", values = .plot_preserved_colors()) +
      ggplot2::labs(
        title = paste0("Calibration trace: ", set, " | ", anchor, " | ", direction),
        x = "Step",
        y = "Threshold value"
      ) +
      theme

    if (!is.null(fail_point)) {
      p <- p + ggplot2::geom_point(
        data = fail_point,
        ggplot2::aes(
          x = .data[["step"]],
          y = .data[["value"]],
          shape = .data[["preserved"]],
          colour = .data[["preserved"]]
        ),
        inherit.aes = FALSE,
        size = 4,
        alpha = 0.8
      )
    }
    if (show_stop && is.finite(start_value) && !is.na(start_value)) {
      p <- p + ggplot2::geom_hline(yintercept = start_value, linetype = "dashed")
    }
    if (show_stop && is.finite(failing_value) && !is.na(failing_value)) {
      p <- p + ggplot2::geom_hline(yintercept = failing_value, linetype = "dotdash")
    }
    if (!legend) p <- p + ggplot2::theme(legend.position = "none")

    return(p)
  }

  if (type == "heatmap") {
    df2 <- df

    fill <- switch(
      metric,
      raw = switch(
        value,
        delta = df2$total_delta_units,
        last_safe = df2$last_safe_value,
        failing = df2$failing_value
      ),
      pct = df2$delta_as_pct_of_raw_range,
      steps = df2$number_of_steps
    )
    if (abs_delta && metric %in% c("raw", "pct")) fill <- abs(fill)

    df2$fill <- fill
    df2$cell <- if (cell == "anchor_direction") {
      paste(df2$anchor, df2$direction, sep = "_")
    } else {
      as.character(df2$anchor)
    }
    df2$cell <- factor(df2$cell, levels = unique(df2$cell))

    fill_label <- if (metric == "pct") {
      "% raw range"
    } else if (metric == "steps") {
      "Steps"
    } else {
      switch(value, delta = "Delta", last_safe = "Last safe", failing = "First failing")
    }

    p <- ggplot2::ggplot(
      df2,
      ggplot2::aes(x = .data[["cell"]], y = .data[["set"]], fill = .data[["fill"]])
    ) +
      ggplot2::geom_tile(...) +
      ggplot2::labs(x = NULL, y = NULL, fill = fill_label, title = NULL) +
      theme

    if (metric %in% c("raw", "pct") && identical(value, "delta") && !isTRUE(abs_delta)) {
      p <- p + ggplot2::scale_fill_gradient2(
        low = .plot_colors()[["orange"]],
        mid = "white",
        high = .plot_colors()[["blue"]],
        midpoint = 0,
        na.value = .plot_colors()[["light_grey"]]
      )
    } else {
      p <- p + ggplot2::scale_fill_gradient(
        low = "white",
        high = .plot_colors()[["blue"]],
        na.value = .plot_colors()[["light_grey"]]
      )
    }

    if (show_text) {
      p <- p + ggplot2::geom_text(ggplot2::aes(label = round(.data[["fill"]], 3)), ...)
    }
    if (!legend) p <- p + ggplot2::theme(legend.position = "none")

    return(p)
  }

  lower <- df[df$direction == "lower", , drop = FALSE]
  upper <- df[df$direction == "upper", , drop = FALSE]

  lower <- lower[!duplicated(lower[c("set", "anchor")]), , drop = FALSE]
  upper <- upper[!duplicated(upper[c("set", "anchor")]), , drop = FALSE]

  merged <- merge(
    lower[, c("set", "anchor", "start_value", "last_safe_value"), drop = FALSE],
    upper[, c("set", "anchor", "start_value", "last_safe_value"), drop = FALSE],
    by = c("set", "anchor"),
    all = TRUE,
    suffixes = c("_lower", "_upper")
  )

  key_order <- unique(paste(as.character(df$set), as.character(df$anchor), sep = "\r"))
  merged_key <- paste(as.character(merged$set), as.character(merged$anchor), sep = "\r")
  merged <- merged[order(match(merged_key, key_order)), , drop = FALSE]

  merged$start_value <- merged$start_value_lower
  use_upper_start <- is.na(merged$start_value)
  merged$start_value[use_upper_start] <- merged$start_value_upper[use_upper_start]

  merged$set <- factor(merged$set, levels = set_levels)
  merged$anchor <- factor(merged$anchor, levels = anchor_levels)

  points <- rbind(
    data.frame(
      set = merged$set,
      anchor = merged$anchor,
      point = "Baseline",
      value = merged$start_value,
      stringsAsFactors = FALSE
    ),
    data.frame(
      set = merged$set,
      anchor = merged$anchor,
      point = "Lower last safe",
      value = merged$last_safe_value_lower,
      stringsAsFactors = FALSE
    ),
    data.frame(
      set = merged$set,
      anchor = merged$anchor,
      point = "Upper last safe",
      value = merged$last_safe_value_upper,
      stringsAsFactors = FALSE
    )
  )
  points <- points[is.finite(points$value) & !is.na(points$value), , drop = FALSE]
  if (!nrow(points)) {
    stop("No finite values to plot after filtering.", call. = FALSE)
  }

  points$set <- factor(points$set, levels = set_levels)
  points$anchor <- factor(points$anchor, levels = anchor_levels)
  points$point <- factor(points$point, levels = c("Baseline", "Lower last safe", "Upper last safe"))

  segment <- merged[
    is.finite(merged$last_safe_value_lower) &
      !is.na(merged$last_safe_value_lower) &
    is.finite(merged$last_safe_value_upper) &
      !is.na(merged$last_safe_value_upper),
    c("set", "anchor", "last_safe_value_lower", "last_safe_value_upper"),
    drop = FALSE
  ]

  dots_point <- list(...)
  pt_size <- if (!is.null(dots_point$size)) dots_point$size else 3
  pt_alpha <- if (!is.null(dots_point$alpha)) dots_point$alpha else 0.8

  dots_point$size <- NULL
  dots_point$alpha <- NULL
  dots_point$mapping <- NULL
  dots_point$data <- NULL
  dots_point$inherit.aes <- NULL

  point_layer <- do.call(
    ggplot2::geom_point,
    c(
      list(
        mapping = ggplot2::aes(
          x = .data[["value"]],
          y = .data[["anchor"]],
          shape = .data[["point"]],
          colour = .data[["point"]],
          fill = .data[["point"]]
        ),
        size = pt_size,
        alpha = pt_alpha
      ),
      dots_point
    )
  )

  p <- ggplot2::ggplot(points)

  if (nrow(segment)) {
    p <- p + ggplot2::geom_segment(
      data = segment,
      ggplot2::aes(
        x = .data[["last_safe_value_lower"]],
        xend = .data[["last_safe_value_upper"]],
        y = .data[["anchor"]],
        yend = .data[["anchor"]]
      ),
      inherit.aes = FALSE,
      linewidth = 0.8
    )
  }

  p <- p +
    point_layer +
    ggplot2::scale_shape_manual(name = "Point", values = c("Baseline" = 16, "Lower last safe" = 25, "Upper last safe" = 24)) +
    ggplot2::scale_colour_manual(name = "Point", values = .plot_boundary_colors()) +
    ggplot2::scale_fill_manual(name = "Point", values = .plot_boundary_colors()) +
    ggplot2::labs(x = "Threshold value", y = NULL, title = NULL) +
    theme +
    ggplot2::facet_wrap(ggplot2::vars(.data[["set"]]), scales = "free_x")

  if (!legend) p <- p + ggplot2::theme(legend.position = "none")

  p
}
