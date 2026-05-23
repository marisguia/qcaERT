#' @rdname qcaERT_plots
#' @name qcaERT_plots
#' @method plot incl_test
#' @export
plot.incl_test <- function(
    x,
    type = c("interval", "trace"),
    directions = c("lower", "upper"),
    stop_reason = NULL,
    changed_types = NULL,
    solution_type = NULL,
    solution = NULL,
    monitored_solutions = NULL,
    i_mode = NULL,
    direction = NULL,
    show_stop = TRUE,
    legend = TRUE,
    theme = .plot_theme(),
    ...
) {
  .plot_require_ggplot2()

  type <- match.arg(type)
  dots_plot <- list(...)
  .plot_reject_solution_args(solution, monitored_solutions, "plot.incl_test")
  .plot_reject_solution_dots(dots_plot, "plot.incl_test")

  if (is.null(x$diagnostics) || !is.data.frame(x$diagnostics)) {
    stop("x must contain a data frame in x$diagnostics.", call. = FALSE)
  }

  required <- c(
    "direction",
    "solution",
    "monitored_solutions",
    "i_mode",
    "incl.cut_start",
    "incl.cut_last_safe",
    "incl.cut_first_failing",
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

  normalize_i_mode <- function(value) {
    if (is.null(value)) return(NULL)

    value <- trimws(as.character(value))
    value <- value[nzchar(value)]
    if (!length(value)) return(character(0))

    out <- character(length(value))
    for (i in seq_along(value)) {
      mode <- tolower(value[i])
      out[i] <- if (mode == "all") {
        "all"
      } else if (mode == "c1p1") {
        "C1P1"
      } else {
        stop("`i_mode` must contain only 'all' and/or 'C1P1'.", call. = FALSE)
      }
    }

    unique(out)
  }

  stop_no_rows <- function() {
    lines <- c("No rows left to plot after filtering.")

    if (!is.null(directions)) lines <- c(lines, paste0("Requested directions: ", paste(directions, collapse = ", ")))
    if (!is.null(stop_reason)) lines <- c(lines, paste0("Requested stop_reason: ", paste(stop_reason, collapse = ", ")))
    if (!is.null(changed_types)) lines <- c(lines, paste0("Requested changed_types: ", paste(changed_types, collapse = ", ")))
    if (!is.null(solution_type)) lines <- c(lines, paste0("Requested solution_type: ", paste(solution_type, collapse = ", ")))
    if (!is.null(i_mode)) lines <- c(lines, paste0("Requested i_mode: ", paste(i_mode, collapse = ", ")))

    for (column in c("direction", "solution_type", "stop_reason", "changed_types", "i_mode")) {
      available <- sort(table(.plot_as_chr0(df0[[column]])), decreasing = TRUE)
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
  selected_solution <- .plot_select_solution_type(df0, solution_type)
  df0 <- selected_solution$data
  df <- df0
  solution_type <- selected_solution$solution_type
  i_mode <- normalize_i_mode(i_mode)

  if (!is.null(directions)) df <- df[df$direction %in% directions, , drop = FALSE]
  if (!is.null(stop_reason)) df <- df[df$stop_reason %in% stop_reason, , drop = FALSE]
  if (!is.null(changed_types)) {
    keep <- .plot_match_solution_tokens(df$changed_types, changed_types, arg = "changed_types")
    df <- df[keep, , drop = FALSE]
  }
  if (!is.null(i_mode)) df <- df[df$i_mode %in% i_mode, , drop = FALSE]

  if (!nrow(df)) stop_no_rows()

  if (type == "trace") {
    if (is.null(direction)) {
      if (length(directions) == 1L) {
        direction <- directions
      } else {
        stop("For type = 'trace', supply direction = 'lower' or direction = 'upper'.", call. = FALSE)
      }
    }

    direction <- .plot_normalize_directions(direction, arg = "direction")
    if (length(direction) != 1L) {
      stop("For type = 'trace', `direction` must be a single value.", call. = FALSE)
    }
    if (!direction %in% df$direction) stop_no_rows()

    if (is.null(x$by_direction) || is.null(x$by_direction[[direction]])) {
      stop("direction not found in x$by_direction.", call. = FALSE)
    }

    path <- x$by_direction[[direction]]
    row <- df[df$direction == direction, , drop = FALSE][1L, , drop = FALSE]
    if (!is.null(path$by_solution_type)) {
      row_solution_type <- as.character(row$solution_type[1L])
      if (!is.null(path$by_solution_type[[row_solution_type]])) {
        path <- path$by_solution_type[[row_solution_type]]
      }
    }

    trace <- path$trace
    if (is.null(trace) || !is.data.frame(trace)) {
      stop("Trace not found for this direction.", call. = FALSE)
    }

    trace_required <- c("step", "incl.cut", "changed", "status")
    missing_trace <- setdiff(trace_required, names(trace))
    if (length(missing_trace)) {
      stop(
        "Trace is missing required column(s): ",
        paste(missing_trace, collapse = ", "),
        call. = FALSE
      )
    }

    trace$safe <- ifelse(is.na(trace$changed), "Unknown", ifelse(trace$changed, "No", "Yes"))
    trace$safe <- factor(trace$safe, levels = c("Yes", "No", "Unknown"))

    start_value <- row$incl.cut_start
    failing_value <- row$incl.cut_first_failing

    fail_point <- NULL
    if (is.finite(failing_value) && !is.na(failing_value)) {
      next_step <- if (nrow(trace) == 0L) 1L else max(trace$step, na.rm = TRUE) + 1L
      fail_point <- data.frame(
        step = next_step,
        incl.cut = failing_value,
        safe = factor("No", levels = levels(trace$safe))
      )
    }

    line_df <- trace[, c("step", "incl.cut"), drop = FALSE]
    if (!is.null(fail_point)) {
      present <- nrow(line_df) && any(line_df$step == fail_point$step & line_df$incl.cut == fail_point$incl.cut)
      if (!present) line_df <- rbind(line_df, fail_point[, c("step", "incl.cut"), drop = FALSE])
      line_df <- line_df[order(line_df$step), , drop = FALSE]
    }

    p <- ggplot2::ggplot() +
      ggplot2::geom_line(
        data = line_df,
        ggplot2::aes(x = .data[["step"]], y = .data[["incl.cut"]]),
        ...
      ) +
      ggplot2::geom_point(
        data = trace,
        ggplot2::aes(
          x = .data[["step"]],
          y = .data[["incl.cut"]],
          shape = .data[["safe"]],
          colour = .data[["safe"]]
        ),
        size = 4,
        alpha = 0.8,
        ...
      ) +
      ggplot2::scale_shape_manual(name = "Preserved", values = c(Yes = 19, No = 15, Unknown = 17)) +
      ggplot2::scale_colour_manual(name = "Preserved", values = .plot_preserved_colors()) +
      ggplot2::labs(
        title = paste0("Inclusion-cutoff trace: ", direction),
        x = "Step",
        y = "Inclusion cutoff"
      ) +
      theme

    if (!is.null(fail_point)) {
      p <- p + ggplot2::geom_point(
        data = fail_point,
        ggplot2::aes(
          x = .data[["step"]],
          y = .data[["incl.cut"]],
          shape = .data[["safe"]],
          colour = .data[["safe"]]
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

  lower <- df[df$direction == "lower", , drop = FALSE]
  upper <- df[df$direction == "upper", , drop = FALSE]

  lower_safe <- if (nrow(lower)) lower$incl.cut_last_safe[1L] else NA_real_
  upper_safe <- if (nrow(upper)) upper$incl.cut_last_safe[1L] else NA_real_
  start_value <- if (nrow(lower)) lower$incl.cut_start[1L] else NA_real_
  if (!is.finite(start_value) || is.na(start_value)) {
    if (nrow(upper)) start_value <- upper$incl.cut_start[1L]
  }

  points <- data.frame(
    point = c("Baseline", "Lower last safe", "Upper last safe"),
    value = c(start_value, lower_safe, upper_safe),
    band = "Robustness interval",
    stringsAsFactors = FALSE
  )
  points <- points[is.finite(points$value) & !is.na(points$value), , drop = FALSE]
  if (!nrow(points)) {
    stop("No finite values to plot after filtering.", call. = FALSE)
  }

  points$point <- factor(points$point, levels = c("Baseline", "Lower last safe", "Upper last safe"))
  points$band <- factor(points$band, levels = "Robustness interval")

  segment <- NULL
  if (is.finite(lower_safe) && !is.na(lower_safe) && is.finite(upper_safe) && !is.na(upper_safe)) {
    segment <- data.frame(
      x = lower_safe,
      xend = upper_safe,
      y = factor("Robustness interval", levels = "Robustness interval"),
      yend = factor("Robustness interval", levels = "Robustness interval")
    )
  }

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
          y = .data[["band"]],
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

  if (!is.null(segment)) {
    p <- p + ggplot2::geom_segment(
      data = segment,
      ggplot2::aes(
        x = .data[["x"]],
        xend = .data[["xend"]],
        y = .data[["y"]],
        yend = .data[["yend"]]
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
    ggplot2::coord_cartesian(xlim = c(0, 1)) +
    ggplot2::labs(x = "Inclusion cutoff", y = NULL, title = NULL) +
    theme

  if (show_stop && is.finite(start_value) && !is.na(start_value)) {
    p <- p + ggplot2::geom_vline(xintercept = start_value, linetype = "dashed")
  }
  if (!legend) p <- p + ggplot2::theme(legend.position = "none")

  p
}
