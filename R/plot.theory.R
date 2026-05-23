#' @rdname qcaERT_plots
#' @name qcaERT_plots
#' @method plot theory_test
#' @export
plot.theory_test <- function(
    x,
    solution_type = NULL,
    intermediate_branch = NULL,
    show_labels = TRUE,
    label_line = TRUE,
    label_line_alpha = 0.45,
    label_line_width = 0.35,
    point_size = 3.5,
    text_size = 3.5,
    label_nudge_x = 0.008,
    label_nudge_y = 0.006,
    legend = TRUE,
    theme = .plot_theme(),
    ...
) {
  .plot_require_ggplot2()
  dots_point <- list(...)
  .plot_reject_solution_dots(dots_point, "plot.theory_test")

  if (is.null(x$results) || is.null(x$results$models) || !is.data.frame(x$results$models)) {
    stop("x must contain a data frame in x$results$models.", call. = FALSE)
  }
  if (is.null(x$results$solutions) || !is.data.frame(x$results$solutions)) {
    stop("x must contain a data frame in x$results$solutions.", call. = FALSE)
  }

  models <- x$results$models
  required_models <- c("theory", "solution_type", "intermediate_branch", "status", "inclS", "covS")
  missing_models <- setdiff(required_models, names(models))
  if (length(missing_models)) {
    stop(
      "x$results$models is missing required column(s): ",
      paste(missing_models, collapse = ", "),
      call. = FALSE
    )
  }

  selected_solution_type <- .theory_plot_select_solution_type(models, solution_type)
  selected_branch <- .theory_plot_select_branch(models, selected_solution_type, intermediate_branch)

  df <- .theory_plot_models(models, selected_solution_type, selected_branch)
  if (!nrow(df)) {
    stop("No successful model rows with finite consistency and coverage are available for the selected solution type.", call. = FALSE)
  }

  solutions <- .theory_plot_solution_labels(
    solutions = x$results$solutions,
    solution_type = selected_solution_type,
    intermediate_branch = selected_branch,
    theories = as.character(df$theory)
  )

  df$legend_label <- unname(solutions[as.character(df$theory)])
  df$legend_label[is.na(df$legend_label) | !nzchar(df$legend_label)] <- paste0(as.character(df$theory), ": <no selected solution>")
  df <- .theory_plot_label_positions(df, label_nudge_x, label_nudge_y)

  theory_names <- as.character(df$theory)
  colors <- .plot_theory_colors(theory_names)
  legend_labels <- stats::setNames(
    .theory_plot_wrap_label(df$legend_label),
    theory_names
  )

  dots_point$mapping <- NULL
  dots_point$data <- NULL
  dots_point$inherit.aes <- NULL
  if (is.null(dots_point$size)) {
    dots_point$size <- point_size
  }
  if (is.null(dots_point$alpha)) {
    dots_point$alpha <- 0.9
  }

  point_layer <- do.call(
    ggplot2::geom_point,
    c(
      list(
        mapping = ggplot2::aes(
          x = .data[["inclS"]],
          y = .data[["covS"]],
          colour = .data[["theory"]]
        )
      ),
      dots_point
    )
  )

  p <- ggplot2::ggplot(df) +
    point_layer

  if (isTRUE(show_labels)) {
    if (isTRUE(label_line)) {
      p <- p + ggplot2::geom_segment(
        ggplot2::aes(
          x = .data[["inclS"]],
          y = .data[["covS"]],
          xend = .data[["label_x"]],
          yend = .data[["label_y"]],
          colour = .data[["theory"]]
        ),
        linewidth = label_line_width,
        alpha = label_line_alpha,
        lineend = "round",
        show.legend = FALSE
      )
    }

    p <- p + ggplot2::geom_text(
      ggplot2::aes(
        x = .data[["label_x"]],
        y = .data[["label_y"]],
        label = .data[["theory"]],
        colour = .data[["theory"]],
        hjust = .data[["label_hjust"]],
        vjust = .data[["label_vjust"]]
      ),
      size = text_size,
      show.legend = FALSE
    )
  }

  title <- paste0("Theory-specification comparison: ", .compact_solution_type_names(selected_solution_type))
  if (identical(selected_solution_type, "intermediate") && !is.na(selected_branch)) {
    title <- paste0(title, " | ", selected_branch)
  }

  p <- p +
    ggplot2::scale_colour_manual(
      name = NULL,
      values = colors,
      breaks = theory_names,
      labels = legend_labels
    ) +
    ggplot2::scale_x_continuous(breaks = seq(0, 1, by = 0.25)) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, by = 0.25)) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off") +
    ggplot2::labs(
      title = title,
      x = "Consistency",
      y = "Coverage"
    ) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(
        ncol = 1,
        byrow = TRUE,
        override.aes = list(size = point_size, alpha = 1)
      )
    ) +
    theme +
    ggplot2::theme(
      legend.position = if (isTRUE(legend)) "bottom" else "none",
      legend.text = ggplot2::element_text(hjust = 0),
      plot.margin = ggplot2::margin(5.5, 20, 5.5, 5.5)
    )

  p
}

.theory_plot_select_solution_type <- function(models, solution_type) {
  available <- unique(as.character(models$solution_type))
  available <- available[!is.na(available) & nzchar(available)]

  if (is.null(solution_type)) {
    if (length(available) == 1L) {
      return(available)
    }
    stop(
      "`solution_type` must be supplied when more than one solution type is present. Available solution types: ",
      paste(available, collapse = ", "),
      call. = FALSE
    )
  }

  solution_type <- .plot_solution_filter(solution_type, allow_all = FALSE, arg = "solution_type")
  if (length(solution_type) != 1L) {
    stop("`solution_type` must be a single solution type.", call. = FALSE)
  }
  if (!solution_type %in% available) {
    stop(
      "`solution_type` is not available in this object. Available solution types: ",
      paste(available, collapse = ", "),
      call. = FALSE
    )
  }

  solution_type
}

.theory_plot_select_branch <- function(models, solution_type, intermediate_branch) {
  if (!identical(solution_type, "intermediate")) {
    if (!is.null(intermediate_branch)) {
      stop("`intermediate_branch` is only used when solution_type = 'intermediate'.", call. = FALSE)
    }
    return(NA_character_)
  }

  branch_values <- unique(as.character(models$intermediate_branch[models$solution_type == "intermediate"]))
  branch_values <- branch_values[!is.na(branch_values) & nzchar(branch_values)]

  if (length(branch_values) == 0L) {
    return(NA_character_)
  }

  if (is.null(intermediate_branch)) {
    if (length(branch_values) == 1L) {
      return(branch_values)
    }
    stop(
      "`intermediate_branch` must be supplied when more than one intermediate branch is present. Available branches: ",
      paste(branch_values, collapse = ", "),
      call. = FALSE
    )
  }

  intermediate_branch <- trimws(as.character(intermediate_branch))
  intermediate_branch <- intermediate_branch[nzchar(intermediate_branch)]
  if (length(intermediate_branch) != 1L) {
    stop("`intermediate_branch` must be a single branch name.", call. = FALSE)
  }
  if (!intermediate_branch %in% branch_values) {
    stop(
      "`intermediate_branch` is not available in this object. Available branches: ",
      paste(branch_values, collapse = ", "),
      call. = FALSE
    )
  }

  intermediate_branch
}

.theory_plot_models <- function(models, solution_type, intermediate_branch) {
  keep <- models$solution_type == solution_type
  if (identical(solution_type, "intermediate")) {
    if (is.na(intermediate_branch)) {
      keep <- keep & is.na(models$intermediate_branch)
    } else {
      keep <- keep & !is.na(models$intermediate_branch) & models$intermediate_branch == intermediate_branch
    }
  }

  df <- models[keep, , drop = FALSE]
  df <- df[df$status == "ok" & is.finite(df$inclS) & is.finite(df$covS), , drop = FALSE]
  if (!nrow(df)) {
    return(df)
  }

  theory_levels <- unique(as.character(df$theory))
  df$theory <- factor(as.character(df$theory), levels = theory_levels)
  df
}

.theory_plot_label_positions <- function(df, label_nudge_x, label_nudge_y) {
  label_width <- nchar(as.character(df$theory), type = "width") * 0.006
  right_end <- df$inclS + label_nudge_x + label_width
  left_start <- df$inclS - label_nudge_x - label_width
  needs_left <- right_end > 0.985
  needs_right <- left_start < 0.015
  x_sign <- ifelse(needs_left & !needs_right, -1, 1)

  df$label_x <- pmin(pmax(df$inclS + x_sign * label_nudge_x, 0.01), 0.99)
  df$label_y <- df$covS
  df$label_hjust <- ifelse(x_sign > 0, 0, 1)
  df$label_vjust <- 0.5

  min_sep <- max(0.04, 6 * label_nudge_y)
  for (side in unique(x_sign)) {
    idx <- which(x_sign == side)
    df$label_y[idx] <- .theory_plot_spread_label_y(df$label_y[idx], min_sep = min_sep)
  }

  df$label_y <- pmin(pmax(df$label_y, 0.01), 0.99)
  df
}

.theory_plot_spread_label_y <- function(y, min_sep = 0.04) {
  if (length(y) <= 1L) {
    return(y)
  }

  order_idx <- order(y)
  sorted <- y[order_idx]
  placed <- sorted

  for (i in seq_along(placed)[-1L]) {
    placed[i] <- max(placed[i], placed[i - 1L] + min_sep)
  }

  upper_overflow <- max(placed, na.rm = TRUE) - 0.98
  if (is.finite(upper_overflow) && upper_overflow > 0) {
    placed <- placed - upper_overflow
  }

  lower_overflow <- 0.02 - min(placed, na.rm = TRUE)
  if (is.finite(lower_overflow) && lower_overflow > 0) {
    placed <- placed + lower_overflow
  }

  out <- y
  out[order_idx] <- placed
  out
}

.theory_plot_solution_labels <- function(solutions, solution_type, intermediate_branch, theories) {
  required <- c("theory", "solution_type", "intermediate_branch", "prime_implicant")
  missing <- setdiff(required, names(solutions))
  if (length(missing)) {
    stop(
      "x$results$solutions is missing required column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  keep <- solutions$solution_type == solution_type
  if (identical(solution_type, "intermediate")) {
    if (is.na(intermediate_branch)) {
      keep <- keep & is.na(solutions$intermediate_branch)
    } else {
      keep <- keep & !is.na(solutions$intermediate_branch) & solutions$intermediate_branch == intermediate_branch
    }
  }

  sol <- solutions[keep, , drop = FALSE]
  labels <- stats::setNames(rep(NA_character_, length(theories)), theories)
  if (!nrow(sol)) {
    return(labels)
  }

  for (theory in theories) {
    terms <- unique(as.character(sol$prime_implicant[sol$theory == theory]))
    terms <- terms[!is.na(terms) & nzchar(terms)]
    if (length(terms)) {
      labels[[theory]] <- paste0(theory, ": ", paste(terms, collapse = " + "))
    }
  }

  labels
}

.theory_plot_wrap_label <- function(x, width = 70L) {
  vapply(
    x,
    function(value) {
      value <- gsub(" \\+ ", " + ", value, fixed = TRUE)
      paste(strwrap(value, width = width), collapse = "\n")
    },
    character(1)
  )
}
