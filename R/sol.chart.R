#' Draw a chart from a sol.df table
#'
#' Turns the solution table returned by [sol.df()] into a visual chart
#' of sufficient configurations. The chart uses one column per aligned prime
#' implicant and shows condition presence, condition absence, prime implicant
#' fit, solution fit, and optionally cases.
#'
#' @param x A data frame returned by [sol.df()].
#' @param conditions Optional character vector giving the condition order. If
#'   `NULL`, conditions are inferred from `Prime_Implicants` in order of first
#'   appearance.
#' @param solution_types Solution types to display, in plotting order. Accepted
#'   values follow the common qcaERT solution-type conventions, excluding `"all"`.
#'   `"complex"` is accepted as an alias for `"conservative"` for display use.
#' @param model Optional positive integer selecting which model to display when
#'   `x` contains more than one model.
#' @param intermediate_branch Optional intermediate branch to display when `x`
#'   contains more than one intermediate branch.
#' @param colors Named character vector with colors for `"conservative"`,
#'   `"intermediate"`, and `"parsimonious"`.
#' @param show_cases Logical; if `TRUE`, include a cases row. Cases are taken
#'   from the most detailed available configuration in each column.
#' @param show_pi_fit Logical; if `TRUE`, include prime-implicant consistency,
#'   raw coverage, unique coverage, and PRI rows.
#' @param show_solution_fit Logical; if `TRUE`, include solution-level
#'   consistency, coverage, and PRI rows.
#' @param digits Non-negative integer used to format fit statistics.
#' @param title Optional plot title. If `NULL`, a default title is used.
#' @param note Logical; if `TRUE`, include a caption explaining symbols and
#'   colors.
#' @param legend Logical; if `TRUE`, include ggplot legends for solution type
#'   and condition state.
#' @param point_size Size of presence/absence symbols.
#' @param text_size Size of table text.
#' @param theme A ggplot2 theme object added to the chart.
#'
#' @returns A ggplot object.
#'
#' @details
#' `sol.chart()` is a visual display for [sol.df()] tables. It does not extract
#' solutions from QCA minimization objects. Use [sol.df()] first, then pass the
#' resulting table to `sol.chart()`.
#'
#' Prime implicant columns are aligned according to the following hierarchy:
#' conservative/complex, intermediate, and parsimonious. When several
#' detailed configurations simplify into the same intermediate or parsimonious
#' prime implicant, the simpler prime implicant is repeated across the relevant
#' columns so that the detailed empirical configurations remain visible.
#'
#' Filled circles denote condition presence. Open circles denote condition
#' absence. Empty cells denote conditions that are irrelevant to that prime
#' implicant at the displayed solution type.
#'
#' @examples
#' library(QCA)
#' data(LC)
#'
#' conditions <- c("DEV", "URB", "LIT", "IND", "STB")
#' dir_exp <- rep("1", length(conditions))
#'
#' tt <- truthTable(
#'   data = LC,
#'   outcome = "SURV",
#'   conditions = conditions,
#'   incl.cut = 0.8,
#'   n.cut = 1
#' )
#'
#' enhanced <- findRows(tt, type = 2)
#'
#' con <- minimize(
#'   input = tt,
#'   include = "",
#'   details = TRUE,
#'   show.cases = FALSE
#' )
#'
#' par <- minimize(
#'   input = tt,
#'   include = "?",
#'   exclude = enhanced,
#'   details = TRUE,
#'   show.cases = FALSE
#' )
#'
#' int <- minimize(
#'   input = tt,
#'   include = "?",
#'   dir.exp = dir_exp,
#'   exclude = enhanced,
#'   details = TRUE,
#'   show.cases = FALSE
#' )
#'
#' solution_table <- sol.df(
#'   conservative = con,
#'   parsimonious = par,
#'   intermediate = int,
#'   solution = "all",
#'   which_M = 1,
#'   i_mode = "C1P1",
#'   include_cases = FALSE,
#'   digits = 2
#' )
#'
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   library(ggplot2)
#'
#'   sol.chart(
#'     solution_table,
#'     conditions = conditions,
#'     solution_types = c("conservative", "intermediate", "parsimonious"),
#'     show_cases = FALSE
#'   )
#' }
#'
#' \donttest{
#' library(QCA)
#' library(ggplot2)
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
#' tt <- truthTable(
#'   data = dat,
#'   outcome = outcome,
#'   conditions = conditions,
#'   incl.cut = 0.8,
#'   n.cut = 1,
#'   complete = TRUE,
#'   show.cases = TRUE
#' )
#' enhanced <- findRows(tt, type = 2)
#'
#' con <- minimize(tt, include = "", details = TRUE, show.cases = FALSE)
#' par <- minimize(tt, include = "?", exclude = enhanced, details = TRUE, show.cases = FALSE)
#' int <- minimize(
#'   tt,
#'   include = "?",
#'   dir.exp = dir_exp,
#'   exclude = enhanced,
#'   details = TRUE,
#'   show.cases = FALSE
#' )
#'
#' solution_table <- sol.df(
#'   conservative = con,
#'   intermediate = int,
#'   parsimonious = par,
#'   solution = "all",
#'   which_M = 1,
#'   i_mode = "C1P1",
#'   include_cases = TRUE,
#'   digits = 2
#' )
#'
#' sol.chart(solution_table)
#' }
#'
#' @seealso [sol.df()], [qcaERT_plots]
#' @export
sol.chart <- function(
    x,
    conditions = NULL,
    solution_types = c("conservative", "intermediate", "parsimonious"),
    model = NULL,
    intermediate_branch = NULL,
    colors = c(
      conservative = "#222222",
      intermediate = "#E69F00",
      parsimonious = "#0072B2"
    ),
    show_cases = TRUE,
    show_pi_fit = TRUE,
    show_solution_fit = TRUE,
    digits = 2,
    title = NULL,
    note = TRUE,
    legend = FALSE,
    point_size = 3.2,
    text_size = 3.3,
    theme = .plot_theme()
) {
  .plot_require_ggplot2()

  chart <- .sol_chart_prepare(
    x = x,
    conditions = conditions,
    solution_types = solution_types,
    model = model,
    intermediate_branch = intermediate_branch,
    colors = colors,
    show_cases = show_cases,
    show_pi_fit = show_pi_fit,
    show_solution_fit = show_solution_fit,
    digits = digits
  )

  if (!nrow(chart$grid)) {
    stop("No chart cells are available to draw.", call. = FALSE)
  }

  caption <- NULL
  if (isTRUE(note)) {
    caption <- paste0(
      "Filled circle = presence of condition; open circle = absence of condition; empty cell = irrelevant condition.",
      "\nBlack = conservative/complex; orange = intermediate; blue = parsimonious."
    )
  }

  p <- ggplot2::ggplot(chart$grid, ggplot2::aes(x = .data[["x"]], y = .data[["y"]])) +
    ggplot2::geom_tile(
      ggplot2::aes(width = .data[["width"]], height = .data[["height"]]),
      fill = "white",
      colour = "#222222",
      linewidth = 0.25
    ) +
    ggplot2::geom_text(
      data = chart$labels,
      ggplot2::aes(
        x = .data[["x"]],
        y = .data[["y"]],
        label = .data[["label"]]
      ),
      size = text_size,
      inherit.aes = FALSE
    )

  if (nrow(chart$states)) {
    p <- p +
      ggplot2::geom_point(
        data = chart$states,
        ggplot2::aes(
          x = .data[["x"]],
          y = .data[["y"]],
          colour = .data[["solution_type"]],
          shape = .data[["state"]]
        ),
        size = point_size,
        stroke = 0.8,
        inherit.aes = FALSE
      )
  }

  if (nrow(chart$text)) {
    p <- p +
      ggplot2::geom_text(
        data = chart$text,
        ggplot2::aes(
          x = .data[["x"]],
          y = .data[["y"]],
          label = .data[["label"]],
          colour = .data[["solution_type"]]
        ),
        size = text_size,
        inherit.aes = FALSE
      )
  }

  if (nrow(chart$plain_text)) {
    p <- p +
      ggplot2::geom_text(
        data = chart$plain_text,
        ggplot2::aes(
          x = .data[["x"]],
          y = .data[["y"]],
          label = .data[["label"]]
        ),
        size = text_size,
        inherit.aes = FALSE
      )
  }

  title <- if (is.null(title)) "Sufficient configurations" else as.character(title)[1L]

  p +
    ggplot2::scale_colour_manual(
      name = "Solution type",
      values = chart$colors,
      breaks = names(chart$colors),
      labels = .sol_chart_solution_labels(names(chart$colors)),
      drop = FALSE
    ) +
    ggplot2::scale_shape_manual(
      name = "Condition",
      values = c(present = 16, absent = 1),
      labels = c(present = "Presence", absent = "Absence"),
      drop = FALSE
    ) +
    ggplot2::scale_x_continuous(
      limits = c(-0.5, chart$n_prime_implicants + 0.5),
      breaks = NULL,
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0.5, chart$n_rows + 0.5),
      breaks = NULL,
      expand = c(0, 0)
    ) +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = NULL,
      caption = caption
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    theme +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      legend.position = if (isTRUE(legend)) "bottom" else "none",
      plot.caption = ggplot2::element_text(hjust = 0, size = max(text_size * 2.6, 8)),
      plot.margin = ggplot2::margin(5.5, 12, 5.5, 5.5)
    )
}

.sol_chart_prepare <- function(
    x,
    conditions = NULL,
    solution_types = c("conservative", "intermediate", "parsimonious"),
    model = NULL,
    intermediate_branch = NULL,
    colors = c(
      conservative = "#222222",
      intermediate = "#E69F00",
      parsimonious = "#0072B2"
    ),
    show_cases = TRUE,
    show_pi_fit = TRUE,
    show_solution_fit = TRUE,
    digits = 2
) {
  .sol_chart_validate_input(x)
  solution_types <- .sol_chart_normalize_solution_types(solution_types)
  colors <- .sol_chart_colors(colors)
  digits <- .sol_chart_digits(digits)

  df <- .sol_chart_terms(x)
  df <- df[df$solution_type %in% solution_types, , drop = FALSE]

  if (!nrow(df)) {
    stop("No rows in `x` match the requested `solution_types`.", call. = FALSE)
  }

  df <- .sol_chart_select_model(df, model)

  selected_branch <- .sol_chart_select_intermediate_branch(df, intermediate_branch)
  if (!is.na(selected_branch)) {
    keep_intermediate <- df$solution_type != "intermediate" |
      (!is.na(df$intermediate_branch) & df$intermediate_branch == selected_branch)
    df <- df[keep_intermediate, , drop = FALSE]
  }

  if (!nrow(df)) {
    stop("No solution rows remain after filtering.", call. = FALSE)
  }

  if (is.null(conditions)) {
    conditions <- .sol_chart_infer_conditions(df$states)
  } else {
    conditions <- trimws(as.character(conditions))
    conditions <- conditions[nzchar(conditions)]
    if (!length(conditions)) {
      stop("`conditions` must contain at least one condition name.", call. = FALSE)
    }
    conditions <- unique(conditions)
  }

  aligned_implicants <- .sol_chart_align_prime_implicants(df)
  if (!length(aligned_implicants)) {
    stop("No prime implicants are available to chart.", call. = FALSE)
  }

  used_types <- .sol_chart_used_solution_types(df, aligned_implicants)
  solution_types <- solution_types[solution_types %in% used_types]
  colors <- colors[solution_types]

  structure <- .sol_chart_rows(
    conditions = conditions,
    show_cases = show_cases,
    show_pi_fit = show_pi_fit,
    show_solution_fit = show_solution_fit
  )

  n_prime_implicants <- length(aligned_implicants)
  n_rows <- nrow(structure)
  grid <- .sol_chart_grid(structure, n_prime_implicants)
  grid$y <- n_rows - grid$row + 1

  labels <- .sol_chart_cell_labels(structure, n_prime_implicants)
  labels$y <- n_rows - labels$row + 1

  offsets <- .sol_chart_offsets(solution_types)

  list(
    grid = grid,
    labels = labels,
    states = .sol_chart_state_points(df, aligned_implicants, conditions, structure, offsets),
    text = .sol_chart_value_text(df, aligned_implicants, structure, offsets, solution_types, digits),
    plain_text = .sol_chart_plain_text(df, aligned_implicants, structure),
    colors = colors,
    n_prime_implicants = n_prime_implicants,
    n_rows = n_rows,
    aligned_prime_implicants = aligned_implicants,
    terms = df,
    row_structure = structure
  )
}

.sol_chart_validate_input <- function(x) {
  if (!is.data.frame(x)) {
    stop("`x` must be a data frame returned by sol.df().", call. = FALSE)
  }

  required <- c(
    "Solution",
    "Model",
    "Intermediate_CnPn",
    "Prime_Implicants",
    "Consistency_PI",
    "PRI_PI",
    "Raw_Coverage_PI",
    "Unique_Coverage_PI",
    "Solution_Consistency",
    "Solution_PRI",
    "Solution_Coverage",
    "Cases"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "`x` must be a table produced by sol.df(). Missing column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

.sol_chart_normalize_solution_types <- function(solution_types) {
  if (is.null(solution_types)) {
    stop("`solution_types` cannot be NULL.", call. = FALSE)
  }

  out <- vapply(solution_types, .sol_chart_normalize_solution_type, character(1))
  out <- out[nzchar(out)]
  out <- unique(out)
  if (!length(out)) {
    stop("`solution_types` must contain at least one solution type.", call. = FALSE)
  }
  if (any(out == "all")) {
    stop("`solution_types` cannot include 'all'.", call. = FALSE)
  }

  out
}

.sol_chart_normalize_solution_type <- function(x) {
  x <- tolower(trimws(as.character(x)))
  if (identical(x, "complex")) {
    x <- "conservative"
  }
  .normalize_solution_std(x)
}

.sol_chart_colors <- function(colors) {
  defaults <- c(
    conservative = "#222222",
    intermediate = "#E69F00",
    parsimonious = "#0072B2"
  )
  if (is.null(colors)) {
    return(defaults)
  }
  if (!is.character(colors) || is.null(names(colors))) {
    stop("`colors` must be a named character vector.", call. = FALSE)
  }

  colors <- colors[!is.na(names(colors)) & nzchar(names(colors))]
  names(colors) <- vapply(names(colors), .sol_chart_normalize_solution_type, character(1))
  defaults[names(colors)] <- colors
  defaults
}

.sol_chart_digits <- function(digits) {
  if (!is.numeric(digits) || length(digits) != 1L || !is.finite(digits) || digits < 0) {
    stop("`digits` must be a single non-negative integer.", call. = FALSE)
  }
  as.integer(digits)
}

.sol_chart_select_model <- function(df, model) {
  models <- suppressWarnings(as.integer(as.character(df$Model)))
  available <- sort(unique(models[!is.na(models)]))

  if (!length(available)) {
    if (!is.null(model)) {
      stop("`model` was supplied, but no model identifiers are available in `x`.", call. = FALSE)
    }
    return(df)
  }

  if (is.null(model)) {
    if (length(available) == 1L) {
      return(df[models == available[[1L]], , drop = FALSE])
    }
    stop(
      "`model` must be supplied when more than one model is present. Available models: ",
      paste(available, collapse = ", "),
      call. = FALSE
    )
  }

  model <- .as_integerish_scalar(model, "model", min = 1L)
  if (!model %in% available) {
    stop(
      "`model` is not available in `x`. Available models: ",
      paste(available, collapse = ", "),
      call. = FALSE
    )
  }

  df[models == model, , drop = FALSE]
}

.sol_chart_terms <- function(x) {
  out <- x
  out$solution_type <- vapply(out$Solution, .sol_chart_normalize_solution_type, character(1))
  out$intermediate_branch <- .sol_chart_clean_text(out$Intermediate_CnPn)
  out$prime_implicant <- trimws(as.character(out$Prime_Implicants))
  out$term_id <- seq_len(nrow(out))
  out$states <- lapply(out$prime_implicant, .sol_chart_parse_term)
  out$n_literals <- vapply(out$states, length, integer(1))
  out
}

.sol_chart_clean_text <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | x %in% c("", "-", "NA", "<NA>")] <- NA_character_
  x
}

.sol_chart_select_intermediate_branch <- function(df, intermediate_branch) {
  branches <- unique(df$intermediate_branch[df$solution_type == "intermediate"])
  branches <- branches[!is.na(branches) & nzchar(branches)]

  if (!length(branches)) {
    if (!is.null(intermediate_branch)) {
      stop("`intermediate_branch` was supplied, but no intermediate branch is available in `x`.", call. = FALSE)
    }
    return(NA_character_)
  }

  if (is.null(intermediate_branch)) {
    if (length(branches) == 1L) {
      return(branches)
    }
    stop(
      "`intermediate_branch` must be supplied when more than one intermediate branch is present. Available branches: ",
      paste(branches, collapse = ", "),
      call. = FALSE
    )
  }

  intermediate_branch <- trimws(as.character(intermediate_branch))
  intermediate_branch <- intermediate_branch[nzchar(intermediate_branch)]
  if (length(intermediate_branch) != 1L) {
    stop("`intermediate_branch` must be a single branch name.", call. = FALSE)
  }
  if (!intermediate_branch %in% branches) {
    stop(
      "`intermediate_branch` is not available in `x`. Available branches: ",
      paste(branches, collapse = ", "),
      call. = FALSE
    )
  }

  intermediate_branch
}

.sol_chart_parse_term <- function(term) {
  term <- trimws(as.character(term)[1L])
  if (is.na(term) || !nzchar(term) || term %in% c("-", "<no selected solution>")) {
    return(stats::setNames(character(0), character(0)))
  }

  tokens <- unlist(strsplit(term, "*", fixed = TRUE), use.names = FALSE)
  tokens <- trimws(tokens)
  tokens <- tokens[nzchar(tokens)]
  if (!length(tokens)) {
    return(stats::setNames(character(0), character(0)))
  }

  states <- character(length(tokens))
  names <- character(length(tokens))
  for (i in seq_along(tokens)) {
    token <- gsub("\\s+", "", tokens[[i]])
    if (startsWith(token, "~")) {
      states[[i]] <- "absent"
      names[[i]] <- substring(token, 2L)
    } else {
      states[[i]] <- "present"
      names[[i]] <- token
    }
  }

  keep <- nzchar(names)
  stats::setNames(states[keep], names[keep])
}

.sol_chart_infer_conditions <- function(states) {
  out <- character(0)
  for (state in states) {
    nms <- names(state)
    out <- c(out, nms[!nms %in% out])
  }
  if (!length(out)) {
    stop("Could not infer condition names from `Prime_Implicants`; supply `conditions`.", call. = FALSE)
  }
  out
}

.sol_chart_align_prime_implicants <- function(df) {
  order_types <- c("conservative", "intermediate", "parsimonious")
  available <- order_types[order_types %in% unique(df$solution_type)]
  if (!length(available)) {
    return(list())
  }

  by_type <- lapply(available, function(tp) df[df$solution_type == tp, , drop = FALSE])
  names(by_type) <- available

  anchor_type <- available[[1L]]
  aligned <- vector("list", nrow(by_type[[anchor_type]]))
  for (i in seq_len(nrow(by_type[[anchor_type]]))) {
    row <- by_type[[anchor_type]][i, , drop = FALSE]
    column <- stats::setNames(rep(NA_integer_, length(available)), available)
    column[[anchor_type]] <- row$term_id
    base_states <- row$states[[1L]]

    for (tp in .sol_chart_later_types(available, anchor_type)) {
      candidate <- .sol_chart_best_match(by_type[[tp]], base_states)
      if (!is.na(candidate)) {
        column[[tp]] <- candidate
        base_states <- df$states[[match(candidate, df$term_id)]]
      }
    }

    aligned[[i]] <- column
  }

  aligned <- .sol_chart_add_unmatched_prime_implicants(aligned, df, by_type, available)
  names(aligned) <- paste0("PI ", seq_along(aligned))
  aligned
}

.sol_chart_add_unmatched_prime_implicants <- function(aligned, df, by_type, available) {
  used <- unlist(aligned, use.names = FALSE)
  used <- used[!is.na(used)]

  for (tp in available) {
    current <- by_type[[tp]]
    unused <- current$term_id[!current$term_id %in% used]
    if (!length(unused)) {
      next
    }

    later <- .sol_chart_later_types(available, tp)
    for (term_id in unused) {
      column <- stats::setNames(rep(NA_integer_, length(available)), available)
      column[[tp]] <- term_id
      base_states <- df$states[[match(term_id, df$term_id)]]

      for (later_type in later) {
        candidate <- .sol_chart_best_match(by_type[[later_type]], base_states)
        if (!is.na(candidate)) {
          column[[later_type]] <- candidate
          base_states <- df$states[[match(candidate, df$term_id)]]
        }
      }

      aligned[[length(aligned) + 1L]] <- column
      used <- c(used, term_id, column[!is.na(column)])
    }
  }

  aligned
}

.sol_chart_later_types <- function(available, solution_type) {
  idx <- match(solution_type, available)
  if (is.na(idx) || idx >= length(available)) {
    return(character(0))
  }
  available[(idx + 1L):length(available)]
}

.sol_chart_best_match <- function(candidates, detailed_states) {
  if (!nrow(candidates)) {
    return(NA_integer_)
  }

  ok <- vapply(candidates$states, .sol_chart_covers, logical(1), detailed = detailed_states)
  if (!any(ok)) {
    return(NA_integer_)
  }

  candidate_rows <- candidates[ok, , drop = FALSE]
  candidate_rows <- candidate_rows[order(-candidate_rows$n_literals, candidate_rows$term_id), , drop = FALSE]
  candidate_rows$term_id[[1L]]
}

.sol_chart_covers <- function(simple, detailed) {
  simple_names <- names(simple)
  if (!length(simple_names)) {
    return(TRUE)
  }
  if (!all(simple_names %in% names(detailed))) {
    return(FALSE)
  }
  all(unname(simple) == unname(detailed[simple_names]))
}

.sol_chart_used_solution_types <- function(df, aligned_implicants) {
  ids <- unlist(aligned_implicants, use.names = FALSE)
  ids <- ids[!is.na(ids)]
  unique(df$solution_type[df$term_id %in% ids])
}

.sol_chart_rows <- function(conditions, show_cases, show_pi_fit, show_solution_fit) {
  rows <- data.frame(
    row = integer(0),
    key = character(0),
    label = character(0),
    kind = character(0),
    stringsAsFactors = FALSE
  )

  add <- function(key, label, kind) {
    rows <<- rbind(
      rows,
      data.frame(
        row = nrow(rows) + 1L,
        key = key,
        label = label,
        kind = kind,
        stringsAsFactors = FALSE
      )
    )
  }

  add("header", "Conditions", "header")
  for (condition in conditions) {
    add(condition, condition, "condition")
  }
  if (isTRUE(show_cases)) {
    add("cases", "Cases", "cases")
  }
  if (isTRUE(show_pi_fit)) {
    add("Consistency_PI", "PI consistency", "pi_fit")
    add("Raw_Coverage_PI", "PI raw coverage", "pi_fit")
    add("Unique_Coverage_PI", "PI unique coverage", "pi_fit")
    add("PRI_PI", "PI PRI", "pi_fit")
  }
  if (isTRUE(show_solution_fit)) {
    add("Solution_Consistency", "Solution consistency", "solution_fit")
    add("Solution_Coverage", "Solution coverage", "solution_fit")
    add("Solution_PRI", "Solution PRI", "solution_fit")
  }

  rows
}

.sol_chart_cell_labels <- function(structure, n_prime_implicants) {
  labels <- data.frame(
    row = integer(0),
    x = numeric(0),
    label = character(0),
    stringsAsFactors = FALSE
  )

  labels <- rbind(
    labels,
    data.frame(row = 1L, x = 0, label = structure$label[[1L]], stringsAsFactors = FALSE)
  )
  labels <- rbind(
    labels,
    data.frame(row = 1L, x = seq_len(n_prime_implicants), label = paste0("PI ", seq_len(n_prime_implicants)), stringsAsFactors = FALSE)
  )

  row_labels <- structure[structure$kind != "header", , drop = FALSE]
  labels <- rbind(
    labels,
    data.frame(row = row_labels$row, x = 0, label = row_labels$label, stringsAsFactors = FALSE)
  )

  labels
}

.sol_chart_grid <- function(structure, n_prime_implicants) {
  rows <- list()

  for (i in seq_len(nrow(structure))) {
    row <- structure$row[[i]]
    kind <- structure$kind[[i]]

    rows[[length(rows) + 1L]] <- data.frame(
      row = row,
      col = 0,
      x = 0,
      width = 1,
      height = 1,
      stringsAsFactors = FALSE
    )

    if (identical(kind, "solution_fit")) {
      rows[[length(rows) + 1L]] <- data.frame(
        row = row,
        col = NA_integer_,
        x = mean(seq_len(n_prime_implicants)),
        width = n_prime_implicants,
        height = 1,
        stringsAsFactors = FALSE
      )
    } else {
      rows[[length(rows) + 1L]] <- data.frame(
        row = row,
        col = seq_len(n_prime_implicants),
        x = seq_len(n_prime_implicants),
        width = 1,
        height = 1,
        stringsAsFactors = FALSE
      )
    }
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

.sol_chart_offsets <- function(solution_types) {
  if (length(solution_types) == 1L) {
    return(stats::setNames(0, solution_types))
  }
  stats::setNames(seq(-0.24, 0.24, length.out = length(solution_types)), solution_types)
}

.sol_chart_state_points <- function(df, aligned_implicants, conditions, structure, offsets) {
  rows <- list()
  condition_rows <- structure[structure$kind == "condition", , drop = FALSE]

  for (pi_i in seq_along(aligned_implicants)) {
    column <- aligned_implicants[[pi_i]]
    for (solution_type in names(column)) {
      term_id <- column[[solution_type]]
      if (is.na(term_id) || !solution_type %in% names(offsets)) {
        next
      }
      term <- df[df$term_id == term_id, , drop = FALSE]
      states <- term$states[[1L]]

      for (condition in conditions) {
        if (!condition %in% names(states)) {
          next
        }
        row_number <- condition_rows$row[condition_rows$key == condition]
        rows[[length(rows) + 1L]] <- data.frame(
          x = pi_i + offsets[[solution_type]],
          y = nrow(structure) - row_number + 1,
          solution_type = solution_type,
          state = states[[condition]],
          stringsAsFactors = FALSE
        )
      }
    }
  }

  .sol_chart_bind_or_empty(rows, c("x", "y", "solution_type", "state"))
}

.sol_chart_value_text <- function(df, aligned_implicants, structure, offsets, solution_types, digits) {
  rows <- list()
  pi_rows <- structure[structure$kind == "pi_fit", , drop = FALSE]

  for (pi_i in seq_along(aligned_implicants)) {
    column <- aligned_implicants[[pi_i]]
    for (solution_type in names(column)) {
      term_id <- column[[solution_type]]
      if (is.na(term_id) || !solution_type %in% names(offsets)) {
        next
      }
      term <- df[df$term_id == term_id, , drop = FALSE]

      for (i in seq_len(nrow(pi_rows))) {
        value <- .sol_chart_format_number(term[[pi_rows$key[[i]]]], digits)
        if (!nzchar(value)) {
          next
        }
        rows[[length(rows) + 1L]] <- data.frame(
          x = pi_i + offsets[[solution_type]],
          y = nrow(structure) - pi_rows$row[[i]] + 1,
          label = value,
          solution_type = solution_type,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  solution_rows <- structure[structure$kind == "solution_fit", , drop = FALSE]
  if (nrow(solution_rows)) {
    center <- mean(seq_along(aligned_implicants))
    for (solution_type in solution_types) {
      one <- df[df$solution_type == solution_type, , drop = FALSE]
      if (!nrow(one) || !solution_type %in% names(offsets)) {
        next
      }
      for (i in seq_len(nrow(solution_rows))) {
        value <- .sol_chart_first_number(one[[solution_rows$key[[i]]]], digits)
        if (!nzchar(value)) {
          next
        }
        rows[[length(rows) + 1L]] <- data.frame(
          x = center + offsets[[solution_type]],
          y = nrow(structure) - solution_rows$row[[i]] + 1,
          label = value,
          solution_type = solution_type,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  .sol_chart_bind_or_empty(rows, c("x", "y", "label", "solution_type"))
}

.sol_chart_plain_text <- function(df, aligned_implicants, structure) {
  cases_row <- structure[structure$kind == "cases", , drop = FALSE]
  if (!nrow(cases_row)) {
    return(.sol_chart_bind_or_empty(list(), c("x", "y", "label")))
  }

  rows <- list()
  for (pi_i in seq_along(aligned_implicants)) {
    term_id <- .sol_chart_anchor_term_id(aligned_implicants[[pi_i]])
    if (is.na(term_id)) {
      next
    }
    term <- df[df$term_id == term_id, , drop = FALSE]
    label <- .sol_chart_clean_text(term$Cases)
    if (length(label) != 1L || is.na(label) || !nzchar(label)) {
      next
    }

    rows[[length(rows) + 1L]] <- data.frame(
      x = pi_i,
      y = nrow(structure) - cases_row$row[[1L]] + 1,
      label = .sol_chart_wrap(label),
      stringsAsFactors = FALSE
    )
  }

  .sol_chart_bind_or_empty(rows, c("x", "y", "label"))
}

.sol_chart_anchor_term_id <- function(aligned_implicant) {
  ids <- aligned_implicant[!is.na(aligned_implicant)]
  if (!length(ids)) {
    return(NA_integer_)
  }
  ids[[1L]]
}

.sol_chart_format_number <- function(x, digits) {
  x <- suppressWarnings(as.numeric(x))[1L]
  if (is.na(x) || !is.finite(x)) {
    return("")
  }
  formatC(x, format = "f", digits = digits)
}

.sol_chart_first_number <- function(x, digits) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (!length(x)) {
    return("")
  }
  .sol_chart_format_number(x[[1L]], digits)
}

.sol_chart_wrap <- function(x, width = 22L) {
  paste(strwrap(as.character(x), width = width), collapse = "\n")
}

.sol_chart_bind_or_empty <- function(rows, cols) {
  if (!length(rows)) {
    out <- as.data.frame(stats::setNames(rep(list(logical(0)), length(cols)), cols))
    for (nm in cols) {
      if (nm %in% c("x", "y")) out[[nm]] <- numeric(0)
      else out[[nm]] <- character(0)
    }
    return(out)
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

.sol_chart_solution_labels <- function(solution_types) {
  labels <- c(
    conservative = "Conservative/complex",
    intermediate = "Intermediate",
    parsimonious = "Parsimonious"
  )
  unname(labels[solution_types])
}
