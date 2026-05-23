if (getRversion() >= "2.15.1") {
  utils::globalVariables(".data")
}

.plot_require_ggplot2 <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required to plot qcaERT results.", call. = FALSE)
  }

  invisible(TRUE)
}

.plot_colors <- function() {
  c(
    blue = "#0072B2",
    sky = "#56B4E9",
    green = "#009E73",
    yellow = "#F0E442",
    orange = "#E69F00",
    vermillion = "#D55E00",
    purple = "#CC79A7",
    grey = "#7A7A7A",
    light_grey = "#EAEAEA",
    dark = "#222222"
  )
}

.plot_theme <- function(base_size = 12) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(colour = .plot_colors()[["grey"]], fill = NA),
      strip.background = ggplot2::element_rect(fill = .plot_colors()[["light_grey"]], colour = .plot_colors()[["grey"]]),
      legend.key = ggplot2::element_blank(),
      legend.title = ggplot2::element_text(size = base_size),
      plot.title = ggplot2::element_text(face = "plain")
    )
}

.plot_preserved_colors <- function() {
  c(
    Yes = .plot_colors()[["blue"]],
    No = .plot_colors()[["vermillion"]],
    Unknown = .plot_colors()[["grey"]]
  )
}

.plot_boundary_colors <- function() {
  c(
    "Baseline" = .plot_colors()[["grey"]],
    "Lower last safe" = .plot_colors()[["orange"]],
    "Upper last safe" = .plot_colors()[["blue"]]
  )
}

.plot_theory_colors <- function(theories) {
  theories <- as.character(theories)
  base <- .plot_colors()[c("blue", "orange", "green", "purple", "sky", "vermillion", "yellow", "dark")]
  stats::setNames(rep(base, length.out = length(theories)), theories)
}

.plot_normalize_directions <- function(value, arg = "directions") {
  if (is.null(value)) return(NULL)

  value <- tolower(trimws(as.character(value)))
  value <- value[nzchar(value)]
  if (!length(value)) return(character(0))

  if (!all(value %in% c("lower", "upper"))) {
    stop(sprintf("`%s` must contain only 'lower' and/or 'upper'.", arg), call. = FALSE)
  }

  unique(value)
}

.plot_as_chr0 <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x
}

.plot_split_tokens <- function(x, normalize_solution = FALSE, allow_all = FALSE) {
  x <- .plot_as_chr0(x)
  x <- x[nzchar(x)]
  if (!length(x)) return(character(0))

  tokens <- unlist(strsplit(x, ",", fixed = TRUE), use.names = FALSE)
  tokens <- trimws(tokens)
  tokens <- tokens[nzchar(tokens)]
  if (!length(tokens)) return(character(0))

  if (normalize_solution) {
    tokens <- vapply(tokens, .normalize_solution_std, character(1))
    if (!allow_all && any(tokens == "all")) {
      stop("Solution-token filters must use 'con'/'conservative', 'par'/'parsimonious', or 'int'/'intermediate'.", call. = FALSE)
    }
  }

  sort(unique(tokens))
}

.plot_solution_filter <- function(x, allow_all = TRUE, arg = "solution") {
  if (is.null(x)) return(NULL)

  x <- trimws(as.character(x))
  x <- x[nzchar(x)]
  if (!length(x)) return(character(0))

  out <- vapply(x, .normalize_solution_std, character(1))
  if (!allow_all && any(out == "all")) {
    stop(sprintf("`%s` must use 'con'/'conservative', 'par'/'parsimonious', or 'int'/'intermediate'.", arg), call. = FALSE)
  }

  sort(unique(out))
}

.plot_reject_solution_dots <- function(dots, caller) {
  if (!length(dots)) return(invisible(TRUE))
  nms <- names(dots)
  if (is.null(nms)) return(invisible(TRUE))

  blocked <- intersect(nms, c("solution", "monitored_solutions"))
  if (length(blocked)) {
    stop(
      "`", caller, "()` does not accept `", blocked[[1L]],
      "`. Use `solution_type` to select conservative, intermediate, or parsimonious plots.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

.plot_reject_solution_args <- function(solution, monitored_solutions, caller) {
  if (!is.null(solution)) {
    stop(
      "`", caller, "()` does not accept `solution`. Use `solution_type` to select conservative, intermediate, or parsimonious plots.",
      call. = FALSE
    )
  }
  if (!is.null(monitored_solutions)) {
    stop(
      "`", caller, "()` does not accept `monitored_solutions`. Use `solution_type` to select conservative, intermediate, or parsimonious plots.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

.plot_row_solution_type <- function(row) {
  if ("solution_type" %in% names(row)) {
    value <- .plot_as_chr0(row$solution_type[[1L]])
    if (nzchar(value)) return(.normalize_solution_std(value))
  }

  if ("monitored_solutions" %in% names(row)) {
    tokens <- .plot_split_tokens(row$monitored_solutions[[1L]], normalize_solution = TRUE, allow_all = FALSE)
    if (length(tokens) == 1L) return(tokens[[1L]])
  }

  if ("solution" %in% names(row)) {
    value <- .plot_as_chr0(row$solution[[1L]])
    if (nzchar(value) && !identical(value, "all")) return(.normalize_solution_std(value))
  }

  NA_character_
}

.plot_ensure_solution_type <- function(df) {
  df$solution_type <- vapply(
    seq_len(nrow(df)),
    function(i) .plot_row_solution_type(df[i, , drop = FALSE]),
    character(1)
  )
  df
}

.plot_select_solution_type <- function(df, solution_type) {
  df <- .plot_ensure_solution_type(df)
  available <- .result_solution_type_order(df$solution_type)

  if (!length(available)) {
    stop("No solution types are available to plot.", call. = FALSE)
  }

  if (is.null(solution_type)) {
    if (length(available) == 1L) {
      return(list(data = df[df$solution_type == available[[1L]], , drop = FALSE], solution_type = available[[1L]]))
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

  list(data = df[df$solution_type == solution_type, , drop = FALSE], solution_type = solution_type)
}

.plot_match_solution_tokens <- function(values, requested, arg = "solution") {
  requested <- .plot_solution_filter(requested, allow_all = FALSE, arg = arg)
  values <- .plot_as_chr0(values)
  keep <- rep(FALSE, length(values))

  for (i in seq_along(values)) {
    tokens <- .plot_split_tokens(values[i], normalize_solution = FALSE)
    keep[i] <- any(tokens %in% requested)
  }

  keep[is.na(keep)] <- FALSE
  keep
}

.plot_fmt_counts <- function(tab) {
  names <- names(tab)
  paste0(names, " (", as.integer(tab), ")", collapse = ", ")
}
