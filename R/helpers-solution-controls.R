.solution_message <- function(style, plain, std) {
  style <- match.arg(style, c("std", "plain"))
  if (style == "std") std else plain
}

.normalize_solution <- function(solution, style = c("std", "plain")) {
  style <- match.arg(style)
  msg <- .solution_message(
    style,
    plain = "solution must be one of 'all', 'con'/'conservative', 'par'/'parsimonious', or 'int'/'intermediate'.",
    std = "`solution` must be one of 'all', 'con'/'conservative', 'par'/'parsimonious', or 'int'/'intermediate'."
  )

  if (!is.character(solution) || length(solution) != 1L || is.na(solution)) {
    stop(msg)
  }

  s <- tolower(trimws(solution))

  if (s == "all") return("all")
  if (s %in% c("con", "conservative")) return("conservative")
  if (s %in% c("par", "parsimonious")) return("parsimonious")
  if (s %in% c("int", "intermediate")) return("intermediate")

  stop(msg)
}

.normalize_solution_std <- function(solution) {
  .normalize_solution(solution, style = "std")
}

.normalize_include <- function(include, caller, style = c("std", "plain")) {
  style <- match.arg(style)
  if (is.null(include)) return(NULL)

  msg <- .solution_message(
    style,
    plain = "include must be NULL, '', or '?'.",
    std = "`include` must be NULL, '', or '?'."
  )
  if (!is.character(include) || length(include) != 1L || is.na(include)) {
    stop(msg)
  }

  include <- trimws(include)

  if (!include %in% c("", "?")) {
    stop(.solution_message(
      style,
      plain = sprintf("For %s, include must be NULL, '', or '?'.", caller),
      std = sprintf("For `%s`, `include` must be NULL, '', or '?'.", caller)
    ))
  }

  include
}

.split_dir_exp_string_generic <- function(x, conditions, endpoint_phrase) {
  x <- trimws(as.character(x))
  if (!nzchar(x)) return(character(0))

  if (grepl(":", x, fixed = TRUE)) {
    des <- trimws(unlist(strsplit(x, split = ":", fixed = TRUE), use.names = FALSE))
    des <- des[nzchar(des)]

    if (length(des) != 2L || !all(des %in% conditions)) {
      stop(
        sprintf(
          "dir.exp contains an invalid condition sequence. When using 'A:B', both endpoints must be condition names in %s.",
          endpoint_phrase
        )
      )
    }

    return(conditions[seq(match(des[1], conditions), match(des[2], conditions))])
  }

  out <- trimws(unlist(strsplit(x, "[[:space:],]+"), use.names = FALSE))
  out[nzchar(out)]
}

.normalize_dir_exp_generic <- function(dir.exp, conditions, endpoint_phrase) {
  if (is.null(dir.exp)) return(NULL)

  if (is.character(dir.exp) && length(dir.exp) == 1L && identical(dir.exp, "character(0)")) {
    return(NULL)
  }

  if (is.character(dir.exp) && length(dir.exp) == 0L) {
    return(NULL)
  }

  if (is.data.frame(dir.exp)) {
    dir.exp <- as.matrix(dir.exp)
  }

  if (is.matrix(dir.exp)) {
    if (ncol(dir.exp) != length(conditions)) {
      stop(
        sprintf(
          "dir.exp must have exactly one column per condition: expected %d column(s) for %d condition(s), got %d.",
          length(conditions), length(conditions), ncol(dir.exp)
        )
      )
    }
    return(dir.exp)
  }

  if (is.character(dir.exp)) {
    if (length(dir.exp) == 1L) {
      dir.exp <- .split_dir_exp_string_generic(
        x = dir.exp,
        conditions = conditions,
        endpoint_phrase = endpoint_phrase
      )
    } else {
      dir.exp <- trimws(as.character(dir.exp))
      dir.exp <- dir.exp[nzchar(dir.exp)]
    }

    if (length(dir.exp) == 0L) {
      return(NULL)
    }

    if (length(dir.exp) != length(conditions)) {
      stop(
        sprintf(
          "dir.exp must specify exactly one expectation per condition: expected %d value(s) for %d condition(s), got %d.",
          length(conditions), length(conditions), length(dir.exp)
        )
      )
    }

    return(dir.exp)
  }

  stop("dir.exp must be NULL, a character vector, a single minimize-style character string, or a matrix/data.frame with one column per condition.")
}

.resolve_solution_controls <- function(solution, include, dir.exp, caller, style = c("std", "plain")) {
  style <- match.arg(style)
  solution <- .normalize_solution(solution, style = style)
  include <- .normalize_include(include, caller, style = style)

  if (solution == "all") {
    if (!is.null(include)) {
      stop(.solution_message(
        style,
        plain = "When solution = 'all', do not supply include.",
        std = "When `solution = \"all\"`, do not supply `include`."
      ))
    }

    monitored <- c("conservative", "parsimonious")
    if (!is.null(dir.exp)) {
      monitored <- c(monitored, "intermediate")
    }

    return(list(
      solution = solution,
      include = NULL,
      monitored = monitored
    ))
  }

  if (solution == "conservative") {
    if (!is.null(include) && !identical(include, "")) {
      stop(.solution_message(
        style,
        plain = "When solution = 'conservative'/'con', include must be ''.",
        std = "When `solution = \"conservative\"`/`\"con\"`, `include` must be ''."
      ))
    }
    if (!is.null(dir.exp)) {
      stop(.solution_message(
        style,
        plain = "When solution = 'conservative'/'con', dir.exp must be NULL.",
        std = "When `solution = \"conservative\"`/`\"con\"`, `dir.exp` must be NULL."
      ))
    }

    return(list(
      solution = solution,
      include = "",
      monitored = "conservative"
    ))
  }

  if (solution == "parsimonious") {
    if (!is.null(include) && !identical(include, "?")) {
      stop(.solution_message(
        style,
        plain = "When solution = 'parsimonious'/'par', include must be '?'.",
        std = "When `solution = \"parsimonious\"`/`\"par\"`, `include` must be '?'."
      ))
    }
    if (!is.null(dir.exp)) {
      stop(.solution_message(
        style,
        plain = "When solution = 'parsimonious'/'par', dir.exp must be NULL.",
        std = "When `solution = \"parsimonious\"`/`\"par\"`, `dir.exp` must be NULL."
      ))
    }

    return(list(
      solution = solution,
      include = "?",
      monitored = "parsimonious"
    ))
  }

  if (!is.null(include) && !identical(include, "?")) {
    stop(.solution_message(
      style,
      plain = "When solution = 'intermediate'/'int', include must be '?'.",
      std = "When `solution = \"intermediate\"`/`\"int\"`, `include` must be '?'."
    ))
  }
  if (is.null(dir.exp)) {
    stop(.solution_message(
      style,
      plain = "When solution = 'intermediate'/'int', dir.exp must be provided.",
      std = "When `solution = \"intermediate\"`/`\"int\"`, `dir.exp` must be provided."
    ))
  }

  list(
    solution = solution,
    include = "?",
    monitored = "intermediate"
  )
}
