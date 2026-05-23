.normalize_i_mode <- function(i_mode = NULL) {
  if (is.null(i_mode)) {
    return(list())
  }

  i_mode <- match.arg(i_mode, c("all", "C1P1"))

  list(
    i_mode = i_mode
  )
}

.normalize_i_solution_names <- function(i.sol) {
  nm <- names(i.sol)

  if (is.null(nm) || any(nm == "")) {
    nm <- paste0("I", seq_along(i.sol))
  }

  nm
}

.select_single_model_position <- function(n_models, which_M, context) {
  if (n_models < 1L) return(integer(0))

  if (which_M > n_models) {
    stop("Requested `which_M = ", which_M, "` but only ", n_models, " model(s) are available for ", context, ".")
  }

  which_M
}

.resolve_branches <- function(obj, i_mode, branches) {
  if (is.null(obj[["i.sol"]]) || length(obj[["i.sol"]]) == 0L) {
    stop("The supplied intermediate object has no `i.sol` component.")
  }

  available <- names(obj[["i.sol"]])

  if (is.null(available) || any(available == "")) {
    stop("The supplied intermediate object has unnamed `i.sol` branches.")
  }

  if (!is.null(branches)) {
    if (!is.character(branches) || length(branches) < 1L) {
      stop("`branches` must be NULL or a non-empty character vector.")
    }

    missing_branches <- setdiff(branches, available)

    if (length(missing_branches) > 0L) {
      stop("Requested branch(es) not found in `i.sol`: ", paste(missing_branches, collapse = ", "))
    }

    return(branches)
  }

  if (i_mode == "C1P1") {
    if (!"C1P1" %in% available) {
      stop("`i_mode = \"C1P1\"` requested, but no `C1P1` branch exists in `i.sol`.")
    }

    return("C1P1")
  }

  available
}
