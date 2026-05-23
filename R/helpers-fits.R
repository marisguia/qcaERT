.fit_get <- function(x, nm) {
  if (is.null(x) || length(x) == 0L) return(NULL)
  nms <- names(x)
  if (is.null(nms) || !nm %in% nms) return(NULL)
  x[[match(nm, nms)]]
}

.fit_exceeds_tol <- function(delta, tol = 0) {
  if (is.na(delta)) return(NA)
  abs(delta) > (tol + sqrt(.Machine$double.eps))
}

.fit_changed <- function(base, cur, tol = 0) {
  if (is.null(base) && is.null(cur)) return(FALSE)
  if (xor(is.null(base), is.null(cur))) return(TRUE)

  nms <- union(names(base), names(cur))
  if (length(nms) == 0L) return(FALSE)

  for (nm in nms) {
    b <- .fit_get(base, nm)
    c <- .fit_get(cur, nm)

    if (is.null(b) && is.null(c)) next
    if (is.null(b) || is.null(c)) return(TRUE)
    if (xor(is.na(b), is.na(c))) return(TRUE)
    if (is.na(b) && is.na(c)) next
    if (isTRUE(.fit_exceeds_tol(c - b, tol = tol))) return(TRUE)
    }

  FALSE
}

.fit_change_names <- function(base, cur, tol = 0) {
  if (is.null(base) && is.null(cur)) return(character(0))
  if (xor(is.null(base), is.null(cur))) {
    return(union(names(base), names(cur)))
  }

  nms <- union(names(base), names(cur))
  out <- character(0)

  for (nm in nms) {
    b <- .fit_get(base, nm)
    c <- .fit_get(cur, nm)

    if (is.null(b) && is.null(c)) {
      next
    } else if (is.null(b) || is.null(c)) {
      out <- c(out, nm)
    } else if (xor(is.na(b), is.na(c))) {
      out <- c(out, nm)
    } else if (!(is.na(b) && is.na(c)) && isTRUE(.fit_exceeds_tol(c - b, tol = tol))) {
      out <- c(out, nm)
    }
  }

  sort(unique(out))
}

.fit_delta_vec <- function(base, cur) {
  if (is.null(base) && is.null(cur)) return(numeric(0))

  nms <- union(names(base), names(cur))
  out <- numeric(0)

  for (nm in nms) {
    b <- .fit_get(base, nm)
    c <- .fit_get(cur, nm)

    val <- if (is.null(b) && is.null(c)) {
      NA_real_
    } else if (is.null(b) || is.null(c) || xor(is.na(b), is.na(c))) {
      NA_real_
    } else if (is.na(b) && is.na(c)) {
      NA_real_
    } else {
      c - b
    }

    out <- c(out, val)
    names(out)[length(out)] <- nm
  }

  out
}

.fit_abs_max <- function(x) {
  if (is.null(x) || length(x) == 0L) return(NA_real_)
  x <- x[is.finite(x)]
  if (length(x) == 0L) return(NA_real_)
  max(abs(x))
}

.fit_vec_from_ic <- function(ic_obj, which_m) {
  if (is.null(ic_obj) || !is.list(ic_obj)) return(NULL)

  if ("individual" %in% names(ic_obj) &&
      length(ic_obj$individual) >= which_m &&
      !is.null(ic_obj$individual[[which_m]]) &&
      is.list(ic_obj$individual[[which_m]]) &&
      "sol.incl.cov" %in% names(ic_obj$individual[[which_m]])) {
    sic <- ic_obj$individual[[which_m]]$sol.incl.cov
    if (is.data.frame(sic)) sic <- as.matrix(sic)
    if (is.matrix(sic)) {
      if (nrow(sic) < 1L) return(NULL)
      vec <- as.numeric(sic[1L, , drop = TRUE])
      names(vec) <- colnames(sic)
      return(vec)
    }
    vec <- as.numeric(sic)
    names(vec) <- names(sic)
    return(vec)
  }

  if (!("sol.incl.cov" %in% names(ic_obj))) return(NULL)
  sic <- ic_obj$sol.incl.cov
  if (is.data.frame(sic)) sic <- as.matrix(sic)

  if (is.matrix(sic)) {
    if (nrow(sic) < which_m) return(NULL)
    vec <- as.numeric(sic[which_m, , drop = TRUE])
    names(vec) <- colnames(sic)
    return(vec)
  }

  if (which_m != 1L) return(NULL)
  vec <- as.numeric(sic)
  names(vec) <- names(sic)
  vec
}

.compare_named_fit_vectors <- function(base_vec, cur_vec, tol) {
  if (is.null(base_vec) || is.null(cur_vec)) {
    return(list(same_fit = NA, max_abs_delta = NA_real_, deltas = NULL))
  }
  if (is.null(names(base_vec)) || is.null(names(cur_vec))) {
    return(list(same_fit = NA, max_abs_delta = NA_real_, deltas = NULL))
  }
  common <- intersect(names(base_vec), names(cur_vec))
  if (length(common) == 0L) {
    return(list(same_fit = NA, max_abs_delta = NA_real_, deltas = NULL))
  }
  d <- cur_vec[common] - base_vec[common]
  ok <- !is.na(d)
  if (!any(ok)) {
    return(list(same_fit = NA, max_abs_delta = NA_real_, deltas = d))
  }
  mad <- max(abs(d[ok]))
  list(
    same_fit = isTRUE(mad <= (tol + sqrt(.Machine$double.eps))),
    max_abs_delta = mad,
    deltas = d
  )
  }

.flatten_fit_map <- function(fit_map, solution_type) {
  if (is.null(fit_map) || length(fit_map) == 0L) {
    return(setNames(numeric(0), character(0)))
  }

  out <- numeric(0)

  for (sol_name in names(fit_map)) {
    vec_raw <- fit_map[[sol_name]]

    if (is.null(vec_raw) || length(vec_raw) == 0L) {
      next
    }

    vec <- as.numeric(vec_raw)
    measure_names <- names(vec_raw)

    if (is.null(measure_names) || any(measure_names == "")) {
      measure_names <- paste0("V", seq_along(vec))
    }

    names(vec) <- paste(solution_type, sol_name, measure_names, sep = "::")
    out <- c(out, vec)
  }

  out
}

.common_fit_keys <- function(base_map, cur_map) {
  intersect(names(base_map), names(cur_map))
}

.group_fit_delta_by_solution <- function(delta_vec, solution_type) {
  if (is.null(delta_vec) || length(delta_vec) == 0L) {
    return(list())
  }

  nms <- names(delta_vec)

  if (is.null(nms) || length(nms) == 0L) {
    return(list())
  }

  prefix <- paste0(solution_type, "::")
  keep <- startsWith(nms, prefix)

  if (!any(keep)) {
    return(list())
  }

  vals <- delta_vec[keep]
  labs <- nms[keep]
  rest <- substring(labs, nchar(prefix) + 1L)

  parts <- strsplit(rest, "::", fixed = TRUE)

  sol_names <- vapply(
    parts,
    function(x) if (length(x) >= 1L) x[1L] else "",
    character(1)
  )

  measure_names <- vapply(
    parts,
    function(x) if (length(x) >= 2L) paste(x[-1L], collapse = "::") else "",
    character(1)
  )

  out <- split(vals, sol_names)

  for (sol_name in names(out)) {
    idx <- sol_names == sol_name
    names(out[[sol_name]]) <- measure_names[idx]
  }

  out
}

.fit_details_from_delta <- function(delta_vec, solution_type, tol = 0) {
  grouped <- .group_fit_delta_by_solution(delta_vec, solution_type)

  if (length(grouped) == 0L) {
    return(NULL)
  }

  lapply(
    grouped,
    function(d) {
      mad <- .fit_abs_max(d)

      list(
        same_fit = if (all(is.na(d))) NA else isTRUE(mad <= (tol + sqrt(.Machine$double.eps))),
        max_abs_delta = mad,
        deltas = d
      )
    }
  )
}

.compare_fit_maps_hybrid <- function(base_map, cur_map, solution_type, tol = 0) {
  common_keys <- .common_fit_keys(base_map, cur_map)

  if (length(common_keys) == 0L) {
    return(list(
      fit_compared = FALSE,
      same_fit = NA,
      max_abs_delta = NA_real_,
      matched_keys = character(0),
      details = NULL,
      changed_names = character(0),
      delta = numeric(0)
    ))
  }

  base_flat <- .flatten_fit_map(base_map[common_keys], solution_type)
  cur_flat  <- .flatten_fit_map(cur_map[common_keys], solution_type)

  if (length(base_flat) == 0L || length(cur_flat) == 0L) {
    return(list(
      fit_compared = FALSE,
      same_fit = NA,
      max_abs_delta = NA_real_,
      matched_keys = common_keys,
      details = NULL,
      changed_names = character(0),
      delta = numeric(0)
    ))
  }

  delta_vec <- .fit_delta_vec(base_flat, cur_flat)

  list(
    fit_compared = TRUE,
    same_fit = !.fit_changed(base_flat, cur_flat, tol = tol),
    max_abs_delta = .fit_abs_max(delta_vec),
    matched_keys = common_keys,
    details = .fit_details_from_delta(delta_vec, solution_type = solution_type, tol = tol),
    changed_names = .fit_change_names(base_flat, cur_flat, tol = tol),
    delta = delta_vec
  )
}
