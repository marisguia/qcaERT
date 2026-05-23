.sanitize_terms <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x)]
  x <- trimws(x)
  x <- x[nzchar(x)]
  x <- gsub("\\s+", "", x)
  sort(unique(x))
}

.prime_labels <- function(primes) {
  if (is.null(primes)) return(character(0))

  rn <- rownames(primes)
  if (!is.null(rn) && length(rn) > 0L) {
    return(as.character(rn))
  }

  if (is.matrix(primes) || is.data.frame(primes)) {
    return(as.character(seq_len(nrow(primes))))
  }

  if (is.list(primes) && !is.null(names(primes)) && length(names(primes)) > 0L) {
    return(as.character(names(primes)))
  }

  character(0)
}

.solution_terms_from_primes <- function(sol_item, primes) {
  labels <- .prime_labels(primes)

  rec <- function(x) {
    if (is.null(x)) return(character(0))

    if (is.data.frame(x)) {
      x <- as.matrix(x)
    }

    if (is.matrix(x)) {
      rn <- rownames(x)
      if (!is.null(rn) && nrow(x) > 0L) {
        keep <- rep(TRUE, nrow(x))

        if (is.numeric(x) || is.integer(x)) {
          keep <- rowSums(abs(x), na.rm = TRUE) > 0
        } else if (is.logical(x)) {
          keep <- rowSums(x, na.rm = TRUE) > 0
        }

        return(.sanitize_terms(rn[keep]))
      }

      return(rec(as.list(x)))
    }

    if (is.logical(x)) {
      if (length(labels) > 0L && length(x) == length(labels)) {
        return(.sanitize_terms(labels[which(x)]))
      }
      return(character(0))
    }

    if (is.numeric(x)) {
      xi <- suppressWarnings(as.integer(x))
      if (
        length(labels) > 0L &&
        length(xi) > 0L &&
        all(!is.na(xi)) &&
        all(abs(x - xi) < sqrt(.Machine$double.eps)) &&
        all(xi >= 1L & xi <= length(labels))
      ) {
        return(.sanitize_terms(labels[xi]))
      }
      return(.sanitize_terms(x))
    }

    if (is.character(x)) {
      xs <- .sanitize_terms(x)
      if (length(labels) > 0L && all(xs %in% labels)) {
        return(xs)
      }
      return(xs)
    }

    if (is.list(x)) {
      return(.sanitize_terms(unlist(lapply(x, rec), use.names = FALSE)))
    }

    .sanitize_terms(x)
  }

  out <- rec(sol_item)
  if (length(out) == 0L) "<EMPTY>" else out
}

.extract_selected_m_standard_sig <- function(res, which_M) {
  sol_all <- if (!is.null(res) && !inherits(res, "error")) res$solution else NULL
  primes <- if (!is.null(res) && !inherits(res, "error")) res$primes else NULL

  nM <- if (!is.null(sol_all)) length(sol_all) else 0L
  hasM <- !is.null(sol_all) && nM >= which_M
  sig <- if (hasM) .solution_terms_from_primes(sol_all[[which_M]], primes) else "<M_MISSING>"

  list(
    sig = sig,
    n_M = as.integer(nM),
    has_M = isTRUE(hasM)
  )
}

.extract_selected_m_intermediate_sig <- function(i.sol, which_M, i_mode = "all") {
  if (is.null(i.sol)) {
    return(list(sig = "<I_SOL_MISSING>", missing_M = TRUE, n_M_min = 0L))
  }

  nm <- .normalize_i_solution_names(i.sol)

  if (i_mode == "C1P1") {
    idx <- which(nm == "C1P1")
    if (length(idx) == 0L) {
      return(list(sig = "<C1P1_MISSING>", missing_M = TRUE, n_M_min = 0L))
    }
    i.sol <- i.sol[idx[1L]]
    nm <- nm[idx[1L]]
  }

  out <- character(0)
  missing_M <- FALSE
  n_M_min <- Inf

  for (k in seq_along(i.sol)) {
    el <- i.sol[[k]]
    sol_all <- if (is.list(el) && "solution" %in% names(el)) el$solution else NULL
    primes <- if (is.list(el) && "primes" %in% names(el)) el$primes else NULL

    nM <- if (!is.null(sol_all)) length(sol_all) else 0L
    n_M_min <- min(n_M_min, nM)

    if (is.null(sol_all) || nM < which_M) {
      missing_M <- TRUE
      sigs <- "<M_MISSING>"
    } else {
      sigs <- .solution_terms_from_primes(sol_all[[which_M]], primes)
      sigs <- paste(sigs, collapse = "+")
    }

    out <- c(out, paste0(nm[k], ":", sigs))
  }

  if (!is.finite(n_M_min)) n_M_min <- 0L

  list(
    sig = sort(unique(out)),
    missing_M = missing_M,
    n_M_min = as.integer(n_M_min)
  )
}

.extract_standard_sig <- function(res, which_M) {
  sol_all <- res$solution
  primes <- res$primes

  nM <- if (!is.null(sol_all)) length(sol_all) else 0L

  if (nM < which_M) {
    return(list(
      sig = "<M_MISSING>",
      selected_solution_missing = TRUE,
      meta = list(nM = nM)
    ))
  }

  sig <- paste(.solution_terms_from_primes(sol_all[[which_M]], primes), collapse = "+")

  list(
    sig = sig,
    selected_solution_missing = FALSE,
    meta = list(nM = nM)
  )
}

.extract_intermediate_sig <- function(res, which_M, i_mode) {
  i.sol <- res$i.sol

  if (is.null(i.sol) || length(i.sol) == 0L) {
    return(list(
      sig = "<I_SOL_MISSING>",
      selected_solution_missing = TRUE,
      meta = list(n_i = 0L)
    ))
  }

  nm <- .normalize_i_solution_names(i.sol)

  if (i_mode == "C1P1") {
    idx <- which(nm == "C1P1")
    if (length(idx) == 0L) {
      return(list(
        sig = "<C1P1_MISSING>",
        selected_solution_missing = TRUE,
        meta = list(n_i = length(i.sol))
      ))
    }
    i.sol <- i.sol[idx[1L]]
    nm <- nm[idx[1L]]
  }

  out <- character(0)
  missing_any <- FALSE

  for (k in seq_along(i.sol)) {
    el <- i.sol[[k]]
    sol_all <- if (is.list(el) && "solution" %in% names(el)) el$solution else NULL
    primes <- if (is.list(el) && "primes" %in% names(el)) el$primes else NULL

    nM <- if (!is.null(sol_all)) length(sol_all) else 0L

    if (nM < which_M) {
      out <- c(out, paste0(nm[k], ":<M_MISSING>"))
      missing_any <- TRUE
    } else {
      out <- c(
        out,
        paste0(
          nm[k], ":",
          paste(.solution_terms_from_primes(sol_all[[which_M]], primes), collapse = "+")
        )
      )
    }
  }

  list(
    sig = sort(unique(out)),
    selected_solution_missing = missing_any,
    meta = list(n_i = length(i.sol))
  )
}

.extract_solution_type_sig <- function(res, solution_type, which_M, i_mode) {
  if (solution_type %in% c("conservative", "parsimonious")) {
    out <- .extract_standard_sig(res, which_M = which_M)
    out$solution_type <- solution_type
    return(out)
  }

  out <- .extract_intermediate_sig(
    res,
    which_M = which_M,
    i_mode = i_mode
  )
  out$solution_type <- solution_type
  out
}

.sig_changed <- function(base, cur) {
  if (is.null(base) && is.null(cur)) return(FALSE)
  if (xor(is.null(base), is.null(cur))) return(TRUE)
  !setequal(base, cur)
}

.change_kind_sig <- function(base, cur) {
  if (!.sig_changed(base, cur)) return(NA_character_)

  base_missing <- !is.null(base) && any(grepl("<.*MISSING>|<NO_SOLUTIONS>", base))
  cur_missing <- !is.null(cur) && any(grepl("<.*MISSING>|<NO_SOLUTIONS>", cur))

  if (!base_missing && cur_missing) return("selected_solution_missing")
  if (base_missing && !cur_missing) return("selected_solution_appeared")
  if (base_missing && cur_missing) return("mixed_change")
  "formula_changed"
}

.change_kind_selected_m_sig <- function(base, cur) {
  if (!.sig_changed(base, cur)) return(NA_character_)

  base_missing <- !is.null(base) && any(grepl("<M_MISSING>", base, fixed = TRUE))
  cur_missing <- !is.null(cur) && any(grepl("<M_MISSING>", cur, fixed = TRUE))

  if (!base_missing && cur_missing) return("selected_M_disappeared")
  if (base_missing && !cur_missing) return("selected_M_appeared")
  if (base_missing && cur_missing) return("mixed_change")
  "formula_changed"
}
