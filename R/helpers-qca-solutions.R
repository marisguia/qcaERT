.qca_solution_objects <- function(conservative, intermediate, parsimonious) {
  out <- list()
  if (!is.null(conservative)) out$conservative <- conservative
  if (!is.null(parsimonious)) out$parsimonious <- parsimonious
  if (!is.null(intermediate)) out$intermediate <- intermediate
  out
}

.qca_requested_solutions <- function(solution, objects) {
  if (solution == "all") {
    return(names(objects))
  }

  if (!solution %in% names(objects)) {
    stop("Requested `solution = \"", solution, "\"` but no `", solution, "` object was supplied.")
  }

  solution
}

.qca_solution_label <- function(x) {
  switch(
    x,
    conservative = "Conservative",
    parsimonious = "Parsimonious",
    intermediate = "Intermediate",
    x
  )
}

.qca_select_model_position <- function(n_models, which_M, context) {
  if (!is.numeric(n_models) || length(n_models) != 1L || is.na(n_models) || n_models < 1L) {
    stop("No models are available for ", context, ".")
  }

  if (which_M > n_models) {
    stop("`which_M = ", which_M, "` exceeds the number of available models for ", context, " (", n_models, ").")
  }

  as.integer(which_M)
}

.qca_select_ic_model <- function(ic, which_M, context) {
  if (!is.null(ic[["individual"]])) {
    m <- .qca_select_model_position(
      n_models = length(ic[["individual"]]),
      which_M = which_M,
      context = context
    )

    return(list(ic = ic[["individual"]][[m]], model = m))
  }

  list(ic = ic, model = NA_integer_)
}

.qca_canonicalize_names <- function(x) {
  out <- trimws(as.character(x))
  gsub("\\s+", "", out)
}

.qca_pims_by_terms <- function(pims, terms) {
  if (is.null(pims) || !is.data.frame(pims)) {
    return(NULL)
  }

  cn <- colnames(pims)
  if (is.null(cn) || length(cn) == 0L) {
    return(NULL)
  }

  canon_cn <- .qca_canonicalize_names(cn)
  canon_terms <- .qca_canonicalize_names(terms)
  idx <- match(canon_terms, canon_cn)

  if (any(is.na(idx))) {
    return(NULL)
  }

  out <- pims[, idx, drop = FALSE]
  colnames(out) <- terms
  out
}

.qca_cases_from_incl_cov <- function(incl_cov, pims, include_cases = TRUE, case_cut = 0.5) {
  if (!isTRUE(include_cases)) {
    return(rep(NA_character_, nrow(incl_cov)))
  }

  if ("cases" %in% colnames(incl_cov)) {
    cases_col <- incl_cov[["cases"]]

    if (is.list(cases_col)) {
      return(vapply(
        cases_col,
        function(x) {
          if (length(x) == 0L) return(NA_character_)
          paste(as.character(x), collapse = ", ")
        },
        character(1)
      ))
    }

    return(as.character(cases_col))
  }

  if (is.null(pims) || !is.data.frame(pims)) {
    return(rep(NA_character_, nrow(incl_cov)))
  }

  rn <- rownames(incl_cov)
  if (is.null(rn) || length(rn) == 0L) {
    return(rep(NA_character_, nrow(incl_cov)))
  }

  keep <- intersect(rn, colnames(pims))
  if (length(keep) == 0L) {
    return(rep(NA_character_, nrow(incl_cov)))
  }

  case_names <- rownames(pims)

  vapply(
    rn,
    function(pi) {
      if (!pi %in% colnames(pims)) return(NA_character_)
      x <- pims[[pi]]
      cases <- case_names[which(x >= case_cut)]
      if (length(cases) == 0L) NA_character_ else paste(cases, collapse = ", ")
    },
    character(1)
  )
}

.qca_selected_solution_missing <- function(
    solution_type,
    target_key,
    signature,
    status,
    error_source,
    error_message,
    meta = NULL
) {
  list(
    solution_type = solution_type,
    target_key = target_key,
    signature = signature,
    terms = character(0),
    solution_membership = NULL,
    term_memberships = NULL,
    status = status,
    error_source = error_source,
    error_message = error_message,
    meta = meta
  )
}

.qca_extract_standard_targets <- function(res, solution_type, which_M) {
  sol_all <- res$solution
  primes <- res$primes
  pims <- res$pims
  nM <- if (!is.null(sol_all)) length(sol_all) else 0L

  if (nM < which_M) {
    sig <- "<M_MISSING>"
    return(list(.qca_selected_solution_missing(
      solution_type = solution_type,
      target_key = paste0(solution_type, ":", sig),
      signature = sig,
      status = "baseline_selected_solution_missing",
      error_source = solution_type,
      error_message = "The selected baseline solution is missing.",
      meta = list(nM = nM)
    )))
  }

  terms <- .solution_terms_from_primes(sol_all[[which_M]], primes)
  sig <- paste(terms, collapse = "+")
  tm <- .qca_pims_by_terms(pims, terms)

  if (is.null(tm)) {
    return(list(list(
      solution_type = solution_type,
      target_key = paste0(solution_type, ":", sig),
      signature = sig,
      terms = terms,
      solution_membership = NULL,
      term_memberships = NULL,
      status = "input_error",
      error_source = "pims",
      error_message = "Could not align term memberships with the selected baseline solution.",
      meta = list(nM = nM)
    )))
  }

  sm <- if (ncol(tm) == 1L) as.numeric(tm[[1L]]) else apply(tm, 1L, max)
  list(list(
    solution_type = solution_type,
    target_key = paste0(solution_type, ":", sig),
    signature = sig,
    terms = terms,
    solution_membership = sm,
    term_memberships = tm,
    status = "ok",
    error_source = NA_character_,
    error_message = NA_character_,
    meta = list(nM = nM)
  ))
}

.qca_extract_intermediate_targets <- function(res, solution_type, which_M, i_mode) {
  i.sol <- res$i.sol
  out <- list()

  if (is.null(i.sol) || length(i.sol) == 0L) {
    return(list(.qca_selected_solution_missing(
      solution_type = solution_type,
      target_key = "<I_SOL_MISSING>",
      signature = "<I_SOL_MISSING>",
      status = "baseline_selected_solution_missing",
      error_source = solution_type,
      error_message = "No intermediate baseline solutions are available.",
      meta = list(n_i = 0L)
    )))
  }

  nm <- .normalize_i_solution_names(i.sol)

  if (i_mode == "C1P1") {
    idx <- which(nm == "C1P1")
    if (length(idx) == 0L) {
      return(list(.qca_selected_solution_missing(
        solution_type = solution_type,
        target_key = "<C1P1_MISSING>",
        signature = "<C1P1_MISSING>",
        status = "baseline_selected_solution_missing",
        error_source = solution_type,
        error_message = "The requested intermediate family `C1P1` is missing.",
        meta = list(n_i = length(i.sol))
      )))
    }

    i.sol <- i.sol[idx[1L]]
    nm <- nm[idx[1L]]
  }

  for (k in seq_along(i.sol)) {
    el <- i.sol[[k]]
    sol_all <- if (is.list(el) && "solution" %in% names(el)) el$solution else NULL
    primes <- if (is.list(el) && "primes" %in% names(el)) el$primes else NULL
    pims <- if (is.list(el) && "pims" %in% names(el)) el$pims else NULL
    nM <- if (!is.null(sol_all)) length(sol_all) else 0L

    if (nM < which_M) {
      out[[length(out) + 1L]] <- .qca_selected_solution_missing(
        solution_type = solution_type,
        target_key = paste0(nm[k], ":<M_MISSING>"),
        signature = paste0(nm[k], ":<M_MISSING>"),
        status = "baseline_selected_solution_missing",
        error_source = solution_type,
        error_message = "The selected intermediate baseline solution is missing.",
        meta = list(n_i = length(i.sol), nM = nM)
      )
      next
    }

    terms <- .solution_terms_from_primes(sol_all[[which_M]], primes)
    sig <- paste0(nm[k], ":", paste(terms, collapse = "+"))
    tm <- .qca_pims_by_terms(pims, terms)

    if (is.null(tm)) {
      out[[length(out) + 1L]] <- list(
        solution_type = solution_type,
        target_key = sig,
        signature = sig,
        terms = terms,
        solution_membership = NULL,
        term_memberships = NULL,
        status = "input_error",
        error_source = "pims",
        error_message = "Could not align intermediate term memberships with the selected baseline solution.",
        meta = list(n_i = length(i.sol), nM = nM)
      )
      next
    }

    sm <- if (ncol(tm) == 1L) as.numeric(tm[[1L]]) else apply(tm, 1L, max)
    out[[length(out) + 1L]] <- list(
      solution_type = solution_type,
      target_key = sig,
      signature = sig,
      terms = terms,
      solution_membership = sm,
      term_memberships = tm,
      status = "ok",
      error_source = NA_character_,
      error_message = NA_character_,
      meta = list(n_i = length(i.sol), nM = nM)
    )
  }

  out
}

.qca_extract_targets_for_solution_type <- function(res, solution_type, which_M, i_mode) {
  if (is.null(res)) {
    return(list(.qca_selected_solution_missing(
      solution_type = solution_type,
      target_key = paste0("<", toupper(substr(solution_type, 1L, 3L)), "_MISSING>"),
      signature = paste0("<", toupper(substr(solution_type, 1L, 3L)), "_MISSING>"),
      status = "baseline_selected_solution_missing",
      error_source = solution_type,
      error_message = "This solution_type is not available.",
      meta = NULL
    )))
  }

  if (solution_type %in% c("conservative", "parsimonious")) {
    return(.qca_extract_standard_targets(
      res = res,
      solution_type = solution_type,
      which_M = which_M
    ))
  }

  .qca_extract_intermediate_targets(
    res = res,
    solution_type = solution_type,
    which_M = which_M,
    i_mode = i_mode
  )
}
