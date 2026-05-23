.normalize_theory_names <- function(names, label) {
  if (is.null(names)) {
    stop("`", label, "` must be named.")
  }

  names <- trimws(as.character(names))
  if (any(is.na(names)) || any(!nzchar(names))) {
    stop("`", label, "` names must be non-empty character strings.")
  }
  if (anyDuplicated(names) > 0L) {
    stop("`", label, "` names must be unique.")
  }

  names
}

.normalize_theories <- function(theories, data, outcome) {
  if (!is.list(theories) || is.data.frame(theories) || length(theories) < 2L) {
    stop("`theories` must be a named list with at least two theory-specific condition sets.")
  }

  theory_names <- .normalize_theory_names(names(theories), "theories")
  names(theories) <- theory_names

  out <- lapply(theory_names, function(theory) {
    conditions <- theories[[theory]]
    if (!is.character(conditions) || length(conditions) < 1L) {
      stop("Each entry in `theories` must be a non-empty character vector of condition names.")
    }

    conditions <- trimws(as.character(conditions))
    if (any(is.na(conditions)) || any(!nzchar(conditions))) {
      stop("Condition names in `theories` must be non-empty character strings.")
    }
    if (anyDuplicated(conditions) > 0L) {
      stop("Theory `", theory, "` contains duplicate condition names.")
    }
    if (outcome %in% conditions) {
      stop("Theory `", theory, "` includes the outcome as a condition; the outcome is handled separately.")
    }

    missing <- setdiff(conditions, colnames(data))
    if (length(missing) > 0L) {
      stop(
        "Theory `", theory, "` contains condition(s) not found in `data`: ",
        paste(missing, collapse = ", ")
      )
    }

    conditions
  })

  names(out) <- theory_names
  out
}

.normalize_theory_dir_exp_one <- function(dir_exp, theory, conditions) {
  if (is.character(dir_exp) && length(dir_exp) > 1L) {
    nms <- names(dir_exp)
    if (!is.null(nms) && all(nzchar(nms))) {
      nms <- trimws(as.character(nms))
      if (!setequal(nms, conditions)) {
        stop(
          "`dir.exp[[\"", theory, "\"]]` names must match that theory's conditions."
        )
      }
      dir_exp <- unname(dir_exp[match(conditions, nms)])
    }
  }

  .normalize_dir_exp_generic(
    dir_exp,
    conditions = conditions,
    endpoint_phrase = paste0("the conditions for theory `", theory, "`")
  )
}

.normalize_theory_dir_exp <- function(dir.exp, theories, solution) {
  if (is.null(dir.exp)) {
    if (identical(solution, "intermediate")) {
      stop("When `solution = \"intermediate\"`/`\"int\"`, `dir.exp` must be provided as a named list with one entry per theory.")
    }
    return(NULL)
  }

  if (!is.list(dir.exp) || is.data.frame(dir.exp)) {
    stop("For `theory.test()`, `dir.exp` must be NULL or a named list with one entry per theory.")
  }

  dir_names <- .normalize_theory_names(names(dir.exp), "dir.exp")
  theory_names <- names(theories)
  missing <- setdiff(theory_names, dir_names)
  extra <- setdiff(dir_names, theory_names)
  if (length(missing) > 0L || length(extra) > 0L) {
    msg <- "`dir.exp` names must match `theories` names."
    if (length(missing) > 0L) {
      msg <- paste0(msg, " Missing: ", paste(missing, collapse = ", "), ".")
    }
    if (length(extra) > 0L) {
      msg <- paste0(msg, " Unknown: ", paste(extra, collapse = ", "), ".")
    }
    stop(msg)
  }

  out <- lapply(theory_names, function(theory) {
    .normalize_theory_dir_exp_one(dir.exp[[theory]], theory, theories[[theory]])
  })
  names(out) <- theory_names
  out
}

.validate_theory_static_exclude <- function(exclude_mode, exclude_static, theories, monitored_solutions) {
  if (!identical(exclude_mode, "static")) {
    return(invisible(TRUE))
  }

  if (!any(monitored_solutions %in% c("parsimonious", "intermediate"))) {
    return(invisible(TRUE))
  }

  if (is.null(exclude_static) || !is.list(exclude_static) || is.data.frame(exclude_static)) {
    stop("For `theory.test()`, `exclude_mode = \"static\"` requires `exclude_static` as a named list with one entry per theory.")
  }

  exclude_names <- .normalize_theory_names(names(exclude_static), "exclude_static")
  theory_names <- names(theories)
  missing <- setdiff(theory_names, exclude_names)
  extra <- setdiff(exclude_names, theory_names)
  if (length(missing) > 0L || length(extra) > 0L) {
    msg <- "`exclude_static` names must match `theories` names when `exclude_mode = \"static\"`."
    if (length(missing) > 0L) {
      msg <- paste0(msg, " Missing: ", paste(missing, collapse = ", "), ".")
    }
    if (length(extra) > 0L) {
      msg <- paste0(msg, " Unknown: ", paste(extra, collapse = ", "), ".")
    }
    stop(msg)
  }

  invisible(TRUE)
}

.empty_theory_diagnostics <- function() {
  data.frame(
    theory = character(0),
    solution_type = character(0),
    status = character(0),
    n_conditions = integer(0),
    conditions = character(0),
    n_tt_rows = integer(0),
    n_observed_rows = integer(0),
    n_remainders = integer(0),
    n_excluded = integer(0),
    selected_solution_missing = logical(0),
    error_source = character(0),
    error_message = character(0),
    stringsAsFactors = FALSE
  )
}

.empty_theory_models <- function() {
  data.frame(
    theory = character(0),
    solution_type = character(0),
    intermediate_branch = character(0),
    status = character(0),
    n_conditions = integer(0),
    n_tt_rows = integer(0),
    n_observed_rows = integer(0),
    n_remainders = integer(0),
    n_excluded = integer(0),
    n_models = integer(0),
    selected_model = integer(0),
    n_terms = integer(0),
    inclS = numeric(0),
    PRI = numeric(0),
    covS = numeric(0),
    stringsAsFactors = FALSE
  )
}

.empty_theory_pairwise <- function() {
  data.frame(
    solution_type = character(0),
    intermediate_branch = character(0),
    theory_1 = character(0),
    theory_2 = character(0),
    delta_inclS = numeric(0),
    delta_PRI = numeric(0),
    delta_covS = numeric(0),
    membership_jaccard = numeric(0),
    mean_abs_membership_delta = numeric(0),
    stringsAsFactors = FALSE
  )
}

.empty_theory_solutions <- function() {
  data.frame(
    theory = character(0),
    solution_type = character(0),
    model = integer(0),
    intermediate_branch = character(0),
    prime_implicant = character(0),
    inclS = numeric(0),
    PRI = numeric(0),
    covS = numeric(0),
    stringsAsFactors = FALSE
  )
}

.empty_theory_results <- function() {
  list(
    models = .empty_theory_models(),
    pairwise = .empty_theory_pairwise(),
    solutions = .empty_theory_solutions()
  )
}

.theory_tt_stats <- function(tt_obj) {
  empty <- list(
    n_tt_rows = NA_integer_,
    n_observed_rows = NA_integer_,
    n_remainders = NA_integer_
  )

  if (is.null(tt_obj) || is.null(tt_obj$tt) || !is.data.frame(tt_obj$tt)) {
    return(empty)
  }

  n_values <- if ("n" %in% names(tt_obj$tt)) {
    suppressWarnings(as.numeric(tt_obj$tt$n))
  } else {
    rep(NA_real_, nrow(tt_obj$tt))
  }

  list(
    n_tt_rows = as.integer(nrow(tt_obj$tt)),
    n_observed_rows = as.integer(sum(!is.na(n_values) & n_values > 0)),
    n_remainders = as.integer(sum(!is.na(n_values) & n_values == 0))
  )
}

.theory_n_excluded <- function(exclude_used) {
  if (is.null(exclude_used)) {
    return(0L)
  }
  as.integer(length(exclude_used))
}

.theory_payload_status <- function(payload, monitored_solutions) {
  solution_type_status <- payload$solution_type_status[monitored_solutions]
  if (all(!is.na(solution_type_status) & solution_type_status == "ok")) {
    return(list(
      status = "ok",
      error_source = NA_character_,
      error_message = NA_character_
    ))
  }

  if (any(solution_type_status == "ok", na.rm = TRUE)) {
    first_bad <- which(!is.na(solution_type_status) & solution_type_status != "ok")[1L]
    solution_type <- monitored_solutions[first_bad]
    return(list(
      status = "partial",
      error_source = unname(payload$solution_type_error_source[[solution_type]]),
      error_message = unname(payload$solution_type_error_message[[solution_type]])
    ))
  }

  first_bad <- which(!is.na(solution_type_status) & nzchar(solution_type_status))[1L]
  if (is.na(first_bad)) {
    return(list(
      status = "run_error",
      error_source = NA_character_,
      error_message = NA_character_
    ))
  }

  solution_type <- monitored_solutions[first_bad]
  list(
    status = unname(payload$solution_type_status[[solution_type]]),
    error_source = unname(payload$solution_type_error_source[[solution_type]]),
    error_message = unname(payload$solution_type_error_message[[solution_type]])
  )
}

.theory_make_run_result <- function(
    theory,
    conditions,
    dir.exp,
    status,
    tt = NULL,
    exclude_used = NULL,
    payload = NULL,
    error_source = NA_character_,
    error_message = NA_character_
) {
  if (is.null(payload)) {
    payload <- .reduced_blank_payload()
  }

  c(
    list(
      theory = theory,
      conditions = conditions,
      dir.exp = dir.exp,
      status = status,
      tt = tt,
      exclude_used = exclude_used
    ),
    payload,
    list(
      error_source = error_source,
      error_message = error_message
    )
  )
}

.theory_build_truth_table <- function(data, outcome, conditions, incl.cut, n.cut, dots_tt) {
  args <- c(
    list(
      data = data,
      outcome = outcome,
      conditions = conditions,
      incl.cut = incl.cut,
      n.cut = n.cut
    ),
    dots_tt
  )

  tryCatch(
    suppressWarnings(do.call(QCA::truthTable, args)),
    error = function(e) e
  )
}

.theory_run_one <- function(
    theory,
    data,
    outcome,
    conditions,
    dir.exp,
    incl.cut,
    n.cut,
    monitored_solutions,
    exclude_mode,
    exclude_recompute,
    exclude_static,
    dots_tt,
    dots_min,
    which_M,
    i_mode
) {
  tt_obj <- .theory_build_truth_table(
    data = data,
    outcome = outcome,
    conditions = conditions,
    incl.cut = incl.cut,
    n.cut = n.cut,
    dots_tt = dots_tt
  )

  if (inherits(tt_obj, "error")) {
    payload <- .reduced_blank_payload()
    payload$solution_type_status[monitored_solutions] <- "truth_table_error"
    payload$solution_type_error_source[monitored_solutions] <- "truthTable"
    payload$solution_type_error_message[monitored_solutions] <- .reduced_error_message(tt_obj)

    return(.theory_make_run_result(
      theory = theory,
      conditions = conditions,
      dir.exp = dir.exp,
      status = "truth_table_error",
      tt = NULL,
      exclude_used = NULL,
      payload = payload,
      error_source = "truthTable",
      error_message = .reduced_error_message(tt_obj)
    ))
  }

  need_exclude <- any(monitored_solutions %in% c("parsimonious", "intermediate")) &&
    exclude_mode != "none"

  exclude_i <- if (identical(exclude_mode, "static") && !is.null(exclude_static)) {
    exclude_static[[theory]]
  } else {
    exclude_static
  }

  ex <- NULL
  ex_error <- NULL
  if (need_exclude) {
    ex <- .reduced_compute_exclude(
      tt_obj = tt_obj,
      exclude_mode = exclude_mode,
      exclude_recompute = exclude_recompute,
      exclude_static = exclude_i
    )

    if (inherits(ex, "error")) {
      ex_error <- ex
      ex <- NULL
    }
  }

  payload <- .reduced_blank_payload()

  for (solution_type in monitored_solutions) {
    if (solution_type %in% c("parsimonious", "intermediate") && inherits(ex_error, "error")) {
      payload$solution_type_status[[solution_type]] <- "exclude_error"
      payload$solution_type_error_source[[solution_type]] <- "exclude"
      payload$solution_type_error_message[[solution_type]] <- .reduced_error_message(ex_error)
      next
    }

    rr <- .reduced_run_solution_type(
      tt_obj = tt_obj,
      solution_type = solution_type,
      ex = ex,
      dots_min = dots_min,
      dir.exp = dir.exp,
      which_M = which_M,
      i_mode = i_mode,
      fit_measures = character(0)
    )

    if (!is.null(rr$error)) {
      payload$solution_type_status[[solution_type]] <- "minimize_error"
      payload$solution_type_error_source[[solution_type]] <- solution_type
      payload$solution_type_error_message[[solution_type]] <- .reduced_error_message(rr$error)
      next
    }

    payload$res[[solution_type]] <- rr$res
    payload$sig[[solution_type]] <- rr$sig
    payload$selected_solution_missing[[solution_type]] <- isTRUE(rr$selected_solution_missing)
    payload$fit[[solution_type]] <- rr$fit
    payload$fit_missing[[solution_type]] <- isTRUE(rr$fit_missing)
    payload$meta[[solution_type]] <- rr$meta
    payload$solution_type_status[[solution_type]] <- "ok"
    payload$solution_type_error_source[[solution_type]] <- NA_character_
    payload$solution_type_error_message[[solution_type]] <- NA_character_
  }

  run_status <- .theory_payload_status(payload, monitored_solutions)

  .theory_make_run_result(
    theory = theory,
    conditions = conditions,
    dir.exp = dir.exp,
    status = run_status$status,
    tt = tt_obj,
    exclude_used = ex,
    payload = payload,
    error_source = run_status$error_source,
    error_message = run_status$error_message
  )
}

.theory_diagnostics_from_runs <- function(by_theory, monitored_solutions) {
  rows <- list()
  for (theory in names(by_theory)) {
    run <- by_theory[[theory]]
    stats <- .theory_tt_stats(run$tt)
    n_excluded <- .theory_n_excluded(run$exclude_used)

    for (solution_type in monitored_solutions) {
      status <- run$solution_type_status[[solution_type]]
      if (is.null(status) || is.na(status)) {
        status <- run$status
      }

      rows[[length(rows) + 1L]] <- data.frame(
        theory = theory,
        solution_type = solution_type,
        status = status,
        n_conditions = length(run$conditions),
        conditions = paste(run$conditions, collapse = ","),
        n_tt_rows = stats$n_tt_rows,
        n_observed_rows = stats$n_observed_rows,
        n_remainders = stats$n_remainders,
        n_excluded = if (solution_type %in% c("parsimonious", "intermediate")) n_excluded else 0L,
        selected_solution_missing = run$selected_solution_missing[[solution_type]] %in% TRUE,
        error_source = run$solution_type_error_source[[solution_type]],
        error_message = run$solution_type_error_message[[solution_type]],
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(rows) == 0L) {
    return(.empty_theory_diagnostics())
  }

  .bind_rows_result(rows)
}

.theory_fit_value <- function(fit_table, row, column) {
  if (is.null(fit_table) || !is.data.frame(fit_table) || nrow(fit_table) < row ||
      !column %in% colnames(fit_table)) {
    return(NA_real_)
  }

  suppressWarnings(as.numeric(fit_table[[column]][row]))
}

.theory_selected_ic <- function(ic, which_M, has_model) {
  if (!isTRUE(has_model) || is.null(ic)) {
    return(NULL)
  }

  if (!is.null(ic[["individual"]])) {
    if (length(ic[["individual"]]) < which_M) {
      return(NULL)
    }
    return(ic[["individual"]][[which_M]])
  }

  ic
}

.theory_selected_fit_row <- function(ic, which_M, has_model) {
  if (!isTRUE(has_model) || is.null(ic)) {
    return(NA_integer_)
  }

  if (!is.null(ic[["individual"]])) {
    return(1L)
  }

  as.integer(which_M)
}

.theory_selected_terms <- function(sol_all, primes, which_M) {
  n_models <- if (!is.null(sol_all)) length(sol_all) else 0L
  if (n_models < which_M) {
    return(NULL)
  }

  terms <- .solution_terms_from_primes(sol_all[[which_M]], primes)
  if (identical(terms, "<EMPTY>")) {
    return(character(0))
  }
  terms
}

.theory_standard_model_info <- function(res, solution_type, which_M) {
  sol_all <- if (!is.null(res) && !inherits(res, "error")) res$solution else NULL
  primes <- if (!is.null(res) && !inherits(res, "error")) res$primes else NULL
  ic <- if (!is.null(res) && !inherits(res, "error")) res$IC else NULL

  n_models <- if (!is.null(sol_all)) length(sol_all) else 0L
  has_model <- n_models >= which_M
  terms <- .theory_selected_terms(sol_all, primes, which_M)
  selected_ic <- .theory_selected_ic(
    ic = ic,
    which_M = which_M,
    has_model = has_model
  )
  fit_row <- .theory_selected_fit_row(ic, which_M = which_M, has_model = has_model)
  fit_table <- if (!is.null(selected_ic)) selected_ic$sol.incl.cov else NULL

  data.frame(
    solution_type = solution_type,
    intermediate_branch = NA_character_,
    n_models = as.integer(n_models),
    selected_model = if (has_model) as.integer(which_M) else NA_integer_,
    n_terms = if (has_model) as.integer(length(terms)) else NA_integer_,
    inclS = if (has_model) .theory_fit_value(fit_table, fit_row, "inclS") else NA_real_,
    PRI = if (has_model) .theory_fit_value(fit_table, fit_row, "PRI") else NA_real_,
    covS = if (has_model) .theory_fit_value(fit_table, fit_row, "covS") else NA_real_,
    stringsAsFactors = FALSE
  )
}

.theory_intermediate_model_info <- function(res, which_M, i_mode) {
  i.sol <- if (!is.null(res) && !inherits(res, "error")) res$i.sol else NULL

  if (is.null(i.sol) || length(i.sol) == 0L) {
    return(data.frame(
      solution_type = "intermediate",
      intermediate_branch = NA_character_,
      n_models = 0L,
      selected_model = NA_integer_,
      n_terms = NA_integer_,
      inclS = NA_real_,
      PRI = NA_real_,
      covS = NA_real_,
      stringsAsFactors = FALSE
    ))
  }

  branch_names <- .normalize_i_solution_names(i.sol)
  if (i_mode == "C1P1") {
    idx <- which(branch_names == "C1P1")
    if (length(idx) == 0L) {
      return(data.frame(
        solution_type = "intermediate",
        intermediate_branch = "C1P1",
        n_models = 0L,
        selected_model = NA_integer_,
        n_terms = NA_integer_,
        inclS = NA_real_,
        PRI = NA_real_,
        covS = NA_real_,
        stringsAsFactors = FALSE
      ))
    }

    i.sol <- i.sol[idx[1L]]
    branch_names <- branch_names[idx[1L]]
  }

  rows <- lapply(seq_along(i.sol), function(i) {
    branch <- i.sol[[i]]
    sol_all <- if (is.list(branch) && "solution" %in% names(branch)) branch$solution else NULL
    primes <- if (is.list(branch) && "primes" %in% names(branch)) branch$primes else NULL

    n_models <- if (!is.null(sol_all)) length(sol_all) else 0L
    has_model <- n_models >= which_M
    terms <- .theory_selected_terms(sol_all, primes, which_M)
    ic <- if (is.list(branch) && "IC" %in% names(branch)) branch$IC else NULL
    selected_ic <- .theory_selected_ic(
      ic = ic,
      which_M = which_M,
      has_model = has_model
    )
    fit_row <- .theory_selected_fit_row(ic, which_M = which_M, has_model = has_model)
    fit_table <- if (!is.null(selected_ic)) selected_ic$sol.incl.cov else NULL

    data.frame(
      solution_type = "intermediate",
      intermediate_branch = branch_names[i],
      n_models = as.integer(n_models),
      selected_model = if (has_model) as.integer(which_M) else NA_integer_,
      n_terms = if (has_model) as.integer(length(terms)) else NA_integer_,
      inclS = if (has_model) .theory_fit_value(fit_table, fit_row, "inclS") else NA_real_,
      PRI = if (has_model) .theory_fit_value(fit_table, fit_row, "PRI") else NA_real_,
      covS = if (has_model) .theory_fit_value(fit_table, fit_row, "covS") else NA_real_,
      stringsAsFactors = FALSE
    )
  })

  .bind_rows_result(rows)
}

.theory_term_fit_value <- function(incl_cov, term, column) {
  if (is.null(incl_cov) || !is.data.frame(incl_cov) ||
      !column %in% colnames(incl_cov) || nrow(incl_cov) == 0L) {
    return(NA_real_)
  }

  rn <- rownames(incl_cov)
  if (is.null(rn) || length(rn) == 0L) {
    return(NA_real_)
  }

  idx <- match(.qca_canonicalize_names(term), .qca_canonicalize_names(rn))
  if (is.na(idx)) {
    return(NA_real_)
  }

  suppressWarnings(as.numeric(incl_cov[[column]][idx]))
}

.theory_solution_rows <- function(theory, solution_type, model, intermediate_branch, terms, incl_cov) {
  if (length(terms) == 0L) {
    return(.empty_theory_solutions())
  }

  data.frame(
    theory = rep(theory, length(terms)),
    solution_type = rep(solution_type, length(terms)),
    model = rep(as.integer(model), length(terms)),
    intermediate_branch = rep(intermediate_branch, length(terms)),
    prime_implicant = as.character(terms),
    inclS = vapply(terms, .theory_term_fit_value, numeric(1), incl_cov = incl_cov, column = "inclS"),
    PRI = vapply(terms, .theory_term_fit_value, numeric(1), incl_cov = incl_cov, column = "PRI"),
    covS = vapply(terms, .theory_term_fit_value, numeric(1), incl_cov = incl_cov, column = "covS"),
    stringsAsFactors = FALSE
  )
}

.theory_standard_solution_rows <- function(theory, res, solution_type, which_M) {
  if (is.null(res) || inherits(res, "error")) {
    return(.empty_theory_solutions())
  }

  sol_all <- res$solution
  primes <- res$primes
  n_models <- if (!is.null(sol_all)) length(sol_all) else 0L
  has_model <- n_models >= which_M
  terms <- .theory_selected_terms(sol_all, primes, which_M)
  selected_ic <- .theory_selected_ic(
    ic = res$IC,
    which_M = which_M,
    has_model = has_model
  )
  incl_cov <- if (!is.null(selected_ic)) selected_ic$incl.cov else NULL

  if (!isTRUE(has_model) || is.null(terms)) {
    return(.empty_theory_solutions())
  }

  .theory_solution_rows(
    theory = theory,
    solution_type = solution_type,
    model = which_M,
    intermediate_branch = NA_character_,
    terms = terms,
    incl_cov = incl_cov
  )
}

.theory_intermediate_solution_rows <- function(theory, res, which_M, i_mode) {
  i.sol <- if (!is.null(res) && !inherits(res, "error")) res$i.sol else NULL
  if (is.null(i.sol) || length(i.sol) == 0L) {
    return(.empty_theory_solutions())
  }

  branch_names <- .normalize_i_solution_names(i.sol)
  if (i_mode == "C1P1") {
    idx <- which(branch_names == "C1P1")
    if (length(idx) == 0L) {
      return(.empty_theory_solutions())
    }
    i.sol <- i.sol[idx[1L]]
    branch_names <- branch_names[idx[1L]]
  }

  rows <- list()
  for (i in seq_along(i.sol)) {
    branch <- i.sol[[i]]
    sol_all <- if (is.list(branch) && "solution" %in% names(branch)) branch$solution else NULL
    primes <- if (is.list(branch) && "primes" %in% names(branch)) branch$primes else NULL
    n_models <- if (!is.null(sol_all)) length(sol_all) else 0L
    has_model <- n_models >= which_M
    terms <- .theory_selected_terms(sol_all, primes, which_M)
    selected_ic <- .theory_selected_ic(
      ic = if (is.list(branch) && "IC" %in% names(branch)) branch$IC else NULL,
      which_M = which_M,
      has_model = has_model
    )
    incl_cov <- if (!is.null(selected_ic)) selected_ic$incl.cov else NULL

    if (!isTRUE(has_model) || is.null(terms)) {
      next
    }

    rows[[length(rows) + 1L]] <- .theory_solution_rows(
      theory = theory,
      solution_type = "intermediate",
      model = which_M,
      intermediate_branch = branch_names[i],
      terms = terms,
      incl_cov = incl_cov
    )
  }

  if (length(rows) == 0L) {
    return(.empty_theory_solutions())
  }

  .bind_rows_result(rows)
}

.theory_solution_type_solution_rows <- function(theory, run, solution_type, which_M, i_mode) {
  if (!identical(run$solution_type_status[[solution_type]], "ok")) {
    return(.empty_theory_solutions())
  }

  if (solution_type %in% c("conservative", "parsimonious")) {
    return(.theory_standard_solution_rows(
      theory = theory,
      res = run$res[[solution_type]],
      solution_type = solution_type,
      which_M = which_M
    ))
  }

  .theory_intermediate_solution_rows(
    theory = theory,
    res = run$res[[solution_type]],
    which_M = which_M,
    i_mode = i_mode
  )
}

.theory_solutions_from_runs <- function(by_theory, monitored_solutions, which_M, i_mode) {
  rows <- list()

  for (theory in names(by_theory)) {
    run <- by_theory[[theory]]

    for (solution_type in monitored_solutions) {
      current <- .theory_solution_type_solution_rows(
        theory = theory,
        run = run,
        solution_type = solution_type,
        which_M = which_M,
        i_mode = i_mode
      )

      if (nrow(current) > 0L) {
        rows[[length(rows) + 1L]] <- current
      }
    }
  }

  if (length(rows) == 0L) {
    return(.empty_theory_solutions())
  }

  .bind_rows_result(rows)
}

.theory_target_branch <- function(target, solution_type) {
  if (!identical(solution_type, "intermediate")) {
    return(NA_character_)
  }

  sig <- as.character(target$signature)
  if (length(sig) != 1L || is.na(sig) || !grepl(":", sig, fixed = TRUE)) {
    return(NA_character_)
  }

  sub(":.*$", "", sig)
}

.theory_target_membership <- function(target) {
  membership <- target$solution_membership
  if (is.null(membership)) {
    return(NULL)
  }

  out <- suppressWarnings(as.numeric(membership))
  if (length(out) == 0L) {
    return(NULL)
  }

  term_memberships <- target$term_memberships
  rn <- if (!is.null(term_memberships) && is.data.frame(term_memberships)) {
    rownames(term_memberships)
  } else {
    NULL
  }

  if (!is.null(rn) && length(rn) == length(out)) {
    names(out) <- rn
  }

  out
}

.theory_targets_from_runs <- function(by_theory, monitored_solutions, which_M, i_mode) {
  rows <- list()

  for (theory in names(by_theory)) {
    run <- by_theory[[theory]]

    for (solution_type in monitored_solutions) {
      if (!identical(run$solution_type_status[[solution_type]], "ok")) {
        next
      }

      targets <- .qca_extract_targets_for_solution_type(
        res = run$res[[solution_type]],
        solution_type = solution_type,
        which_M = which_M,
        i_mode = i_mode
      )

      for (target in targets) {
        membership <- .theory_target_membership(target)
        if (!identical(target$status, "ok") || is.null(membership)) {
          next
        }

        rows[[length(rows) + 1L]] <- list(
          theory = theory,
          solution_type = solution_type,
          intermediate_branch = .theory_target_branch(target, solution_type),
          membership = membership
        )
      }
    }
  }

  rows
}

.theory_same_branch <- function(x, y) {
  if (is.na(x) && is.na(y)) {
    return(TRUE)
  }
  identical(as.character(x), as.character(y))
}

.theory_find_target <- function(targets, theory, solution_type, intermediate_branch) {
  for (target in targets) {
    if (identical(target$theory, theory) &&
        identical(target$solution_type, solution_type) &&
        .theory_same_branch(target$intermediate_branch, intermediate_branch)) {
      return(target)
    }
  }

  NULL
}

.theory_align_memberships <- function(a, b) {
  a <- suppressWarnings(as.numeric(a))
  b <- suppressWarnings(as.numeric(b))
  names_a <- names(a)
  names_b <- names(b)

  if (!is.null(names_a) && !is.null(names_b) &&
      length(names_a) == length(a) && length(names_b) == length(b) &&
      all(nzchar(names_a)) && all(nzchar(names_b))) {
    common <- intersect(names_a, names_b)
    if (length(common) == 0L) {
      return(NULL)
    }

    out <- list(
      a = unname(a[match(common, names_a)]),
      b = unname(b[match(common, names_b)])
    )
  } else {
    if (length(a) != length(b)) {
      return(NULL)
    }

    out <- list(a = unname(a), b = unname(b))
  }

  keep <- !is.na(out$a) & !is.na(out$b)
  if (!any(keep)) {
    return(NULL)
  }

  list(a = out$a[keep], b = out$b[keep])
}

.theory_fuzzy_jaccard <- function(a, b) {
  aligned <- .theory_align_memberships(a, b)
  if (is.null(aligned)) {
    return(NA_real_)
  }

  denom <- sum(pmax(aligned$a, aligned$b), na.rm = TRUE)
  if (denom == 0) {
    return(1)
  }

  sum(pmin(aligned$a, aligned$b), na.rm = TRUE) / denom
}

.theory_mean_abs_membership_delta <- function(a, b) {
  aligned <- .theory_align_memberships(a, b)
  if (is.null(aligned)) {
    return(NA_real_)
  }

  mean(abs(aligned$b - aligned$a))
}

.theory_model_fit_row <- function(models, theory, solution_type, intermediate_branch) {
  keep <- models$theory == theory & models$solution_type == solution_type

  if ("intermediate_branch" %in% names(models)) {
    if (is.na(intermediate_branch)) {
      keep <- keep & is.na(models$intermediate_branch)
    } else {
      keep <- keep & !is.na(models$intermediate_branch) &
        models$intermediate_branch == intermediate_branch
    }
  }

  idx <- which(keep)
  if (length(idx) == 0L) {
    return(NULL)
  }

  models[idx[1L], , drop = FALSE]
}

.theory_fit_delta <- function(models, theory_1, theory_2, solution_type, intermediate_branch, column) {
  row_1 <- .theory_model_fit_row(models, theory_1, solution_type, intermediate_branch)
  row_2 <- .theory_model_fit_row(models, theory_2, solution_type, intermediate_branch)

  if (is.null(row_1) || is.null(row_2) || !column %in% names(row_1) || !column %in% names(row_2)) {
    return(NA_real_)
  }

  suppressWarnings(as.numeric(row_2[[column]]) - as.numeric(row_1[[column]]))
}

.theory_pairwise_branches <- function(targets, solution_type) {
  branches <- vapply(
    targets,
    function(x) {
      if (!identical(x$solution_type, solution_type)) {
        return(NA_character_)
      }
      x$intermediate_branch
    },
    character(1)
  )

  branches <- unique(branches)
  if (!identical(solution_type, "intermediate")) {
    return(NA_character_)
  }

  branches[!is.na(branches)]
}

.theory_pairwise_from_runs <- function(by_theory, models, monitored_solutions, which_M, i_mode) {
  targets <- .theory_targets_from_runs(
    by_theory = by_theory,
    monitored_solutions = monitored_solutions,
    which_M = which_M,
    i_mode = i_mode
  )

  theory_names <- names(by_theory)
  if (length(targets) == 0L || length(theory_names) < 2L) {
    return(.empty_theory_pairwise())
  }

  pairs <- utils::combn(theory_names, 2L, simplify = FALSE)
  rows <- list()

  for (solution_type in monitored_solutions) {
    branches <- .theory_pairwise_branches(targets, solution_type)
    if (length(branches) == 0L) {
      next
    }

    for (branch in branches) {
      for (pair in pairs) {
        theory_1 <- pair[1L]
        theory_2 <- pair[2L]
        target_1 <- .theory_find_target(targets, theory_1, solution_type, branch)
        target_2 <- .theory_find_target(targets, theory_2, solution_type, branch)

        if (is.null(target_1) || is.null(target_2)) {
          next
        }

        rows[[length(rows) + 1L]] <- data.frame(
          solution_type = solution_type,
          intermediate_branch = branch,
          theory_1 = theory_1,
          theory_2 = theory_2,
          delta_inclS = .theory_fit_delta(models, theory_1, theory_2, solution_type, branch, "inclS"),
          delta_PRI = .theory_fit_delta(models, theory_1, theory_2, solution_type, branch, "PRI"),
          delta_covS = .theory_fit_delta(models, theory_1, theory_2, solution_type, branch, "covS"),
          membership_jaccard = .theory_fuzzy_jaccard(target_1$membership, target_2$membership),
          mean_abs_membership_delta = .theory_mean_abs_membership_delta(target_1$membership, target_2$membership),
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(rows) == 0L) {
    return(.empty_theory_pairwise())
  }

  .bind_rows_result(rows)
}

.theory_solution_type_model_info <- function(run, solution_type, which_M, i_mode) {
  if (solution_type %in% c("conservative", "parsimonious")) {
    return(.theory_standard_model_info(
      res = run$res[[solution_type]],
      solution_type = solution_type,
      which_M = which_M
    ))
  }

  .theory_intermediate_model_info(
    res = run$res[[solution_type]],
    which_M = which_M,
    i_mode = i_mode
  )
}

.theory_model_status <- function(run, solution_type, selected_model) {
  status <- run$solution_type_status[[solution_type]]
  if (is.null(status) || is.na(status)) {
    status <- run$status
  }

  if (identical(status, "ok") && is.na(selected_model)) {
    return("selected_solution_missing")
  }

  status
}

.theory_models_from_runs <- function(by_theory, monitored_solutions, which_M, i_mode) {
  rows <- list()

  for (theory in names(by_theory)) {
    run <- by_theory[[theory]]
    stats <- .theory_tt_stats(run$tt)
    n_excluded <- .theory_n_excluded(run$exclude_used)

    for (solution_type in monitored_solutions) {
      model_info <- .theory_solution_type_model_info(
        run = run,
        solution_type = solution_type,
        which_M = which_M,
        i_mode = i_mode
      )

      for (i in seq_len(nrow(model_info))) {
        rows[[length(rows) + 1L]] <- data.frame(
          theory = theory,
          solution_type = model_info$solution_type[i],
          intermediate_branch = model_info$intermediate_branch[i],
          status = .theory_model_status(run, solution_type, model_info$selected_model[i]),
          n_conditions = length(run$conditions),
          n_tt_rows = stats$n_tt_rows,
          n_observed_rows = stats$n_observed_rows,
          n_remainders = stats$n_remainders,
          n_excluded = if (solution_type %in% c("parsimonious", "intermediate")) n_excluded else 0L,
          n_models = model_info$n_models[i],
          selected_model = model_info$selected_model[i],
          n_terms = model_info$n_terms[i],
          inclS = model_info$inclS[i],
          PRI = model_info$PRI[i],
          covS = model_info$covS[i],
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(rows) == 0L) {
    return(.empty_theory_models())
  }

  .bind_rows_result(rows)
}

.theory_deparse_one <- function(x) {
  if (is.null(x)) {
    return("NULL")
  }
  paste(utils::capture.output(dput(x)), collapse = "")
}

.theory_n_rows <- function(x) {
  if (!is.data.frame(x)) {
    return(0L)
  }
  as.integer(nrow(x))
}

.theory_print_solutions <- function(solutions, row.names = FALSE, ...) {
  display <- solutions
  if ("model" %in% names(display) && all(display$model == 1L, na.rm = TRUE)) {
    display$model <- NULL
  }
  if ("intermediate_branch" %in% names(display) && all(is.na(display$intermediate_branch))) {
    display$intermediate_branch <- NULL
  }

  fit_cols <- intersect(c("inclS", "PRI", "covS"), names(display))
  for (col in fit_cols) {
    display[[col]] <- round(display[[col]], 3L)
  }

  print.data.frame(display, row.names = row.names, ...)
}

#' Theory-specification robustness for QCA models
#'
#' Compare several theoretically oriented QCA condition sets while holding the
#' outcome, truth-table cutoffs, solution type, exclusion handling, and
#' model-selection settings constant. Each theory supplies its own condition
#' set.
#'
#' The function builds a separate truth table for each theory, recomputes
#' exclusions separately when requested, and runs the monitored solution types
#' under the same analytic settings. The raw per-theory QCA objects are stored
#' in `by_theory`. `results$models` gives the clean model-level comparison
#' table, and `results$solutions` gives the selected prime implicants or
#' solution terms by theory and solution type. `results$pairwise` compares selected
#' solution memberships across theories using fuzzy Jaccard similarity and
#' mean absolute membership differences.
#'
#' @param data A non-empty data frame containing the calibrated outcome and
#'   calibrated condition columns.
#' @param outcome Name of the calibrated outcome column in `data`.
#' @param theories Named list of theory-specific condition sets. Each entry
#'   must be a non-empty character vector of condition names in `data`.
#'   Conditions may be shared across theories, but no theory may contain the
#'   outcome as a condition.
#' @param incl.cut Inclusion cutoff to be used for every theory-specific truth
#'   table.
#' @param n.cut Frequency cutoff to be used for every theory-specific truth
#'   table.
#' @param solution Solution type to compare. Accepted values are `"all"`,
#'   `"con"` or `"conservative"`, `"par"` or `"parsimonious"`, and `"int"` or
#'   `"intermediate"`.
#' @param include Optional minimization include setting. Currently, this
#'   argument accepts only `NULL`, `""`, or `"?"`.
#' @param dir.exp Theory-specific directional expectations. Use `NULL` when no
#'   intermediate solutions are monitored. Otherwise supply a named list with
#'   one entry per theory. Each entry may be an ordered character vector aligned
#'   to that theory's conditions or a named character vector whose names match
#'   that theory's conditions.
#' @param which_M Positive integer giving which solution alternative to use
#'   when minimization returns multiple models.
#' @param i_mode Character string controlling intermediate-solution selection.
#'   Accepted values are `"C1P1"` and `"all"`. The default is `"C1P1"` because
#'   comparative theory testing needs a single comparable intermediate branch
#'   per theory by default.
#' @param exclude_mode Character string controlling how excluded rows are
#'   handled for parsimonious and intermediate minimization. `"recompute"`
#'   recalculates exclusions separately for each theory-specific truth table,
#'   `"static"` reuses theory-specific static exclusions supplied through
#'   `exclude_static`, and `"none"` does not use exclusions.
#' @param exclude_recompute Named list of arguments passed to [QCA::findRows()]
#'   when `exclude_mode = "recompute"`.
#' @param exclude_static Static exclusions used when `exclude_mode = "static"`.
#'   For
#'   `theory.test()`, this must be a named list with one entry per theory when
#'   parsimonious or intermediate solutions are monitored.
#' @param progress Logical; if `TRUE`, show a text progress bar during
#'   per-theory QCA runs in interactive sessions.
#' @param x A `theory_test` object returned by [theory.test()].
#' @param row.names Logical; if `TRUE`, print row names in the selected
#'   solutions table printed by [print.theory_test()].
#' @param ... Additional arguments split between [QCA::truthTable()] and
#'   [QCA::minimize()]. The function also looks in `...` for `include`,
#'   `dir.exp`, or `direxp` if those arguments were not supplied explicitly. In
#'   [print.theory_test()], `...` is passed to [print.data.frame()]. In
#'   [as.data.frame.theory_test()], `...` is ignored.
#'
#' @returns An object of class `theory_test` with the following components:
#'   \describe{
#'     \item{`diagnostics`}{A detailed internal diagnostics table with one row
#'     per theory and monitored solution type.}
#'     \item{`results`}{A named list with `models`, `pairwise`, and
#'     `solutions` tables. `models` is populated with model-level diagnostics
#'     and fit values; `solutions` is populated with selected solution terms
#'     and term-level fit values; `pairwise` is populated with pairwise theory
#'     comparisons by solution type.}
#'     \item{`by_theory`}{A named list containing the theory-specific condition
#'     sets, directional expectations, truth tables, exclusions, minimization
#'     objects, selected solution terms used for comparison, and current run
#'     status.}
#'     \item{`settings`}{A list containing the analysis settings used in the
#'     call.}
#'   }
#'
#'   `print.theory_test()` prints a compact theory-specification summary and
#'   the selected solutions table. `as.data.frame.theory_test()` returns
#'   `results$models`.
#'
#' @examples
#' \donttest{
#' library(QCA)
#' data(LR)
#'
#' outcome <- "SURV"
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
#' theories <- list(
#'   development = c("DEV", "URB", "LIT"),
#'   industrial = c("DEV", "URB", "IND"),
#'   broad = c("DEV", "URB", "LIT", "IND", "STB")
#' )
#'
#' dir_exp <- list(
#'   development = c("1", "1", "1"),
#'   industrial = c("1", "1", "1"),
#'   broad = c("1", "1", "1", "1", "1")
#' )
#'
#' theory_out <- theory.test(
#'   data = dat,
#'   outcome = outcome,
#'   theories = theories,
#'   incl.cut = 0.8,
#'   n.cut = 1,
#'   solution = "all",
#'   dir.exp = dir_exp,
#'   progress = TRUE
#' )
#'
#' theory_out
#' as.data.frame(theory_out)
#' theory_out$results$solutions
#' theory_out$results$pairwise
#' }
#'
#' @seealso [calib.test()], [incl.test()], [ncut.test()], [loo.test()],
#'   [subsample.test()], [altset.test()], [cluster.test()], [sol.df()]
#' @export
theory.test <- function(
    data,
    outcome,
    theories,
    incl.cut = 1,
    n.cut = 1,
    solution = "all",
    include = NULL,
    dir.exp = NULL,
    which_M = 1,
    i_mode = "C1P1",
    exclude_mode = c("recompute", "static", "none"),
    exclude_recompute = list(type = 2),
    exclude_static = NULL,
    progress = TRUE,
    ...
) {
  .require_qca()

  selection_controls <- .normalize_i_mode(i_mode)
  i_mode <- selection_controls$i_mode
  exclude_mode <- match.arg(exclude_mode)
  mc <- match.call(expand.dots = FALSE)
  dots_raw <- list(...)
  .reject_exclusion_controls_in_dots(dots_raw, "theory.test")

  if (is.null(dim(data)) || is.null(nrow(data)) || nrow(data) < 1L) {
    stop("`data` must be a non-empty data frame object with at least one row.")
  }

  if (!is.character(outcome) || length(outcome) != 1L || is.na(outcome) || !nzchar(outcome)) {
    stop("`outcome` must be a single non-empty character string.")
  }
  outcome <- trimws(outcome)
  if (!outcome %in% colnames(data)) {
    stop("`outcome` must name a column in `data`.")
  }

  theories <- .normalize_theories(theories, data = data, outcome = outcome)

  if (!is.numeric(incl.cut) || length(incl.cut) != 1L || !is.finite(incl.cut) ||
      incl.cut < 0 || incl.cut > 1) {
    stop("`incl.cut` must be a single finite number in [0, 1].")
  }

  n.cut <- .as_integerish_scalar(n.cut, "n.cut", min = 1L)
  which_M <- .coerce_which_M(which_M)

  if (!is.logical(progress) || length(progress) != 1L || is.na(progress)) {
    stop("`progress` must be `TRUE` or `FALSE`.")
  }

  if (is.null(include)) {
    inc <- .dot_get(dots_raw, "include")
    if (!is.null(inc)) include <- inc
  }

  if (is.null(dir.exp)) {
    de <- .dot_get(dots_raw, "dir.exp")
    if (is.null(de)) de <- .dot_get(dots_raw, "direxp")
    if (!is.null(de)) dir.exp <- de
  }

  solution_norm <- .normalize_solution_std(solution)
  dir.exp <- .normalize_theory_dir_exp(dir.exp, theories, solution = solution_norm)

  solution_controls <- .resolve_solution_controls(
    solution = solution_norm,
    include = include,
    dir.exp = dir.exp,
    caller = "theory.test",
    style = "std"
  )

  solution <- solution_controls$solution
  include <- solution_controls$include
  monitored_solutions <- solution_controls$monitored

  .validate_exclusion_controls(
    exclude_mode = exclude_mode,
    exclude_recompute = exclude_recompute,
    exclude_static = exclude_static,
    exclude_recompute_supplied = !is.null(mc$exclude_recompute),
    exclude_static_supplied = !is.null(mc$exclude_static),
    monitored_solutions = monitored_solutions,
    style = "std"
  )
  .validate_theory_static_exclude(
    exclude_mode = exclude_mode,
    exclude_static = exclude_static,
    theories = theories,
    monitored_solutions = monitored_solutions
  )

  dots_split <- .split_truth_table_minimize_dots(dots_raw)
  progress_state <- .new_qcaert_progress(total = length(theories), progress = progress)
  on.exit(progress_state$close(), add = TRUE)

  by_theory <- lapply(names(theories), function(theory) {
    run <- .theory_run_one(
      theory = theory,
      data = data,
      outcome = outcome,
      conditions = theories[[theory]],
      dir.exp = if (is.null(dir.exp)) NULL else dir.exp[[theory]],
      incl.cut = incl.cut,
      n.cut = n.cut,
      monitored_solutions = monitored_solutions,
      exclude_mode = exclude_mode,
      exclude_recompute = exclude_recompute,
      exclude_static = exclude_static,
      dots_tt = dots_split$tt,
      dots_min = dots_split$min,
      which_M = which_M,
      i_mode = i_mode
    )
    progress_state$tick()
    run
  })
  names(by_theory) <- names(theories)

  diagnostics <- .theory_diagnostics_from_runs(
    by_theory = by_theory,
    monitored_solutions = monitored_solutions
  )
  models <- .theory_models_from_runs(
    by_theory = by_theory,
    monitored_solutions = monitored_solutions,
    which_M = which_M,
    i_mode = i_mode
  )
  solutions <- .theory_solutions_from_runs(
    by_theory = by_theory,
    monitored_solutions = monitored_solutions,
    which_M = which_M,
    i_mode = i_mode
  )
  pairwise <- .theory_pairwise_from_runs(
    by_theory = by_theory,
    models = models,
    monitored_solutions = monitored_solutions,
    which_M = which_M,
    i_mode = i_mode
  )
  results <- .empty_theory_results()
  results$models <- models
  results$pairwise <- pairwise
  results$solutions <- solutions

  .new_result_object(
    "theory_test",
    diagnostics = diagnostics,
    results = results,
    by_theory = by_theory,
    settings = list(
      outcome = outcome,
      theories = theories,
      incl.cut = incl.cut,
      n.cut = n.cut,
      solution = solution,
      monitored_solutions = monitored_solutions,
      include = include,
      dir.exp = dir.exp,
      which_M = which_M,
      i_mode = i_mode,
      exclude_mode = exclude_mode,
      exclude_recompute = exclude_recompute,
      exclude_static = exclude_static,
      progress = progress
    )
  )
}

#' Print a `theory_test` object
#'
#' @rdname theory.test
#' @export
print.theory_test <- function(x, row.names = FALSE, ...) {
  settings <- x$settings

  cat("<theory_test>\n")
  cat("qcaERT theory-specification test\n\n")
  cat("Outcome: ", settings$outcome, "\n", sep = "")

  theories <- settings$theories
  cat("Theories tested: ", length(theories), "\n", sep = "")
  for (theory in names(theories)) {
    cat("  ", theory, ": ", paste(theories[[theory]], collapse = ", "), "\n", sep = "")
  }

  cat("\nSolution: ", settings$solution, "\n", sep = "")
  cat("Monitored solutions: ", paste(settings$monitored_solutions, collapse = ", "), "\n", sep = "")
  cat("which_M: ", settings$which_M, "\n", sep = "")
  if (any(settings$monitored_solutions %in% "intermediate")) {
    cat("i_mode: ", settings$i_mode, "\n", sep = "")
  }

  cat("\nTruth-table settings:\n")
  cat("  incl.cut: ", format(settings$incl.cut, trim = TRUE), "\n", sep = "")
  cat("  n.cut: ", settings$n.cut, "\n", sep = "")

  cat("\nRun status:\n")
  for (theory in names(x$by_theory)) {
    cat("  ", theory, ": ", x$by_theory[[theory]]$status, "\n", sep = "")
  }

  if (.theory_n_rows(x$results$solutions) > 0L) {
    cat("\nSolutions\n")
    .theory_print_solutions(x$results$solutions, row.names = row.names, ...)
  }

  cat("\nTables:\n")
  cat("  as.data.frame(x) or x$results$models: model-level diagnostics (", .theory_n_rows(x$results$models), " rows)\n", sep = "")
  cat("  x$results$solutions: extracted solution terms (", .theory_n_rows(x$results$solutions), " rows)\n", sep = "")
  cat("  x$results$pairwise: pairwise theory comparisons (", .theory_n_rows(x$results$pairwise), " rows)\n", sep = "")
  cat("  x$diagnostics: raw per-theory solution_type diagnostics (", .theory_n_rows(x$diagnostics), " rows)\n", sep = "")
  cat("  x$by_theory: truth tables, exclusions, and minimization objects\n", sep = "")

  invisible(x)
}

#' Return the model-level table from a `theory_test` object
#'
#' @rdname theory.test
#' @export
as.data.frame.theory_test <- function(x, ...) {
  x$results$models
}
