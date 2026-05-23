#' Build a compact solution table from QCA minimization output
#'
#' Extracts prime implicants, fit statistics, model identifiers, intermediate
#' branch labels, and optional case membership strings from supplied
#' [QCA::minimize()] result objects and returns them as one combined data
#' frame.
#'
#' @param conservative Optional conservative minimization result of class
#'   `"QCA_min"`.
#' @param intermediate Optional intermediate minimization result of class
#'   `"QCA_min"`.
#' @param parsimonious Optional parsimonious minimization result of class
#'   `"QCA_min"`.
#' @param solution Which solution type to extract. Accepted values are
#'   `"all"`, `"con"` or `"conservative"`, `"par"` or `"parsimonious"`, and
#'   `"int"` or `"intermediate"`. When `solution = "all"`, the function
#'   extracts every supplied object among `conservative`, `parsimonious`, and
#'   `intermediate`.
#' @param which_M Positive integer giving which model to extract when a
#'   supplied minimization object contains multiple models.
#' @param i_mode Character string controlling which intermediate branches to
#'   extract when `intermediate` is supplied and `branches = NULL`. Accepted
#'   values are `"all"` and `"C1P1"`.
#' @param branches Optional character vector of intermediate branch names to
#'   extract from `intermediate$i.sol`. When supplied, this overrides `i_mode`.
#' @param include_cases Logical; if `TRUE`, include a `Cases` column using the
#'   case information stored in the minimization object when available. Case
#'   reconstruction expects the data-frame `pims` component produced by
#'   [QCA::minimize()].
#' @param pi_incl_cut Optional lower cutoff applied to the prime-implicant
#'   consistency column. Rows with `Consistency_PI < pi_incl_cut` are removed.
#' @param digits Optional non-negative integer used to round numeric columns in
#'   the returned table.
#' @param na_string Single character string used to replace missing values in
#'   non-numeric output fields. The default is `"-"`.
#' @param verbose Logical; if `TRUE`, print a short progress message while
#'   processing each requested solution type.
#'
#' @returns A data frame with one row per extracted prime implicant and the
#'   following columns:
#'   \describe{
#'     \item{`Solution`}{Solution type label: `"Conservative"`,
#'     `"Parsimonious"`, or `"Intermediate"`.}
#'     \item{`Model`}{Selected model number when model-specific output is
#'     available.}
#'     \item{`Intermediate_CnPn`}{Intermediate branch label when rows come from
#'     `intermediate$i.sol`.}
#'     \item{`Prime_Implicants`}{Prime implicant name.}
#'     \item{`Consistency_PI`}{Prime-implicant consistency (`inclS`).}
#'     \item{`PRI_PI`}{Prime-implicant PRI.}
#'     \item{`Raw_Coverage_PI`}{Prime-implicant raw coverage (`covS`).}
#'     \item{`Unique_Coverage_PI`}{Prime-implicant unique coverage (`covU`).}
#'     \item{`Solution_Consistency`}{Solution-level consistency.}
#'     \item{`Solution_PRI`}{Solution-level PRI.}
#'     \item{`Solution_Coverage`}{Solution-level coverage.}
#'     \item{`Cases`}{Case labels or case strings when available and requested.}
#'   }
#'
#'   The function returns a regular data frame, not a custom result object.
#'
#' @details
#' Supply at least one of `conservative`, `parsimonious`, or `intermediate`.
#'
#' For conservative and parsimonious solutions, the function extracts rows from
#' the `IC` component of the supplied minimization object. For intermediate
#' solutions, it extracts rows from the selected `i.sol` branches.
#'
#' If a supplied minimization object contains multiple models in
#' `IC$individual`, `which_M` selects the model position to extract. If the
#' requested model does not exist, the function stops with an error.
#'
#' When `include_cases = TRUE`, the function first uses any `cases` column
#' already stored in the relevant `incl.cov` table. If that is unavailable, it
#' tries to reconstruct case strings from the corresponding `pims` data frame
#' stored by [QCA::minimize()].
#'
#' The QCA solution-object conventions are described in `?qcaERT_conventions`.
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
#' sol.df(
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
#' \donttest{
#' library(QCA)
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
#' sol.df(
#'   conservative = con,
#'   parsimonious = par,
#'   intermediate = int,
#'   solution = "all",
#'   which_M = 1,
#'   i_mode = "C1P1",
#'   include_cases = TRUE,
#'   digits = 3
#' )
#' }
#'
#' @seealso [sol.chart()], [calib.test()], [incl.test()], [ncut.test()],
#'   [loo.test()], [subsample.test()], [altset.test()], [theory.test()],
#'   [cluster.test()]
#' @export
sol.df <- function(
    conservative = NULL,
    intermediate = NULL,
    parsimonious = NULL,
    solution = "all",
    which_M = 1,
    i_mode = c("all", "C1P1"),
    branches = NULL,
    include_cases = TRUE,
    pi_incl_cut = NULL,
    digits = NULL,
    na_string = "-",
    verbose = FALSE
) {
  selection_controls <- .normalize_i_mode(i_mode)
  i_mode <- selection_controls$i_mode

  solution <- .normalize_solution_std(solution)

  which_M <- .as_integerish_scalar(which_M, "which_M", min = 1L)

  if (!is.null(pi_incl_cut)) {
    if (!is.numeric(pi_incl_cut) || length(pi_incl_cut) != 1L || !is.finite(pi_incl_cut)) {
      stop("`pi_incl_cut` must be NULL or a single finite numeric value.")
    }
  }

  if (!is.null(digits)) {
    if (!is.numeric(digits) || length(digits) != 1L || !is.finite(digits) || digits < 0) {
      stop("`digits` must be NULL or a single non-negative integer.")
    }
    digits <- as.integer(digits)
  }

  if (!is.character(na_string) || length(na_string) != 1L || is.na(na_string)) {
    stop("`na_string` must be a single character string.")
  }

  .validate_qca_min(conservative, "conservative")
  .validate_qca_min(intermediate, "intermediate")
  .validate_qca_min(parsimonious, "parsimonious")

  objects <- .qca_solution_objects(
    conservative = conservative,
    intermediate = intermediate,
    parsimonious = parsimonious
  )

  if (length(objects) == 0L) {
    stop("Provide at least one of `conservative`, `parsimonious`, or `intermediate`.")
  }

  requested <- .qca_requested_solutions(solution, objects)

  .build_rows <- function(
    solution_label,
    incl_cov,
    sol_incl_cov,
    cases,
    model = NA_integer_,
    branch = NA_character_
  ) {
    if (is.null(incl_cov) || !is.data.frame(incl_cov) || nrow(incl_cov) == 0L) {
      return(data.frame(
        Solution = character(0),
        Model = integer(0),
        Intermediate_CnPn = character(0),
        Prime_Implicants = character(0),
        Consistency_PI = numeric(0),
        PRI_PI = numeric(0),
        Raw_Coverage_PI = numeric(0),
        Unique_Coverage_PI = numeric(0),
        Solution_Consistency = numeric(0),
        Solution_PRI = numeric(0),
        Solution_Coverage = numeric(0),
        Cases = character(0),
        stringsAsFactors = FALSE
      ))
    }

    pi_names <- rownames(incl_cov)
    if (is.null(pi_names)) {
      pi_names <- as.character(seq_len(nrow(incl_cov)))
    }

    sol_row <- 1L
    if (!is.null(sol_incl_cov) && is.data.frame(sol_incl_cov) && nrow(sol_incl_cov) >= 1L) {
      sol_row <- 1L
    }

    out <- data.frame(
      Solution = rep(solution_label, nrow(incl_cov)),
      Model = rep(model, nrow(incl_cov)),
      Intermediate_CnPn = rep(branch, nrow(incl_cov)),
      Prime_Implicants = pi_names,
      Consistency_PI = if ("inclS" %in% colnames(incl_cov)) as.numeric(incl_cov[["inclS"]]) else NA_real_,
      PRI_PI = if ("PRI" %in% colnames(incl_cov)) as.numeric(incl_cov[["PRI"]]) else NA_real_,
      Raw_Coverage_PI = if ("covS" %in% colnames(incl_cov)) as.numeric(incl_cov[["covS"]]) else NA_real_,
      Unique_Coverage_PI = if ("covU" %in% colnames(incl_cov)) as.numeric(incl_cov[["covU"]]) else NA_real_,
      Solution_Consistency = if (!is.null(sol_incl_cov) && "inclS" %in% colnames(sol_incl_cov)) rep(as.numeric(sol_incl_cov[sol_row, "inclS"]), nrow(incl_cov)) else NA_real_,
      Solution_PRI = if (!is.null(sol_incl_cov) && "PRI" %in% colnames(sol_incl_cov)) rep(as.numeric(sol_incl_cov[sol_row, "PRI"]), nrow(incl_cov)) else NA_real_,
      Solution_Coverage = if (!is.null(sol_incl_cov) && "covS" %in% colnames(sol_incl_cov)) rep(as.numeric(sol_incl_cov[sol_row, "covS"]), nrow(incl_cov)) else NA_real_,
      Cases = as.character(cases),
      stringsAsFactors = FALSE
    )

    if (!is.null(pi_incl_cut)) {
      out <- out[is.na(out$Consistency_PI) | out$Consistency_PI >= pi_incl_cut, , drop = FALSE]
    }

    rownames(out) <- NULL
    out
  }

  .extract_standard <- function(obj, solution_type, which_M) {
    if (isTRUE(verbose)) {
      message("Processing ", solution_type, " solution...")
    }

    solution_label <- .qca_solution_label(solution_type)
    ic <- obj[["IC"]]
    selected <- .qca_select_ic_model(ic, which_M = which_M, context = solution_type)
    icm <- selected$ic
    incl_cov <- icm[["incl.cov"]]
    sol_incl_cov <- icm[["sol.incl.cov"]]
    pims <- if ("pims" %in% names(icm)) icm[["pims"]] else obj[["pims"]]
    cases <- .qca_cases_from_incl_cov(incl_cov, pims, include_cases = include_cases)

    .build_rows(
      solution_label = solution_label,
      incl_cov = incl_cov,
      sol_incl_cov = sol_incl_cov,
      cases = cases,
      model = selected$model,
      branch = NA_character_
    )
  }

  .extract_intermediate <- function(obj, which_M, i_mode, branches) {
    if (isTRUE(verbose)) {
      message("Processing intermediate solution...")
    }

    take_branches <- .resolve_branches(obj, i_mode = i_mode, branches = branches)

    out <- list()

    for (br in take_branches) {
      current <- obj[["i.sol"]][[br]]
      ic <- current[["IC"]]
      selected <- .qca_select_ic_model(
        ic,
        which_M = which_M,
        context = paste0("intermediate branch ", br)
      )

      icm <- selected$ic
      incl_cov <- icm[["incl.cov"]]
      sol_incl_cov <- icm[["sol.incl.cov"]]
      pims <- if ("pims" %in% names(icm)) icm[["pims"]] else current[["pims"]]
      cases <- .qca_cases_from_incl_cov(incl_cov, pims, include_cases = include_cases)

      out[[length(out) + 1L]] <- .build_rows(
        solution_label = "Intermediate",
        incl_cov = incl_cov,
        sol_incl_cov = sol_incl_cov,
        cases = cases,
        model = selected$model,
        branch = br
      )
    }

    if (length(out) == 0L) {
      return(.build_rows(
        solution_label = "Intermediate",
        incl_cov = NULL,
        sol_incl_cov = NULL,
        cases = character(0)
      ))
    }

    do.call(rbind, out)
  }

  rows <- list()

  if ("conservative" %in% requested) {
    rows[[length(rows) + 1L]] <- .extract_standard(
      obj = objects$conservative,
      solution_type = "conservative",
      which_M = which_M
    )
  }

  if ("parsimonious" %in% requested) {
    rows[[length(rows) + 1L]] <- .extract_standard(
      obj = objects$parsimonious,
      solution_type = "parsimonious",
      which_M = which_M
    )
  }

  if ("intermediate" %in% requested) {
    rows[[length(rows) + 1L]] <- .extract_intermediate(
      obj = objects$intermediate,
      which_M = which_M,
      i_mode = i_mode,
      branches = branches
    )
  }

  out <- .bind_rows_result(rows)

  if (!is.null(digits)) {
    numeric_cols <- vapply(out, is.numeric, logical(1))
    out[numeric_cols] <- lapply(out[numeric_cols], round, digits = digits)
  }

  if (!is.null(na_string)) {
    out$Model[is.na(out$Model)] <- na_string
    out$Intermediate_CnPn[is.na(out$Intermediate_CnPn)] <- na_string
    out$Cases[is.na(out$Cases)] <- na_string

    for (nm in names(out)) {
      if (!is.numeric(out[[nm]]) && !is.integer(out[[nm]])) {
        out[[nm]][is.na(out[[nm]])] <- na_string
      }
    }
  }

  rownames(out) <- NULL
  out
}
