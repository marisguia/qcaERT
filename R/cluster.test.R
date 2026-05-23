#' Cluster heterogeneity diagnostics for QCA configurations
#'
#' Evaluates how the consistency and coverage of selected QCA configurations
#' vary across clusters and, when repeated units are available, across units
#' observed in more than one cluster. The function starts from an existing
#' truth table, minimizes it under the requested solution type, extracts the
#' selected baseline configurations, and then compares pooled fit values with
#' cluster-specific and within-unit fit values.
#'
#' @param data A data frame object containing the columns used to build
#'   `tt` plus the cluster and, optionally, unit identifiers. `data`
#'   must have the same number of rows as `tt$recoded.data`.
#' @param tt A truth table object of class `"QCA_tt"`, typically created with
#'   [QCA::truthTable()].
#' @param cluster_id Name of the column in `data` identifying clusters. This
#'   must be a single non-empty character string, and the column must contain
#'   at least two distinct non-missing cluster values.
#' @param unit_id Optional name of the column in `data` identifying units that
#'   may appear in more than one cluster. If `NULL`, units are treated as row
#'   positions and within-unit diagnostics are not available across clusters.
#' @param solution Solution type to evaluate. Accepted values are `"all"`,
#'   `"con"` or `"conservative"`, `"par"` or `"parsimonious"`, and `"int"` or
#'   `"intermediate"`.
#' @param include Optional minimization include setting. Currently, this
#'   argument accepts only `NULL`, `""`, or `"?"`.
#' @param dir.exp Directional expectations used when the monitored solution is
#'   intermediate. When `solution = "all"`, supplying `dir.exp` adds
#'   intermediate solutions to the monitored set.
#' @param exclude Optional exclusion specification passed to [QCA::minimize()]
#'   for parsimonious and intermediate minimization.
#' @param which_M Positive integer giving which solution alternative to use
#'   when minimization returns multiple models.
#' @param i_mode Character string controlling intermediate-solution selection.
#'   Accepted values are `"all"` and `"C1P1"`.
#' @param necessity Logical; if `TRUE`, compute fit as necessity consistency
#'   and coverage. If `FALSE`, compute fit as sufficiency consistency and
#'   coverage.
#' @param progress Logical; if `TRUE` and the session is interactive, show a
#'   text progress bar while minimizing the monitored solution types.
#' @param x A `cluster_test` object returned by [cluster.test()].
#' @param row.names Logical; passed to [print.data.frame()] by
#'   [print.cluster_test()].
#' @param ... Additional arguments. In [cluster.test()], these are filtered and
#'   forwarded to [QCA::minimize()]. The function also looks in `...` for
#'   `include`, `dir.exp` or `direxp`, and `exclude` or `omit` if those
#'   arguments were not supplied explicitly. In [print.cluster_test()], `...`
#'   is passed to [print.data.frame()]. In
#'   [as.data.frame.cluster_test()], `...` is ignored.
#'
#' @returns An object of class `cluster_test` with the following components:
#'   \describe{
#'     \item{`diagnostics`}{A detailed data frame with one row per selected
#'     configuration. It records the solution type, configuration key, component count,
#'     status, pooled consistency and coverage, maximum and mean absolute
#'     cluster-level deltas, the clusters with the worst consistency and
#'     coverage values, whether within-unit diagnostics were available, the
#'     number of repeated units, and error information when a configuration could not
#'     be evaluated.}
#'     \item{`results`}{A named list with three tables:
#'       \describe{
#'         \item{`overview`}{One row per selected configuration, summarizing pooled fit,
#'         maximum cluster deltas, worst clusters, and within-unit
#'         availability.}
#'         \item{`clusters`}{One row per configuration-component-cluster combination,
#'         giving cluster-specific consistency, coverage, and deltas from the
#'         pooled configuration values. The whole configuration is stored as
#'         `component = "solution"`; separate term-level rows are included only
#'         for multi-term solutions.}
#'         \item{`units`}{One row per configuration-component-unit combination for
#'         units observed in more than one cluster, giving within-unit
#'         consistency, coverage, and deltas from the pooled configuration values.
#'         Separate term-level rows are included only for multi-term solutions.}
#'       }}
#'     \item{`baseline`}{A list containing the input truth table, minimization
#'     results by solution type, selected solution terms used for comparison,
#'     metadata, and the extracted configuration definitions used for the
#'     heterogeneity diagnostics.}
#'     \item{`by_cluster`}{A named list of detailed cluster-level diagnostics
#'     for each selected configuration.}
#'     \item{`by_unit`}{A named list of detailed unit-level diagnostics for
#'     each selected configuration when repeated units are available, or `NULL`
#'     otherwise.}
#'     \item{`settings`}{A list containing the analysis settings used to build
#'     the result object.}
#'   }
#'
#'   `print.cluster_test()` prints a compact overview and, when the object has
#'   one displayed configuration, cluster and within-unit details.
#'   `as.data.frame.cluster_test()` returns `results$overview`.
#'
#' @details
#' The function recovers the outcome membership from `tt$recoded.data`, runs
#' minimization for the requested solution type or solution types, extracts the
#' selected baseline configurations, and computes pooled fit values for each
#' configuration and configuration component.
#'
#' Cluster-level diagnostics compare the pooled fit of each selected
#' configuration with the fit obtained inside each cluster defined by
#' `cluster_id`.
#'
#' If `unit_id` identifies units that appear in more than one cluster, the
#' function also computes within-unit diagnostics by comparing the same unit
#' across clusters.
#'
#' Configuration extraction uses the data-frame `pims` component produced by
#' [QCA::minimize()]. The shared solution-control, QCA solution-object, and
#' returned-object conventions are described in `?qcaERT_conventions`.
#'
#' @examples
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
#' dat$development_group <- ifelse(
#'   LR$DEV >= median(LR$DEV, na.rm = TRUE),
#'   "higher development",
#'   "lower development"
#' )
#' dat$case_id <- rownames(dat)
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
#' out <- cluster.test(
#'   data = dat,
#'   tt = tt,
#'   cluster_id = "development_group",
#'   unit_id = "case_id",
#'   solution = "intermediate",
#'   dir.exp = dir_exp,
#'   exclude = enhanced,
#'   which_M = 1,
#'   necessity = FALSE,
#'   progress = TRUE
#' )
#'
#' out
#' as.data.frame(out)
#' out$results$clusters
#' out$results$units
#' }
#'
#' @seealso [calib.test()], [incl.test()], [ncut.test()], [loo.test()],
#'   [subsample.test()], [altset.test()], [theory.test()], [sol.df()]
#' @export
cluster.test <- function(
    data,
    tt,
    cluster_id,
    unit_id = NULL,
    solution = "all",
    include = NULL,
    dir.exp = NULL,
    exclude = NULL,
    which_M = 1,
    i_mode = c("all", "C1P1"),
    necessity = FALSE,
    progress = TRUE,
    ...
) {
  .require_qca()

  selection_controls <- .normalize_i_mode(i_mode)
  i_mode <- selection_controls$i_mode
  dots_raw <- list(...)

  if (is.null(dim(data)) || is.null(nrow(data)) || nrow(data) < 1L) {
    stop("`data` must be a non-empty data frame object with at least one row.")
  }

  if (!methods::is(tt, "QCA_tt")) {
    stop("`tt` must be a truth table object of class 'QCA_tt'.")
  }

  if (!is.character(cluster_id) || length(cluster_id) != 1L || !nzchar(cluster_id)) {
    stop("`cluster_id` must be a single non-empty character string.")
  }
  if (!cluster_id %in% colnames(data)) {
    stop("`cluster_id` must name a column in `data`.")
  }

  if (!is.null(unit_id)) {
    if (!is.character(unit_id) || length(unit_id) != 1L || !nzchar(unit_id)) {
      stop("`unit_id` must be NULL or a single non-empty character string.")
    }
    if (!unit_id %in% colnames(data)) {
      stop("`unit_id` must name a column in `data`.")
    }
  }

  if (!is.logical(necessity) || length(necessity) != 1L || is.na(necessity)) {
    stop("`necessity` must be a single TRUE/FALSE value.")
  }

  if (is.null(tt$recoded.data) || nrow(tt$recoded.data) != nrow(data)) {
    stop("`data` must have the same number of rows as `tt$recoded.data`.")
  }

  .err_msg <- function(e) {
    if (!inherits(e, "error")) return(NA_character_)
    tryCatch(conditionMessage(e), error = function(...) as.character(e))
  }

  .fit_pair <- function(x, y, necessity = FALSE) {
    if (length(x) != length(y)) stop("Internal error: x and y must have equal length.")

    ok <- !(is.na(x) | is.na(y))
    x <- as.numeric(x[ok])
    y <- as.numeric(y[ok])

    if (length(x) == 0L) {
      return(c(consistency = NA_real_, coverage = NA_real_))
    }

    num <- sum(pmin(x, y))
    den_cons <- if (necessity) sum(y) else sum(x)
    den_cov  <- if (necessity) sum(x) else sum(y)

    cons <- if (den_cons > 0) num / den_cons else NA_real_
    cov  <- if (den_cov  > 0) num / den_cov  else NA_real_

    c(consistency = cons, coverage = cov)
  }

  .summarize_delta <- function(d) {
    d <- as.numeric(d)
    d <- d[!is.na(d)]
    if (length(d) == 0L) {
      return(c(max_abs = NA_real_, mean_abs = NA_real_))
    }
    c(max_abs = max(abs(d)), mean_abs = mean(abs(d)))
  }

  .diag_component_tables <- function(x, y, cluster_vec, unit_vec, necessity) {
    pooled <- .fit_pair(x, y, necessity = necessity)

    ok <- !(is.na(x) | is.na(y) | is.na(cluster_vec) | is.na(unit_vec))
    x <- as.numeric(x[ok])
    y <- as.numeric(y[ok])
    cluster_vec <- as.character(cluster_vec[ok])
    unit_vec <- as.character(unit_vec[ok])

    X <- xtabs(as.numeric(x) ~ unit_vec + cluster_vec)
    Y <- xtabs(as.numeric(y) ~ unit_vec + cluster_vec)

    cls <- colnames(X)
    cls_sizes <- table(cluster_vec)

    by_cluster <- lapply(seq_along(cls), function(j) {
      fitj <- .fit_pair(X[, j], Y[, j], necessity = necessity)
      data.frame(
        cluster_id = cls[j],
        cluster_size = as.integer(cls_sizes[cls[j]]),
        consistency = unname(fitj["consistency"]),
        coverage = unname(fitj["coverage"]),
        delta_consistency = unname(fitj["consistency"] - pooled["consistency"]),
        delta_coverage = unname(fitj["coverage"] - pooled["coverage"]),
        stringsAsFactors = FALSE
      )
    })
    by_cluster <- do.call(rbind, by_cluster)

    unit_clusters <- tapply(cluster_vec, unit_vec, function(z) length(unique(as.character(z))))
    repeated_units <- names(unit_clusters)[!is.na(unit_clusters) & unit_clusters > 1L]

    by_unit <- NULL
    if (length(repeated_units) > 0L) {
      row_keep <- rownames(X) %in% repeated_units
      Xu <- X[row_keep, , drop = FALSE]
      Yu <- Y[row_keep, , drop = FALSE]

      by_unit <- lapply(seq_len(nrow(Xu)), function(i) {
        fiti <- .fit_pair(Xu[i, ], Yu[i, ], necessity = necessity)
        data.frame(
          unit_id = rownames(Xu)[i],
          n_clusters = sum((Xu[i, ] > 0) | (Yu[i, ] > 0)),
          consistency = unname(fiti["consistency"]),
          coverage = unname(fiti["coverage"]),
          delta_consistency = unname(fiti["consistency"] - pooled["consistency"]),
          delta_coverage = unname(fiti["coverage"] - pooled["coverage"]),
          stringsAsFactors = FALSE
        )
      })
      by_unit <- do.call(rbind, by_unit)
    }

    list(
      pooled = pooled,
      by_cluster = by_cluster,
      by_unit = by_unit
    )
  }

  .run_solution_type <- function(tt_obj, solution_type, exclude, dir.exp, dots_filtered, which_M, i_mode) {
    args <- c(
      list(
        input = tt_obj,
        include = if (solution_type == "conservative") "" else "?"
      ),
      dots_filtered
    )

    if (solution_type == "intermediate") {
      args$dir.exp <- dir.exp
    }

    if (solution_type %in% c("parsimonious", "intermediate") && !is.null(exclude)) {
      args$exclude <- exclude
    }

    res <- tryCatch(
      suppressWarnings(do.call(QCA::minimize, args)),
      error = function(e) e
    )

    if (inherits(res, "error")) {
      return(list(
        status = "minimize_error",
        error_source = solution_type,
        error_message = .err_msg(res),
        res = NULL,
        sig = NULL,
        selected_solution_missing = NA,
        meta = NULL
      ))
    }

    sig_info <- .extract_solution_type_sig(
      res,
      solution_type = solution_type,
      which_M = which_M,
      i_mode = i_mode
    )

    list(
      status = "ok",
      error_source = NA_character_,
      error_message = NA_character_,
      res = res,
      sig = sig_info$sig,
      selected_solution_missing = isTRUE(sig_info$selected_solution_missing),
      meta = sig_info$meta
    )
  }

  conditions <- tt$options$conditions
  if (is.null(conditions) || length(conditions) < 1L) {
    stop("Could not infer conditions from `tt`.")
  }
  if (!is.null(tt$options$outcome)) {
    .validate_outcome_conditions_distinct(tt$options$outcome, conditions)
  }

  include_user <- include
  dir_user <- dir.exp
  exclude_user <- exclude
  solution_user <- solution

  if (is.null(include)) {
    inc <- .dot_get(dots_raw, "include")
    if (!is.null(inc)) include <- inc
  }

  if (is.null(dir.exp)) {
    de <- .dot_get(dots_raw, "dir.exp")
    if (is.null(de)) de <- .dot_get(dots_raw, "direxp")
    if (!is.null(de)) dir.exp <- de
  }
  dir.exp <- .normalize_dir_exp_generic(
    dir.exp,
    conditions = conditions,
    endpoint_phrase = "the truth table conditions"
  )

  if (is.null(exclude)) {
    ex <- .dot_get(dots_raw, "exclude")
    if (is.null(ex)) ex <- .dot_get(dots_raw, "omit")
    if (!is.null(ex)) exclude <- ex
  }

  solution_controls <- .resolve_solution_controls(
    solution = solution,
    include = include,
    dir.exp = dir.exp,
    caller = "cluster.test",
    style = "std"
  )

  solution <- solution_controls$solution
  include <- solution_controls$include
  monitored_solutions <- solution_controls$monitored
  which_M <- .coerce_which_M(which_M)
  dots_filtered <- .filter_dots_for_minimize(dots_raw)

  progress_state <- .new_qcaert_progress(
    total = 2L * length(monitored_solutions),
    progress = progress
  )
  on.exit(progress_state$close(), add = TRUE)
  .bump_pb <- progress_state$tick

  cluster_nonmiss <- stats::na.omit(as.character(data[[cluster_id]]))
  if (length(unique(cluster_nonmiss)) < 2L) {
    stop("`cluster_id` must identify at least two distinct non-missing clusters.")
  }

  outcome_name <- tt$options$outcome
  if (length(outcome_name) > 1L) outcome_name <- outcome_name[1L]
  outcome_name <- gsub("^~", "", trimws(as.character(outcome_name)))

  if (!outcome_name %in% colnames(tt$recoded.data)) {
    stop("Could not recover the outcome membership from `tt$recoded.data`.")
  }

  recoded_outcome <- as.numeric(tt$recoded.data[, outcome_name])
  cluster_vec <- as.character(data[[cluster_id]])
  if (is.null(unit_id)) {
    unit_vec <- as.character(seq_len(nrow(data)))
  } else {
    unit_vec <- as.character(data[[unit_id]])
  }

  baseline_res <- list(conservative = NULL, parsimonious = NULL, intermediate = NULL)
  baseline_sig <- list(conservative = NULL, parsimonious = NULL, intermediate = NULL)
  baseline_meta <- list(conservative = NULL, parsimonious = NULL, intermediate = NULL)
  baseline_status <- c(conservative = NA_character_, parsimonious = NA_character_, intermediate = NA_character_)
  baseline_error_source <- c(conservative = NA_character_, parsimonious = NA_character_, intermediate = NA_character_)
  baseline_error_message <- c(conservative = NA_character_, parsimonious = NA_character_, intermediate = NA_character_)

  for (rg in monitored_solutions) {
    rr <- .run_solution_type(
      tt_obj = tt,
      solution_type = rg,
      exclude = exclude,
      dir.exp = dir.exp,
      dots_filtered = dots_filtered,
      which_M = which_M,
      i_mode = i_mode
    )

    baseline_status[[rg]] <- rr$status
    baseline_error_source[[rg]] <- rr$error_source
    baseline_error_message[[rg]] <- rr$error_message
    baseline_res[[rg]] <- rr$res
    baseline_sig[[rg]] <- rr$sig
    baseline_meta[[rg]] <- rr$meta

    .bump_pb()
  }

  targets <- list()
  diagnostics_rows <- list()
  by_cluster <- list()
  by_unit <- list()

  for (rg in monitored_solutions) {
    if (!isTRUE(baseline_status[[rg]] == "ok")) {
      target_key <- paste0("<", toupper(substr(rg, 1L, 3L)), "_ERROR>")
      diagnostics_rows[[length(diagnostics_rows) + 1L]] <- data.frame(
        solution_type = rg,
        i_mode = if (identical(rg, "intermediate")) i_mode else NA_character_,
        which_M = which_M,
        necessity = necessity,
        configuration_key = target_key,
        component_count = NA_integer_,
        status = "minimize_error",
        error_source = baseline_error_source[[rg]],
        error_message = baseline_error_message[[rg]],
        pooled_consistency = NA_real_,
        pooled_coverage = NA_real_,
        n_clusters = length(unique(stats::na.omit(cluster_vec))),
        max_abs_delta_consistency = NA_real_,
        max_abs_delta_coverage = NA_real_,
        mean_abs_delta_consistency = NA_real_,
        mean_abs_delta_coverage = NA_real_,
        worst_cluster_consistency_id = NA_character_,
        worst_cluster_coverage_id = NA_character_,
        worst_cluster_consistency = NA_real_,
        worst_cluster_coverage = NA_real_,
        within_available = FALSE,
        n_units_repeated = 0L,
        max_abs_within_delta_consistency = NA_real_,
        max_abs_within_delta_coverage = NA_real_,
        mean_abs_within_delta_consistency = NA_real_,
        mean_abs_within_delta_coverage = NA_real_,
        stringsAsFactors = FALSE
      )

      .bump_pb()
      next
    }

    tar_rg <- .qca_extract_targets_for_solution_type(
      res = baseline_res[[rg]],
      solution_type = rg,
      which_M = which_M,
      i_mode = i_mode
    )

    for (ttg in tar_rg) {
      target_key <- ttg$target_key
      component_count <- length(ttg$terms)
      term_components <- if (length(ttg$terms) > 1L) ttg$terms else character(0)

      if (!identical(ttg$status, "ok")) {
        diagnostics_rows[[length(diagnostics_rows) + 1L]] <- data.frame(
          solution_type = rg,
          i_mode = if (identical(rg, "intermediate")) i_mode else NA_character_,
          which_M = which_M,
          necessity = necessity,
          configuration_key = target_key,
          component_count = component_count,
          status = ttg$status,
          error_source = ttg$error_source,
          error_message = ttg$error_message,
          pooled_consistency = NA_real_,
          pooled_coverage = NA_real_,
          n_clusters = length(unique(stats::na.omit(cluster_vec))),
          max_abs_delta_consistency = NA_real_,
          max_abs_delta_coverage = NA_real_,
          mean_abs_delta_consistency = NA_real_,
          mean_abs_delta_coverage = NA_real_,
          worst_cluster_consistency_id = NA_character_,
          worst_cluster_coverage_id = NA_character_,
          worst_cluster_consistency = NA_real_,
          worst_cluster_coverage = NA_real_,
          within_available = FALSE,
          n_units_repeated = 0L,
          max_abs_within_delta_consistency = NA_real_,
          max_abs_within_delta_coverage = NA_real_,
          mean_abs_within_delta_consistency = NA_real_,
          mean_abs_within_delta_coverage = NA_real_,
          stringsAsFactors = FALSE
        )

        targets[[target_key]] <- c(
          ttg,
          list(
            outcome_membership = recoded_outcome,
            pooled_consistency = NA_real_,
            pooled_coverage = NA_real_
          )
        )
        next
      }

      comp_names <- c("solution", term_components)
      comp_vectors <- list(solution = as.numeric(ttg$solution_membership))

      if (length(term_components) > 0L &&
          !is.null(ttg$term_memberships) &&
          ncol(ttg$term_memberships) > 0L) {
        for (j in seq_along(ttg$terms)) {
          term_name <- ttg$terms[j]
          if (term_name %in% term_components) {
            comp_vectors[[term_name]] <- as.numeric(ttg$term_memberships[[j]])
          }
        }
      }

      pooled_terms <- vector("list", length(comp_names))
      names(pooled_terms) <- comp_names

      cluster_terms <- vector("list", length(comp_names))
      names(cluster_terms) <- comp_names

      unit_terms <- vector("list", length(comp_names))
      names(unit_terms) <- comp_names

      for (nm in comp_names) {
        dd <- .diag_component_tables(
          x = comp_vectors[[nm]],
          y = recoded_outcome,
          cluster_vec = cluster_vec,
          unit_vec = unit_vec,
          necessity = necessity
        )

        pooled_terms[[nm]] <- dd$pooled

        tmpc <- dd$by_cluster
        tmpc$component <- nm
        cluster_terms[[nm]] <- tmpc[, c(
          "cluster_id",
          "cluster_size",
          "component",
          "consistency",
          "coverage",
          "delta_consistency",
          "delta_coverage"
        )]

        if (!is.null(dd$by_unit)) {
          tmpu <- dd$by_unit
          tmpu$component <- nm
          unit_terms[[nm]] <- tmpu[, c(
            "unit_id",
            "n_clusters",
            "component",
            "consistency",
            "coverage",
            "delta_consistency",
            "delta_coverage"
          )]
        } else {
          unit_terms[[nm]] <- NULL
        }
      }

      clusters_df <- do.call(rbind, cluster_terms)
      units_df <- if (all(vapply(unit_terms, is.null, logical(1)))) {
        NULL
      } else {
        do.call(rbind, unit_terms[!vapply(unit_terms, is.null, logical(1))])
      }

      sol_cluster <- subset(clusters_df, component == "solution")
      sum_cl_cons <- .summarize_delta(sol_cluster$delta_consistency)
      sum_cl_cov  <- .summarize_delta(sol_cluster$delta_coverage)

      worst_cons_idx <- if (nrow(sol_cluster) > 0L && any(!is.na(sol_cluster$delta_consistency))) {
        which.max(abs(sol_cluster$delta_consistency))
      } else {
        NA_integer_
      }
      worst_cov_idx <- if (nrow(sol_cluster) > 0L && any(!is.na(sol_cluster$delta_coverage))) {
        which.max(abs(sol_cluster$delta_coverage))
      } else {
        NA_integer_
      }

      pooled_solution_df <- data.frame(
        consistency = unname(pooled_terms[["solution"]]["consistency"]),
        coverage = unname(pooled_terms[["solution"]]["coverage"]),
        stringsAsFactors = FALSE
      )

      term_only <- term_components
      pooled_terms_df <- if (length(term_only) > 0L) {
        data.frame(
          component = term_only,
          consistency = vapply(term_only, function(nm) unname(pooled_terms[[nm]]["consistency"]), numeric(1)),
          coverage = vapply(term_only, function(nm) unname(pooled_terms[[nm]]["coverage"]), numeric(1)),
          stringsAsFactors = FALSE
        )
      } else {
        data.frame(
          component = character(0),
          consistency = numeric(0),
          coverage = numeric(0),
          stringsAsFactors = FALSE
        )
      }

      term_hetero <- if (length(term_only) > 0L) {
        do.call(rbind, lapply(term_only, function(nm) {
          dfx <- subset(clusters_df, component == nm)
          sc <- .summarize_delta(dfx$delta_consistency)
          sv <- .summarize_delta(dfx$delta_coverage)
          data.frame(
            component = nm,
            max_abs_delta_consistency = unname(sc["max_abs"]),
            max_abs_delta_coverage = unname(sv["max_abs"]),
            mean_abs_delta_consistency = unname(sc["mean_abs"]),
            mean_abs_delta_coverage = unname(sv["mean_abs"]),
            stringsAsFactors = FALSE
          )
        }))
      } else {
        data.frame(
          component = character(0),
          max_abs_delta_consistency = numeric(0),
          max_abs_delta_coverage = numeric(0),
          mean_abs_delta_consistency = numeric(0),
          mean_abs_delta_coverage = numeric(0),
          stringsAsFactors = FALSE
        )
      }

      by_cluster[[target_key]] <- list(
        solution_type = rg,
        configuration_key = target_key,
        solution_expression = ttg$signature,
        terms = ttg$terms,
        pooled = list(
          solution = pooled_solution_df,
          terms = pooled_terms_df
        ),
        clusters = clusters_df,
        heterogeneity = list(
          solution = data.frame(
            max_abs_delta_consistency = unname(sum_cl_cons["max_abs"]),
            max_abs_delta_coverage = unname(sum_cl_cov["max_abs"]),
            mean_abs_delta_consistency = unname(sum_cl_cons["mean_abs"]),
            mean_abs_delta_coverage = unname(sum_cl_cov["mean_abs"]),
            stringsAsFactors = FALSE
          ),
          terms = term_hetero
        )
      )

      within_available <- !is.null(units_df)
      n_units_repeated <- if (within_available) length(unique(units_df$unit_id)) else 0L

      if (within_available) {
        within_sol_df <- subset(units_df, component == "solution")
        sum_wi_cons <- .summarize_delta(within_sol_df$delta_consistency)
        sum_wi_cov  <- .summarize_delta(within_sol_df$delta_coverage)

        term_hetero_u <- if (length(term_only) > 0L) {
          do.call(rbind, lapply(term_only, function(nm) {
            dfx <- subset(units_df, component == nm)
            sc <- .summarize_delta(dfx$delta_consistency)
            sv <- .summarize_delta(dfx$delta_coverage)
            data.frame(
              component = nm,
              max_abs_delta_consistency = unname(sc["max_abs"]),
              max_abs_delta_coverage = unname(sv["max_abs"]),
              mean_abs_delta_consistency = unname(sc["mean_abs"]),
              mean_abs_delta_coverage = unname(sv["mean_abs"]),
              stringsAsFactors = FALSE
            )
          }))
        } else {
          data.frame(
            component = character(0),
            max_abs_delta_consistency = numeric(0),
            max_abs_delta_coverage = numeric(0),
            mean_abs_delta_consistency = numeric(0),
            mean_abs_delta_coverage = numeric(0),
            stringsAsFactors = FALSE
          )
        }

        by_unit[[target_key]] <- list(
          solution_type = rg,
          configuration_key = target_key,
          solution_expression = ttg$signature,
          terms = ttg$terms,
          units = units_df,
          heterogeneity = list(
            solution = data.frame(
              max_abs_delta_consistency = unname(sum_wi_cons["max_abs"]),
              max_abs_delta_coverage = unname(sum_wi_cov["max_abs"]),
              mean_abs_delta_consistency = unname(sum_wi_cons["mean_abs"]),
              mean_abs_delta_coverage = unname(sum_wi_cov["mean_abs"]),
              stringsAsFactors = FALSE
            ),
            terms = term_hetero_u
          )
        )
      } else {
        sum_wi_cons <- c(max_abs = NA_real_, mean_abs = NA_real_)
        sum_wi_cov  <- c(max_abs = NA_real_, mean_abs = NA_real_)
      }

      diagnostics_rows[[length(diagnostics_rows) + 1L]] <- data.frame(
        solution_type = rg,
        i_mode = if (identical(rg, "intermediate")) i_mode else NA_character_,
        which_M = which_M,
        necessity = necessity,
        configuration_key = target_key,
        component_count = component_count,
        status = "ok",
        error_source = NA_character_,
        error_message = NA_character_,
        pooled_consistency = unname(pooled_terms[["solution"]]["consistency"]),
        pooled_coverage = unname(pooled_terms[["solution"]]["coverage"]),
        n_clusters = length(unique(stats::na.omit(cluster_vec))),
        max_abs_delta_consistency = unname(sum_cl_cons["max_abs"]),
        max_abs_delta_coverage = unname(sum_cl_cov["max_abs"]),
        mean_abs_delta_consistency = unname(sum_cl_cons["mean_abs"]),
        mean_abs_delta_coverage = unname(sum_cl_cov["mean_abs"]),
        worst_cluster_consistency_id = if (is.na(worst_cons_idx)) NA_character_ else as.character(sol_cluster$cluster_id[worst_cons_idx]),
        worst_cluster_coverage_id = if (is.na(worst_cov_idx)) NA_character_ else as.character(sol_cluster$cluster_id[worst_cov_idx]),
        worst_cluster_consistency = if (is.na(worst_cons_idx)) NA_real_ else as.numeric(sol_cluster$consistency[worst_cons_idx]),
        worst_cluster_coverage = if (is.na(worst_cov_idx)) NA_real_ else as.numeric(sol_cluster$coverage[worst_cov_idx]),
        within_available = within_available,
        n_units_repeated = n_units_repeated,
        max_abs_within_delta_consistency = unname(sum_wi_cons["max_abs"]),
        max_abs_within_delta_coverage = unname(sum_wi_cov["max_abs"]),
        mean_abs_within_delta_consistency = unname(sum_wi_cons["mean_abs"]),
        mean_abs_within_delta_coverage = unname(sum_wi_cov["mean_abs"]),
        stringsAsFactors = FALSE
      )

      targets[[target_key]] <- c(
        ttg,
        list(
          outcome_membership = recoded_outcome,
          pooled_consistency = unname(pooled_terms[["solution"]]["consistency"]),
          pooled_coverage = unname(pooled_terms[["solution"]]["coverage"])
        )
      )
    }

    .bump_pb()
  }

  diagnostics <- .bind_rows_result(diagnostics_rows)
  if (length(by_unit) == 0L) by_unit <- NULL
  baseline_configurations <- lapply(targets, function(x) {
    out <- x
    if ("target_key" %in% names(out)) {
      out$configuration_key <- out$target_key
      out$target_key <- NULL
    }
    if ("signature" %in% names(out)) {
      out$solution_expression <- out$signature
      out$signature <- NULL
    }
    out
  })

  .new_result_object(
    "cluster_test",
    diagnostics = diagnostics,
    results = .make_cluster_results(diagnostics, by_cluster, by_unit),
    baseline = list(
      tt = tt,
      res = baseline_res,
      solution_terms = baseline_sig,
      meta = baseline_meta,
      configurations = baseline_configurations
    ),
    by_cluster = by_cluster,
    by_unit = by_unit,
    settings = list(
      outcome = outcome_name,
      conditions = conditions,
      cluster_id = cluster_id,
      unit_id = unit_id,
      solution = solution,
      monitored_solutions = monitored_solutions,
      include = include,
      dir.exp = dir.exp,
      exclude = exclude,
      which_M = which_M,
      i_mode = i_mode,
      necessity = necessity,
      progress = progress
    )
  )
}

.cluster_print_number <- function(x, digits = 3L) {
  out <- rep(NA_character_, length(x))
  ok <- !is.na(x)
  out[ok] <- formatC(as.numeric(x[ok]), format = "f", digits = digits)
  out
}

.cluster_print_delta <- function(x, digits = 3L) {
  out <- rep(NA_character_, length(x))
  ok <- !is.na(x)
  out[ok] <- sprintf(paste0("%+.", digits, "f"), as.numeric(x[ok]))
  out
}

.cluster_print_has_columns <- function(x, cols) {
  is.data.frame(x) && all(cols %in% names(x))
}

.cluster_overview_for_print <- function(overview) {
  needed <- c(
    "solution_type",
    "configuration",
    "status",
    "pooled_consistency",
    "pooled_coverage",
    "worst_cluster_consistency_id",
    "max_abs_cluster_delta_consistency",
    "worst_cluster_coverage_id",
    "max_abs_cluster_delta_coverage",
    "within_available",
    "n_units_repeated"
  )
  if (!.cluster_print_has_columns(overview, needed)) {
    return(overview)
  }

  within_units <- ifelse(
    !is.na(overview$within_available) & overview$within_available,
    paste0("available (", overview$n_units_repeated, " units)"),
    "not available"
  )

  data.frame(
    solution_type = overview$solution_type,
    configuration = overview$configuration,
    status = overview$status,
    pooled_cons = .cluster_print_number(overview$pooled_consistency),
    pooled_cov = .cluster_print_number(overview$pooled_coverage),
    worst_consistency_cluster = overview$worst_cluster_consistency_id,
    max_delta_cons = .cluster_print_number(overview$max_abs_cluster_delta_consistency),
    worst_coverage_cluster = overview$worst_cluster_coverage_id,
    max_delta_cov = .cluster_print_number(overview$max_abs_cluster_delta_coverage),
    within_units = within_units,
    stringsAsFactors = FALSE
  )
}

.cluster_details_for_print <- function(results, overview) {
  clusters <- results$clusters
  needed <- c(
    "solution_type",
    "configuration",
    "component",
    "cluster_id",
    "cluster_size",
    "consistency",
    "delta_consistency",
    "coverage",
    "delta_coverage"
  )
  if (!.cluster_print_has_columns(overview, c("solution_type", "configuration")) ||
      nrow(overview) != 1L ||
      !.cluster_print_has_columns(clusters, needed)) {
    return(NULL)
  }

  keep <- clusters$solution_type == overview$solution_type[1L] &
    clusters$configuration == overview$configuration[1L] &
    clusters$component == "solution"
  clusters <- clusters[keep, , drop = FALSE]
  if (nrow(clusters) == 0L) {
    return(NULL)
  }

  data.frame(
    cluster_id = clusters$cluster_id,
    n = clusters$cluster_size,
    consistency = .cluster_print_number(clusters$consistency),
    delta_consistency = .cluster_print_delta(clusters$delta_consistency),
    coverage = .cluster_print_number(clusters$coverage),
    delta_coverage = .cluster_print_delta(clusters$delta_coverage),
    stringsAsFactors = FALSE
  )
}

.cluster_units_for_print <- function(results, overview) {
  units <- results$units
  needed <- c(
    "solution_type",
    "configuration",
    "component",
    "unit_id",
    "n_clusters",
    "consistency",
    "delta_consistency",
    "coverage",
    "delta_coverage"
  )
  if (!.cluster_print_has_columns(overview, c("solution_type", "configuration")) ||
      nrow(overview) != 1L ||
      !.cluster_print_has_columns(units, needed)) {
    return(NULL)
  }

  keep <- units$solution_type == overview$solution_type[1L] &
    units$configuration == overview$configuration[1L] &
    units$component == "solution"
  units <- units[keep, , drop = FALSE]
  if (nrow(units) == 0L) {
    return(NULL)
  }

  data.frame(
    unit_id = units$unit_id,
    n_clusters = units$n_clusters,
    consistency = .cluster_print_number(units$consistency),
    delta_consistency = .cluster_print_delta(units$delta_consistency),
    coverage = .cluster_print_number(units$coverage),
    delta_coverage = .cluster_print_delta(units$delta_coverage),
    stringsAsFactors = FALSE
  )
}

.cluster_within_unavailable_message <- function(overview, settings) {
  if (is.null(settings$unit_id) || is.na(settings$unit_id)) {
    return("Not available: no unit id was supplied.")
  }

  if (.cluster_print_has_columns(overview, "n_units_repeated") && nrow(overview) == 1L) {
    n_units <- overview$n_units_repeated[1L]
    return(paste0(
      "Not available: ",
      n_units,
      " units are observed in more than one cluster."
    ))
  }

  "Not available."
}

#' Print a `cluster_test` object
#'
#' @rdname cluster.test
#' @export
print.cluster_test <- function(x, row.names = FALSE, ...) {
  results <- x$results
  overview <- results$overview
  settings <- x$settings

  .print_qcaert_heading("cluster_test", "cluster heterogeneity", settings)
  if (!is.null(settings$cluster_id)) {
    cat("Cluster id: ", settings$cluster_id, "\n", sep = "")
  }
  if (!is.null(settings$unit_id) && !is.na(settings$unit_id)) {
    cat("Unit id: ", settings$unit_id, "\n", sep = "")
  }
  if (!is.null(settings$necessity)) {
    cat("Necessity mode: ", settings$necessity, "\n", sep = "")
  }

  .print_qcaert_table(.cluster_overview_for_print(overview), "Overview", row.names = row.names, ...)

  if (is.data.frame(overview) && nrow(overview) == 1L) {
    cluster_details <- .cluster_details_for_print(results, overview)
    if (!is.null(cluster_details)) {
      .print_qcaert_table(cluster_details, "Cluster details", row.names = row.names, ...)
    }

    cat("\nWithin-unit diagnostics\n")
    unit_details <- .cluster_units_for_print(results, overview)
    if (!is.null(unit_details)) {
      print(unit_details, row.names = row.names, ...)
    } else {
      cat(" ", .cluster_within_unavailable_message(overview, settings), "\n", sep = "")
    }
  }

  cat("\nSummary\n")
  if (is.data.frame(overview) && nrow(overview) > 0L) {
    n_ok <- sum(overview$status == "ok", na.rm = TRUE)
    n_errors <- sum(!is.na(overview$status) & overview$status != "ok", na.rm = TRUE)

    cat(" Configurations evaluated: ", nrow(overview), "\n", sep = "")
    cat(" Successful configurations: ", n_ok, "\n", sep = "")
    cat(" Non-ok configurations: ", n_errors, "\n", sep = "")
  }

  detailed_tables <- "x$results$overview"
  if (!is.null(results$clusters)) {
    detailed_tables <- c(detailed_tables, "x$results$clusters")
  }
  if (!is.null(results$units)) {
    detailed_tables <- c(detailed_tables, "x$results$units")
  }
  cat(" Detailed tables: ", paste(detailed_tables, collapse = ", "), "\n", sep = "")

  invisible(x)
}

#' Return the overview table from a `cluster_test` object
#'
#' @rdname cluster.test
#' @export
as.data.frame.cluster_test <- function(x, ...) {
  .as.data.frame_cluster_overview(x, ...)
}
