#' qcaERT argument and output conventions
#'
#' The qcaERT robustness functions are siblings. They share a common public
#' structure for solution controls, exclusion handling, returned objects,
#' printing, and data-frame coercion. Individual function pages describe the
#' function-specific workflow; this page describes the package-wide conventions
#' that should be read consistently across the function family.
#'
#' @section Solution Controls:
#' Most functions accept `solution`, `include`, `dir.exp`, `which_M`, and
#' `i_mode`.
#'
#' `solution` may be `"all"`, `"con"`/`"conservative"`,
#' `"par"`/`"parsimonious"`, or `"int"`/`"intermediate"`. For single-solution-type
#' calls, `include` is optional and is resolved from `solution`: conservative
#' uses `include = ""`, while parsimonious and intermediate use
#' `include = "?"`. Intermediate solutions require `dir.exp`.
#'
#' When `solution = "all"`, do not supply `include`. The monitored set contains
#' conservative and parsimonious solutions by default. If `dir.exp` is supplied,
#' the intermediate solution is also monitored. In stepwise boundary searches
#' such as [incl.test()], [ncut.test()], and [calib.test()], these solution
#' types are searched independently; one solution type changing does not stop
#' the search for another solution type. When a baseline or step fails in one
#' monitored solution type, inspect the solution-type-specific rows in
#' `diagnostics`, `results`, or supporting `by_*` components before interpreting
#' an all-solution run as a single combined failure.
#'
#' `which_M` selects the model or solution alternative to use when QCA
#' minimization returns multiple alternatives. `i_mode` controls which
#' intermediate branches are included; accepted values are `"all"` and
#' `"C1P1"`. Most robustness functions default to allowing all intermediate
#' branches. `theory.test()` defaults to `"C1P1"` because
#' comparative theory testing needs one comparable intermediate branch per
#' theory by default.
#'
#' @section Exclusions:
#' Functions that can monitor parsimonious or intermediate solutions use a
#' common exclusion convention. `exclude_mode = "recompute"` recalculates excluded
#' rows for each perturbed, reduced, or sampled truth table using
#' [QCA::findRows()] and `exclude_recompute`. `exclude_mode = "static"`
#' reuses the supplied `exclude_static` object. `exclude_mode = "none"` avoids
#' exclusion handling.
#'
#' The family default for recomputation is
#' `exclude_recompute = list(type = 2)`. [incl.test()], [ncut.test()],
#' [calib.test()], [loo.test()], [subsample.test()], [altset.test()], and
#' [theory.test()] share this `exclude_mode`, `exclude_recompute`, and
#' `exclude_static` convention. `cluster.test()` starts from an existing truth
#' table and therefore accepts `exclude` directly instead of exposing
#' `exclude_mode`.
#'
#' @section Calibration Specifications:
#' Calibration-family functions use `calib_spec` for calibration information.
#' A `calib_spec` entry is named by calibrated set, contains the raw source name
#' in `raw`, the calibration `type`, and `thresholds`, and may contain `method`
#' and a `calibrate` list forwarded to [QCA::calibrate()].
#'
#' For condition-only calibration tests, `calib_spec` is named by
#' `conditions`. When `calib.test()` or `altset.test()` is called with
#' `test.outcome = TRUE`, `calib_spec` is named by `c(conditions, outcome)`.
#' The outcome must not be included in
#' `conditions`; outcome calibration testing is requested explicitly with
#' `test.outcome = TRUE`.
#'
#' Crisp conditions use one threshold. Fuzzy direct calibration supports three
#' thresholds in `c(E, C, I)` order or six thresholds in
#' `c(E1, C1, I1, I2, C2, E2)` order. Fuzzy indirect calibration treats its
#' thresholds as ordered cutpoints.
#'
#' These calibration-perturbation routines are designed for QCA workflows using
#' crisp and fuzzy sets. They are not multi-value calibration tools.
#' In [loo.test()] and [subsample.test()] with `calib = "recompute"`, a
#' `calib_spec` entry may additionally contain `findTh`, a named list of
#' [QCA::findTh()] arguments, to re-estimate thresholds on each reduced or
#' subsampled raw dataset. Without `findTh`, the baseline `thresholds` stored in
#' `calib_spec` are reused.
#'
#' @section Result Objects:
#' Most robustness functions return an S3 object with `diagnostics`, `results`,
#' and `settings`, plus supporting components such as `baseline`, `bounds`,
#' `by_direction`, `by_case`, `by_run`, `by_draw`, or `summary`.
#'
#' `diagnostics` is the detailed internal table. `results` is the clean
#'  table. The `print()` method shows a concise summary plus the
#'  results, and `as.data.frame()` returns the clean `results` table.
#'
#' For [incl.test()], [ncut.test()], and [calib.test()], `result_shape`
#' controls the layout of the clean `results` table when `solution = "all"`.
#' The default `"wide"` layout keeps one row per tested path and uses
#' solution-type-prefixed columns such as `con_last_safe` and `par_reason`. The
#' `"long"` layout uses one row per tested path and solution type, with a
#' `solution_type` column. The raw `diagnostics` table is unchanged.
#'
#' `cluster.test()` and `theory.test()` use structured `results` lists because
#' their clean output has more than one natural table. `cluster.test()` returns
#' `overview`, `clusters`, and `units`; `as.data.frame()` returns
#' `results$overview`. `theory.test()` returns `models`, `pairwise`, and
#' `solutions`; `as.data.frame()` returns `results$models`.
#'
#' @section Plotting:
#' Plotting requires `ggplot2`. `plot()` methods are provided
#' for `incl_test`, `calib_test`, and `theory_test` objects. Interval and trace
#' views map directly to stored boundary-search diagnostics; theory plots map
#' selected theory-specific models into consistency/coverage space.
#' [sol.chart()] renders [sol.df()] tables as solution charts. See
#' `?qcaERT_plots` and `?sol.chart`.
#'
#' @section QCA Solution Objects:
#' `cluster.test()` and [sol.df()] consume QCA minimization objects. [sol.df()]
#' extracts a compact data frame, and [sol.chart()] gives a visual display of
#' that extracted table. Case and solution-expression reconstruction use the
#' data-frame `pims` component produced by [QCA::minimize()].
#'
#' @name qcaERT_conventions
NULL
