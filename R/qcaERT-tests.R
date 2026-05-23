#' Choose a qcaERT robustness tool
#'
#' This page maps common QCA robustness tests to their respective functions
#'
#' @section Calibration:
#' Use [calib.test()] when you want to know whether a QCA solution is sensitive
#' to calibration thresholds. It perturbs one calibration anchor at a time and
#' reports how far each threshold can move before the monitored solution
#' changes or the search reaches a boundary.
#'
#' You can also use [altset.test()] when you want calibration perturbations to
#' be combined with other perturbations in sampled alternative analysis settings,
#' such as alternative inclusion cutoffs or frequency cutoffs.
#'
#' @section Inclusion or n.cut:
#' Use [incl.test()] to check the inclusion (consistency) cutoff for
#' sufficiency. It searches below and above the baseline `incl.cut` and records
#' the last safe cutoff and first failing cutoff in each direction.
#'
#' Use [ncut.test()] when the concern is the minimum number of cases under which
#' a truth table row is declared as a remainder. It searches lower and upper
#' `n.cut` values and records when the monitored solution changes or the
#' feasible boundary is reached.
#'
#' @section Cases:
#' Use [loo.test()] when you suspect individual cases may drive the solution.
#' It removes selected cases one at a time and compares each reduced analysis
#' with the baseline.
#'
#' Use [subsample.test()] when you want repeated partial-sample checks. It draws
#' subsamples, rebuilds the QCA analysis, and summarizes how often the baseline
#' solution is preserved across runs. This is a stringent, punishing robustness
#' check and is most useful when sample-composition sensitivity is substantively
#' important.
#'
#' @section Groups or clusters:
#' Use [cluster.test()] when you expect heterogeneity across clusters, groups,
#' or repeated units. It compares cluster-specific consistency, coverage, and
#' related fit patterns against the overall truth table.
#'
#' @section Theory Specifications:
#' Use [theory.test()] when you want to compare several theoretically motivated
#' condition sets under the same outcome, truth-table cutoffs, solution type,
#' exclusion handling, and settings for `which_M` and `i_mode`.
#'
#' @section Reporting QCA Solutions:
#' Use [sol.df()] when you want QCA minimization objects turned into a compact,
#' data frame. Use [sol.chart()] when you want that table
#' rendered as a visual chart of prime implicants.
#'
#' @section How To Read The Results:
#' Most qcaERT robustness functions return `diagnostics`, `results`, and
#' `settings`. `diagnostics` is the detailed table; `results` is the clean table
#' returned by `as.data.frame()`. `print()` shows a concise summary. The shared
#' output structure is described in `?qcaERT_conventions`.
#'
#' For [calib.test()], [incl.test()], and [theory.test()] objects, `plot()`
#' methods provide optional visual inspection helpers when `ggplot2` is
#' installed. [sol.chart()] provides the corresponding visual presentation for
#' [sol.df()] tables. See `?qcaERT_plots`.
#'
#' @examples
#' \donttest{
#' ?qcaERT_tests
#' ?qcaERT_conventions
#' ?qcaERT_plots
#' }
#'
#' @name qcaERT_tests
NULL
