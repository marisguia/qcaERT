#' qcaERT: Enhanced robustness tests for QCA
#'
#' qcaERT extends the usual Qualitative Comparative Analysis (QCA) with tools
#' for assessing how stable and robust a QCA analysis is. It can test alternative
#' calibration thresholds, inclusion cutoffs, truth table frequency cutoffs,
#' case deletion, subsampling, alternative solutions, cluster-specific
#' analyses and different theoretical configurations. It also offers a compact way to organize and visualize QCA
#' solutions.
#' The package is built around the QCA package (Dusa 2019) workflow using
#' [QCA::calibrate()], [QCA::truthTable()], [QCA::minimize()], and
#' [QCA::findRows()]. It is not intended to cover every possible QCA variant
#' or calibration transformation.
#'
#' @section Start Here:
#' If you know the robustness concern but not the function name, start with
#' `?qcaERT_tests`, which maps qcaERT's tools.
#'
#' qcaERT includes three World Happiness Report demonstration objects. See
#' `?whr_raw`, `?whr_calibrated`, and `?whr_calib_spec`.
#'
#' A typical R workflow for sufficient analysis using qcaERT is:
#' \enumerate{
#'   \item calibrate data with [QCA::calibrate()],
#'   \item build a truth table with [QCA::truthTable()],
#'   \item minimize with [QCA::minimize()],
#'   \item run one or more qcaERT robustness tests, and
#'   \item inspect `print(x)`, `as.data.frame(x)`, `x$diagnostics`, and, where
#'   available, `plot(x)`.
#' }
#'
#' @section Function Family:
#' The main qcaERT functions are:
#' \itemize{
#'   \item [calib.test()] for calibration-threshold robustness.
#'   \item [incl.test()] for truth table inclusion-cutoff robustness.
#'   \item [ncut.test()] for truth table frequency-cutoff robustness.
#'   \item [loo.test()] for leave-one-out case influence.
#'   \item [subsample.test()] for repeated subsample stability (advanced).
#'   \item [altset.test()] for sampled alternative analysis settings that can
#'   combine calibration, inclusion-cutoff, and frequency-cutoff perturbations.
#'   \item [theory.test()] for comparing theoretically motivated configurations
#'   using the same outcome, truth-table cutoffs, solution type,
#'   exclusion handling, and model-selection settings.
#'   \item [cluster.test()] for cluster, group, or repeated-unit heterogeneity.
#'   \item [sol.df()] for converting QCA minimization objects into compact
#'   solution tables.
#'   \item [sol.chart()] for rendering `sol.df()` tables as solution charts.
#' }
#'
#' @section Returned Objects:
#' Most robustness functions return R objects with a common structure:
#' \itemize{
#'   \item `diagnostics`: the detailed table, useful for inspection and
#'   troubleshooting.
#'   \item `results`: a cleaner table.
#'   \item `settings`: the analysis settings used to create the result.
#'   \item supporting components such as `baseline`, `bounds`, `by_direction`,
#'   `by_case`, `by_run`, `by_draw`, or `summary`, depending on the test.
#' }
#'
#' `print()` gives a concise summary. `as.data.frame()` returns the main clean
#' table. `cluster.test()` and `theory.test()` use structured `results` lists:
#' `cluster.test()` returns `overview`, `clusters`, and `units`, while
#' `theory.test()` returns `models`, `pairwise`, and `solutions`.
#'
#' @section Common Conventions:
#' The family-wide argument and output conventions are described in
#' `?qcaERT_conventions`. It explains solution controls such as `solution`,
#' `include`, `dir.exp`, `which_M`, and `i_mode`; exclusion handling and its
#' function-specific exceptions; calibration specifications; and the common
#' returned-object structure.
#'
#' @section Plotting:
#' Plotting requires `ggplot2`. Current plot methods are
#' available for [calib.test()], [incl.test()], and [theory.test()] results.
#' [sol.chart()] provides a visual presentation for [sol.df()] tables. See
#' `?qcaERT_plots`.
#'
#' @section Learn More:
#' \itemize{
#'   \item `?qcaERT_tests` for choosing the right robustness test.
#'   \item `?qcaERT_conventions` for common argument and output conventions.
#'   \item `?qcaERT_plots` for plotting `calib_test`, `incl_test`, and
#'   `theory_test` objects, and for charting `sol.df()` tables.
#'   \item `vignette("qcaERT-overview", package = "qcaERT")` for a guided
#'   introduction.
#'   \item `vignette("qcaERT-result-objects", package = "qcaERT")` for the
#'   common returned-object structure.
#'   \item `vignette("qcaERT-calibration", package = "qcaERT")` for
#'   calibration specifications, scale-aware perturbations, and alternative
#'   sets.
#'   \item `news(package = "qcaERT")` for development changes.
#'   \item `citation(package = "qcaERT")` for citation information.
#' }
#'
#' @section Recommended citation:
#' Marisguia, B. A. H. (2026). qcaERT: Enhanced Robustness Tests for
#' Qualitative Comparative Analysis. R package version 0.1.1.
#' <https://CRAN.R-project.org/package=qcaERT>.
#' doi:10.32614/CRAN.package.qcaERT.
#'
#' Please also cite the QCA package and the methodological sources relevant
#' to the analysis.
#'
#' @references
#' Dusa, Adrian. 2019. *QCA with R. A Comprehensive Resource*. Cham:
#' Springer. <https://adriandusa.com/research/books/2019-QCA/>.
#'
#' Ragin, C. C. 2014. *The Comparative Method: Moving Beyond Qualitative and
#' Quantitative Strategies*. Oakland, California: University of California
#' Press.
#'
#' @author Breno A. H. Marisguia

#' @aliases qcaERT-package
#' @importFrom stats setNames xtabs
"_PACKAGE"

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("component"))
}
