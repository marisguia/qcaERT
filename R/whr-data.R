#' World Happiness Report 2025 demonstration data
#'
#' A country-level data set derived from the World Happiness Report
#' 2025 data for Figure 2.1. It is included to support qcaERT demonstrations
#' based on a real QCA workflow.
#'
#' @format A data frame with 145 countries and 6 columns:
#' \describe{
#'   \item{WELL}{Life evaluation, three-year average.}
#'   \item{GDP}{Contribution of log GDP per capita.}
#'   \item{SOC}{Contribution of social support.}
#'   \item{LIFE}{Contribution of healthy life expectancy.}
#'   \item{FREE}{Contribution of freedom to make life choices.}
#'   \item{GEN}{Contribution of generosity.}
#' }
#'
#' Country names are stored as row names. The explanatory-factor columns are
#' World Happiness Report decomposition components, not raw survey responses.
#'
#' @source World Happiness Report 2025, data for Figure 2.1, used with
#' permission. See <https://www.worldhappiness.report/data-sharing/>.
#'
#' Helliwell, J. F., Layard, R., Sachs, J. D., De Neve, J.-E., Aknin, L. B.,
#' & Wang, S. (Eds.). (2025). *World Happiness Report 2025*. University of
#' Oxford: Wellbeing Research Centre.
#'
#' @examples
#' data(whr_raw)
#' head(whr_raw)
#'
#' @keywords datasets
"whr_raw"

#' Calibrated World Happiness Report 2025 demonstration data
#'
#' A calibrated version of [whr_raw] for qcaERT demonstrations. It contains the
#' same countries and columns as [whr_raw], but the values are calibrated set
#' memberships.
#'
#' @format A data frame with 145 countries and 6 calibrated sets:
#' \describe{
#'   \item{WELL}{Calibrated outcome. Direct fuzzy calibration with thresholds
#'   2.181, 5.3555, and 7.0505.}
#'   \item{GDP}{Direct fuzzy calibration with six thresholds.}
#'   \item{SOC}{Indirect fuzzy calibration with six ordered cutpoints.}
#'   \item{LIFE}{Direct fuzzy calibration with three thresholds.}
#'   \item{FREE}{Crisp calibration with one threshold.}
#'   \item{GEN}{Direct fuzzy calibration with three thresholds.}
#' }
#'
#' The condition calibration specifications are stored in [whr_calib_spec].
#'
#' @source World Happiness Report 2025, data for Figure 2.1, used with
#' permission. See <https://www.worldhappiness.report/data-sharing/>.
#'
#' @examples
#' data(whr_calibrated)
#' head(whr_calibrated)
#'
#' @keywords datasets
"whr_calibrated"

#' Calibration specifications for the WHR demonstration data
#'
#' A named list of qcaERT calibration specifications for the five condition
#' sets in [whr_raw] and [whr_calibrated].
#'
#' @format A named list with entries for `GDP`, `SOC`, `LIFE`, `FREE`, and
#' `GEN`. Each entry contains:
#' \describe{
#'   \item{raw}{The column in [whr_raw] used for calibration.}
#'   \item{type}{The calibration type, either fuzzy or crisp.}
#'   \item{method}{The QCA calibration method.}
#'   \item{thresholds}{The thresholds or ordered cutpoints passed to
#'   [QCA::calibrate()].}
#' }
#'
#' This object covers the condition sets. If the outcome is also tested in
#' [calib.test()] or [altset.test()], add an entry for `WELL`.
#'
#' @source World Happiness Report 2025, data for Figure 2.1, used with
#' permission. See <https://www.worldhappiness.report/data-sharing/>.
#'
#' @examples
#' data(whr_calib_spec)
#' whr_calib_spec
#'
#' @keywords datasets
"whr_calib_spec"
