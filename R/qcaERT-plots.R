#' Plot and chart qcaERT results
#'
#' Plot methods provide visual inspection helpers for qcaERT result objects.
#' They are simplified views of the existing result object: the
#' detailed `diagnostics` table supplies the interval and heatmap views, while
#' the path-level trace components supply trace views. [sol.chart()] provides a
#' matching visual display for [sol.df()] solution tables.
#'
#' Plotting is optional. The qcaERT analysis functions do not require
#' `ggplot2`, but these methods do.
#'
#' @param x An `incl_test` object returned by [incl.test()], a `calib_test`
#'   object returned by [calib.test()], or a `theory_test` object returned by
#'   [theory.test()].
#' @param type Plot type. `incl_test` supports `"interval"` and `"trace"`.
#'   `calib_test` supports `"interval"`, `"heatmap"`, and `"trace"`.
#'   `theory_test` has one consistency-coverage plot and does not use `type`.
#' @param solution_type Solution type to plot. Required when more than one
#'   solution type is present. Accepted values follow the common qcaERT
#'   solution-type conventions, excluding `"all"`.
#' @param intermediate_branch For `theory_test` intermediate plots, the
#'   intermediate branch to plot when more than one branch is present.
#' @param directions Character vector selecting lower and/or upper searches.
#' @param stop_reason Optional character vector used to filter diagnostic rows
#'   by stop reason before plotting.
#' @param changed_types Optional solution-type filter applied to comma-coded
#'   `changed_types` diagnostic values. Accepted tokens follow the common
#'   qcaERT solution-type conventions: `"con"`/`"conservative"`,
#'   `"par"`/`"parsimonious"`, and `"int"`/`"intermediate"`.
#' @param solution Reserved. Plot methods do not accept `solution`; use
#'   `solution_type`.
#' @param monitored_solutions Reserved. Plot methods do not accept
#'   `monitored_solutions`; use `solution_type`.
#' @param i_mode For `incl_test`, optional filter applied to the `i_mode`
#'   diagnostic column. Accepted values are `"all"` and `"C1P1"`.
#' @param direction For `type = "trace"`, the single search direction to plot:
#'   `"lower"` or `"upper"`.
#' @param show_stop Logical. If `TRUE`, draw reference lines for baseline and,
#'   when present, first-failing values.
#' @param legend Logical. If `FALSE`, hide the plot legend.
#' @param theme A ggplot2 theme object added to the plot.
#' @param sets For `calib_test`, optional character vector selecting calibrated
#'   sets.
#' @param roles For `calib_test`, optional character vector selecting set
#'   roles. Supported values are `"condition"` and `"outcome"`.
#' @param anchors For `calib_test`, optional character vector selecting
#'   calibration anchors. Supported anchors are taken from the result object and
#'   may include crisp (`"T"`), fuzzy direct three-threshold (`"E"`, `"C"`,
#'   `"I"`), fuzzy direct six-threshold (`"E1"`, `"C1"`, `"I1"`, `"I2"`,
#'   `"C2"`, `"E2"`), or fuzzy indirect (`"T1"`, `"T2"`, ...) anchors.
#' @param metric For `calib_test` heatmaps, the quantity used for the fill
#'   color: `"raw"`, `"pct"`, or `"steps"`.
#' @param value For `calib_test` heatmaps with `metric = "raw"`, the raw
#'   threshold value to show: `"delta"`, `"last_safe"`, or `"failing"`.
#' @param abs_delta Logical. If `TRUE`, heatmaps using raw or percentage deltas
#'   display absolute values.
#' @param cell For `calib_test` heatmaps, whether cells are split by
#'   anchor-direction path (`"anchor_direction"`) or by anchor only
#'   (`"anchor"`).
#' @param show_text Logical. If `TRUE`, add rounded heatmap values as text.
#' @param show_labels For `theory_test`, logical. If `TRUE`, print theory names
#'   next to their consistency/coverage points.
#' @param label_line For `theory_test`, logical. If `TRUE`, draw a line from
#'   each consistency/coverage point to its theory-name label.
#' @param label_line_alpha For `theory_test`, transparency of label connector
#'   lines.
#' @param label_line_width For `theory_test`, width of label connector lines.
#' @param point_size For `theory_test`, point size.
#' @param text_size For `theory_test`, theory-name label size.
#' @param label_nudge_x For `theory_test`, horizontal nudge for theory-name
#'   labels.
#' @param label_nudge_y For `theory_test`, vertical nudge for theory-name
#'   labels.
#' @param set For `calib_test` trace plots, the single calibrated set to plot.
#' @param anchor For `calib_test` trace plots, the single calibration anchor to
#'   plot.
#' @param order_sets For `calib_test`, set ordering in interval and heatmap
#'   views. `"input"` preserves result order; `"most_sensitive"` and
#'   `"least_sensitive"` order by the smallest solution-changing percentage
#'   delta when available.
#' @param ... Additional graphical arguments forwarded to the primary ggplot2
#'   geometry.
#'
#' @returns A ggplot object.
#'
#' @section Plot Types:
#' For `incl_test`, `"interval"` shows the baseline inclusion cutoff and the
#' lower/upper last-safe values. `"trace"` shows the stepwise path for one
#' search direction.
#'
#' For `calib_test`, `"interval"` shows baseline and lower/upper last-safe
#' threshold values by set and anchor. `"heatmap"` summarizes sensitivity
#' across anchor paths. `"trace"` shows the stepwise path for one set,
#' anchor, and direction.
#'
#' For `theory_test`, the plot compares theory-specific selected models in
#' consistency/coverage space for one selected solution type. Theory names are shown
#' near the points, and the legend pairs each theory with its selected solution
#' terms.
#'
#' For [sol.df()] tables, [sol.chart()] draws a table-like chart of sufficient
#' configurations, using filled and open circles for condition presence and
#' absence.
#'
#' @section Family Consistency:
#' These methods follow the common qcaERT output conventions described in
#' `?qcaERT_conventions`: plots read from `diagnostics` and path-level supporting
#' components, while `print()` and `as.data.frame()` keep their existing
#' concise and tabular behavior. [sol.chart()] follows the same division of
#' labor by reading from the already-extracted [sol.df()] table.
#'
#' @examples
#' \donttest{
#' library(QCA)
#' library(ggplot2)
#'
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
#' incl_out <- incl.test(
#'   data = dat,
#'   outcome = outcome,
#'   conditions = conditions,
#'   incl.cut = 0.8,
#'   step = 0.05,
#'   max_steps = 5,
#'   n.cut = 1,
#'   solution = "all",
#'   dir.exp = dir_exp,
#'   progress = TRUE
#' )
#'
#' calib_spec <- list(
#'   DEV = list(raw = "DEV", type = "fuzzy", method = "direct", thresholds = thresholds$DEV),
#'   URB = list(raw = "URB", type = "fuzzy", method = "direct", thresholds = thresholds$URB),
#'   LIT = list(raw = "LIT", type = "fuzzy", method = "direct", thresholds = thresholds$LIT),
#'   IND = list(raw = "IND", type = "fuzzy", method = "direct", thresholds = thresholds$IND),
#'   STB = list(raw = "STB", type = "fuzzy", method = "direct", thresholds = thresholds$STB)
#' )
#'
#' calib_out <- calib.test(
#'   raw.data = LR,
#'   calib.data = dat,
#'   outcome = outcome,
#'   conditions = conditions,
#'   calib_spec = calib_spec,
#'   test.conditions = c("DEV", "URB"),
#'   unit_step = NULL,
#'   unit_step_divisor = 10,
#'   max_steps = 5,
#'   incl.cut = 0.8,
#'   n.cut = 1,
#'   solution = "all",
#'   dir.exp = dir_exp,
#'   progress = TRUE
#' )
#'
#' theories <- list(
#'   development = c("DEV", "URB", "LIT"),
#'   industrial = c("DEV", "URB", "IND"),
#'   broad = c("DEV", "URB", "LIT", "IND", "STB")
#' )
#'
#' theory_out <- theory.test(
#'   data = dat,
#'   outcome = outcome,
#'   theories = theories,
#'   incl.cut = 0.8,
#'   n.cut = 1,
#'   solution = "all",
#'   dir.exp = list(
#'     development = c("1", "1", "1"),
#'     industrial = c("1", "1", "1"),
#'     broad = c("1", "1", "1", "1", "1")
#'   ),
#'   progress = TRUE
#' )
#'
#' plot(incl_out, solution_type = "conservative")
#' plot(incl_out, solution_type = "conservative", type = "trace", direction = "lower")
#' plot(calib_out, solution_type = "conservative")
#' plot(calib_out, solution_type = "conservative", type = "heatmap")
#' plot(
#'   calib_out,
#'   solution_type = "conservative",
#'   type = "trace",
#'   set = "DEV",
#'   anchor = "E1",
#'   direction = "lower"
#' )
#' plot(theory_out, solution_type = "conservative")
#' }
#'
#' @name qcaERT_plots
NULL
