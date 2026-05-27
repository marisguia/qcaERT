# qcaERT

 qcaERT provides enhanced robustness tests for Qualitative Comparative Analysis (QCA). It is designed for the moment after calibration, truth table construction, and minimization, when the question becomes:

> How much does this QCA solution depend on my thresholds, cutoffs, cases, sample, or grouping structure?

Building on the workflow supported by the [QCA package](https://CRAN.R-project.org/package=QCA) (Dusa, 2019), qcaERT treats robustness evaluation as an auditable process through a series of comprehensive diagnostics.

## What qcaERT helps with

| Concern | Use |
| --- | --- |
| Calibration thresholds may be fragile | `calib.test()` |
| Inclusion cutoff may be fragile | `incl.test()` |
| Frequency cutoff may be fragile | `ncut.test()` |
| Individual cases may drive the solution | `loo.test()` |
| Sample composition may matter | `subsample.test()` |
| Several analytic choices may vary together | `altset.test()` |
| Theoretical condition-set specifications should be compared | `theory.test()` |
| Results may differ across groups or clusters | `cluster.test()` |
| QCA minimization output needs a clean table or chart | `sol.df()`, `sol.chart()` |

Inside R, the same map is available with:

```r
?qcaERT_tests
```

## Installation

qcaERT can be installed directly from CRAN:

```r
install.packages("qcaERT")
```

It can also be installed from GitHub with:

```r
install.packages("remotes")
remotes::install_github("marisguia/qcaERT")
```

Then load it normally:

```r
library(qcaERT)
library(QCA)
```

Plotting is optional and uses `ggplot2`:

```r
install.packages("ggplot2")
```

## A compact LR workflow

This example uses the `LR` data from the `QCA` package.

```r
library(QCA)
library(qcaERT)

data(LR)

conditions <- c("DEV", "URB", "LIT", "IND", "STB")
outcome <- "SURV"
dir_exp <- rep("1", length(conditions))

thresholds <- list(
  DEV = findTh(LR$DEV, groups = 7),
  URB = findTh(LR$URB, groups = 4),
  LIT = findTh(LR$LIT, groups = 4),
  IND = findTh(LR$IND, groups = 4),
  STB = findTh(LR$STB, groups = 4),
  SURV = findTh(LR$SURV, groups = 4)
)

dat <- LR
dat$DEV <- calibrate(LR$DEV, type = "fuzzy", thresholds = thresholds$DEV)
dat$URB <- calibrate(LR$URB, type = "fuzzy", thresholds = thresholds$URB)
dat$LIT <- calibrate(LR$LIT, type = "fuzzy", thresholds = thresholds$LIT)
dat$IND <- calibrate(LR$IND, type = "fuzzy", thresholds = thresholds$IND)
dat$STB <- calibrate(LR$STB, type = "fuzzy", thresholds = thresholds$STB)
dat$SURV <- calibrate(LR$SURV, type = "fuzzy", thresholds = thresholds$SURV)
```

Run a regular QCA analysis:

```r
tt <- truthTable(
  dat,
  outcome = outcome,
  conditions = conditions,
  incl.cut = 0.8,
  n.cut = 1,
  complete = TRUE,
  show.cases = TRUE
)

sol <- minimize(tt, include = "", details = TRUE, show.cases = FALSE)
solution_table <- sol.df(conservative = sol, solution = "conservative")
solution_table
sol.chart(solution_table)
```

Then check robustness.

```r
incl_out <- incl.test(
  data = dat,
  outcome = outcome,
  conditions = conditions,
  incl.cut = 0.8,
  step = 0.05,
  max_steps = 4,
  n.cut = 1,
  solution = "all",
  dir.exp = dir_exp,
  progress = TRUE
)

incl_out
as.data.frame(incl_out)
incl_out$diagnostics
```

Compare theoretically motivated condition sets under the same analytic
settings:

```r
theories <- list(
  development = c("DEV", "URB", "LIT"),
  industrial = c("DEV", "URB", "IND"),
  broad = c("DEV", "URB", "LIT", "IND", "STB")
)

dir_exp_theories <- list(
  development = c("1", "1", "1"),
  industrial = c("1", "1", "1"),
  broad = c("1", "1", "1", "1", "1")
)

theory_out <- theory.test(
  data = dat,
  outcome = outcome,
  theories = theories,
  incl.cut = 0.8,
  n.cut = 1,
  solution = "all",
  dir.exp = dir_exp_theories,
  progress = TRUE
)

theory_out
as.data.frame(theory_out)
theory_out$results$solutions
theory_out$results$pairwise
```

For calibration robustness, define `calib_spec` and let qcaERT compute
scale-aware perturbation steps. `calib_spec` records the raw column,
calibration type, method, thresholds, and any extra `QCA::calibrate()`
arguments in one place.

```r
calib_spec <- list(
  DEV = list(raw = "DEV", type = "fuzzy", method = "direct", thresholds = thresholds$DEV),
  URB = list(raw = "URB", type = "fuzzy", method = "direct", thresholds = thresholds$URB),
  LIT = list(raw = "LIT", type = "fuzzy", method = "direct", thresholds = thresholds$LIT),
  IND = list(raw = "IND", type = "fuzzy", method = "direct", thresholds = thresholds$IND),
  STB = list(raw = "STB", type = "fuzzy", method = "direct", thresholds = thresholds$STB)
)

calib_spec_outcome <- calib_spec
calib_spec_outcome$SURV <- list(
  raw = "SURV",
  type = "fuzzy",
  method = "direct",
  thresholds = thresholds$SURV
)

calib_out <- calib.test(
  raw.data = LR,
  calib.data = dat,
  outcome = outcome,
  conditions = conditions,
  calib_spec = calib_spec,
  test.conditions = c("DEV", "URB"),
  unit_step = NULL,
  unit_step_divisor = 10,
  max_steps = 5,
  incl.cut = 0.8,
  n.cut = 1,
  solution = "all",
  dir.exp = dir_exp,
  progress = TRUE
)

calib_out
as.data.frame(calib_out)
calib_out$bounds
```

Here, `conditions` defines the full QCA model, while `test.conditions` selects
which calibrated conditions are perturbed. If `test.conditions` is omitted, all
model conditions are tested.

To test the outcome calibration, keep the outcome out of `conditions` and ask
for it explicitly:

```r
calib_outcome <- calib.test(
  raw.data = LR,
  calib.data = dat,
  outcome = outcome,
  conditions = conditions,
  calib_spec = calib_spec_outcome,
  test.conditions = NULL,
  test.outcome = TRUE,
  unit_step = NULL,
  unit_step_divisor = 10,
  max_steps = 5,
  incl.cut = 0.8,
  n.cut = 1,
  solution = "all",
  dir.exp = dir_exp,
  progress = TRUE
)
```

## Reading qcaERT results

Most qcaERT robustness functions return an S3 object with:

- `diagnostics`: detailed/internal results
- `results`: clean results
- `settings`: the settings used to run the analysis
- supporting components such as `baseline`, `bounds`, `by_direction`,
  `by_case`, `by_run`, `by_draw`, or `summary`

Use:

```r
print(incl_out)
as.data.frame(incl_out)
incl_out$diagnostics
```

For `incl.test()`, `ncut.test()`, and `calib.test()`, `result_shape` controls
the layout of the clean table when `solution = "all"`. The default `"wide"`
layout keeps one row per tested path with solution-type-specific columns;
`"long"` returns one row per tested path and solution type, with a
`solution_type` column.

`cluster.test()` and `theory.test()` are the deliberate structured-result
exceptions. `cluster.test()` contains three tables:

- `overview`
- `clusters`
- `units`

For `cluster_test` objects, `as.data.frame()` returns `results$overview`.

`theory.test()` contains:

- `models`
- `solutions`
- `pairwise`

For `theory_test` objects, `as.data.frame()` returns `results$models`.

## Plotting

If `ggplot2` is installed, `calib.test()`, `incl.test()`, and `theory.test()`
results can be plotted directly.

```r
plot(incl_out, solution_type = "conservative")
plot(incl_out, solution_type = "conservative", type = "trace", direction = "lower")

plot(calib_out, solution_type = "conservative")
plot(calib_out, solution_type = "conservative", type = "heatmap")
plot(calib_out, solution_type = "conservative", type = "trace", set = "DEV", anchor = "E1", direction = "lower")

plot(theory_out, solution_type = "conservative")
```

For direct six-threshold fuzzy calibration, anchors are `E1`, `C1`, `I1`,
`I2`, `C2`, and `E2`. For indirect calibration, anchors are `T1`, `T2`, and so
on.

See:

```r
?qcaERT_plots
```

## Learn more

```r
?qcaERT
?qcaERT_tests
?qcaERT_conventions
?qcaERT_plots
vignette("qcaERT-overview", package = "qcaERT")
vignette("qcaERT-result-objects", package = "qcaERT")
vignette("qcaERT-calibration", package = "qcaERT")
news(package = "qcaERT")
citation(package = "qcaERT")
```

## Status

qcaERT is release-ready.
