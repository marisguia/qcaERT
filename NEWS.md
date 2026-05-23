# qcaERT 0.1.0

This is the first public version of qcaERT. This NEWS entry records the current
public interface.

## Package scope

- Added a family of robustness tools for QCA workflows:
  - `calib.test()` for calibration-threshold robustness.
  - `incl.test()` for inclusion-cutoff robustness.
  - `ncut.test()` for frequency-cutoff robustness.
  - `loo.test()` for leave-one-out case influence.
  - `subsample.test()` for repeated subsample stability.
  - `altset.test()` for sampled alternative analysis settings.
  - `theory.test()` for comparing theoretically motivated condition-set
    specifications.
  - `cluster.test()` for cluster, group, and repeated-unit heterogeneity.
  - `sol.df()` for compact QCA solution tables.
  - `sol.chart()` for visual presentation of `sol.df()` tables.

## Shared API and output conventions

- Standardized the function family around common solution controls:
  `solution`, `include`, `dir.exp`, `which_M`, and `i_mode`.
- Standardized exclusion handling across applicable functions with
  `exclude_mode`, `exclude_recompute`, and `exclude_static`.
- Standardized most result objects around:
  - `diagnostics` for detailed/internal results.
  - `results` for the clean table.
  - `settings` for the analysis settings.
  - supporting fields such as `baseline`, `bounds`, `by_direction`,
    `by_case`, `by_run`, `by_draw`, and `summary` where relevant.
- Standardized `print()` methods to show concise summaries.
- Standardized `as.data.frame()` methods to return the clean table.
- Documented the structured-result exceptions explicitly:
  - `cluster.test()` returns `results$overview`, `results$clusters`, and
    `results$units`; `as.data.frame()` returns `results$overview`.
  - `theory.test()` returns `results$models`, `results$solutions`, and
    `results$pairwise`; `as.data.frame()` returns `results$models`.
