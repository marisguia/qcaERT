# qcaERT (Unreleased)

- Stratified draws in `subsample.test()` now use the actual case from a
  one-case stratum. When the frequency-cutoff grid in `altset.test()` contains
  a single value, every draw uses that value.
- `loo.test()` and `subsample.test()` now retrieve fit statistics for the
  model selected by `which_M` when QCA returns multiple models. When fit
  comparison is requested and no measures can be compared, `fit_changed` and
  `n_fit_deltas` are `NA`. A completed comparison with no differences beyond
  `fit_tol` reports `FALSE` and `0`, respectively.
- Tests that build truth tables now apply `neg.out = TRUE` to a single
  outcome during truth-table construction. Supply either `neg.out = TRUE`
  or a leading `~` in the outcome. The tests also reject using the outcome
  set as a condition.
- `subsample.test()` reports `exact_match_baseline = TRUE` when all requested
  solution types match. An observed difference gives `FALSE`. An incomplete
  comparison with no observed difference gives `NA`.
- When recalibration fails at a proposed threshold, `calib.test()` records
  `calibration_error` for that search path and preserves the last safe
  threshold, the failed threshold, and QCA's error message.
- With `unit_step = NULL`, `calib.test()` and `altset.test()` calculate step
  sizes for the sets selected by `test.conditions` and `test.outcome`.
- `incl.test()` now evaluates `0` or `1` when a step lands there within
  floating-point precision. With `show_stop = TRUE`, calibration trace plots
  label a rejected threshold as infeasible on a reference line.
- `sol.chart()` keeps rows without a model number alongside those for the
  selected numbered model.

# qcaERT 0.1.2

- `incl.test()` and `ncut.test()` now leave `first_failing` missing when the
  next proposed cutoff lies outside the admissible search range, because that
  value was not analyzed. Inclusion-cutoff trace plots likewise omit untested
  boundary proposals and their reference lines.
- `ncut.test()` now rejects a baseline `n.cut` greater than the number of rows
  in the analysis data.
- `altset.test()` now verifies that every tested condition or outcome
  membership in `calib.data` matches the reference calibration reconstructed
  from `raw.data` and `calib_spec`.
- Corrected alternative-set fit reporting so `n_fit_deltas = 0` identifies a
  completed comparison with no differences beyond `fit_tol`, whereas `NA`
  identifies a draw in which fit was not compared. Printed summaries now show
  the separate denominators for draw availability, formula preservation, and
  fit preservation.
- Removed the unused `necessity` argument from `cluster.test()`. The function
  now documents its scope explicitly as the sufficiency assessment of selected
  solution formulas and prime implicants, and its displayed `configuration`
  values no longer include a redundant solution-type prefix.
- Added centralized row-alignment checks for paired raw, calibrated, reduced-run,
  and truth-table inputs. Identifiable case labels must now be unique and appear
  in the same order; otherwise pairing remains explicitly positional.
- Corrected `cluster.test()` pooled, cluster-specific, and within-unit
  calculations. Missing unit identifiers no longer remove rows from pooled or
  cluster fit, and duplicate unit-cluster observations are rejected.
- Isolated cosmetic progress-message selection from R's analytical random-number
  stream, so progress display cannot change seeded results.
- Standardized validation of integer-valued controls, including search budgets,
  sample sizes, replications, seeds, and display digits. Intentional
  `sample_prop` rounding is unchanged.
- Declared `R (>= 3.5.0)`, matching the serialization format used by the shipped
  demonstration data.

# qcaERT 0.1.1

- Added the World Happiness Report demonstration objects `whr_raw`, `whr_calibrated`, and `whr_calib_spec`.
- Fixed calibration trace plots so `plot(..., type = "trace")` no longer duplicates an evaluated failing threshold.

# qcaERT 0.1.0

This is the first public version of qcaERT.

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
