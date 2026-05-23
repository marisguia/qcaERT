test_that("track has been removed from public function arguments", {
  expect_false("track" %in% names(formals(altset.test)))
  expect_false("track" %in% names(formals(cluster.test)))
  expect_false("track" %in% names(formals(incl.test)))
  expect_false("track" %in% names(formals(loo.test)))
  expect_false("track" %in% names(formals(ncut.test)))
  expect_false("track" %in% names(formals(sol.df)))
  expect_false("track" %in% names(formals(subsample.test)))
})

test_that("old track helpers are gone", {
  ns <- asNamespace("qcaERT")

  expect_false(exists(".normalize_track_i_mode", envir = ns, inherits = FALSE))
  expect_false(exists(".select_model_positions", envir = ns, inherits = FALSE))
})

test_that("no-op verbose has been removed from quiet function arguments", {
  quiet <- list(
    incl.test,
    ncut.test,
    calib.test,
    loo.test,
    subsample.test,
    theory.test
  )

  for (fn in quiet) {
    expect_false("verbose" %in% names(formals(fn)))
  }

  expect_true("verbose" %in% names(formals(altset.test)))
  expect_true("verbose" %in% names(formals(sol.df)))
})
