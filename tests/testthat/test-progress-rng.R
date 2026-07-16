test_that("cosmetic progress message selection does not touch the R RNG", {
  set.seed(20260716)
  seed_before <- .Random.seed

  messages <- vapply(
    seq_len(20L),
    function(...) .qcaert_progress_message(),
    character(1)
  )

  expect_true(all(messages %in% .qcaert_progress_messages()))
  expect_identical(.Random.seed, seed_before)
})

test_that("creating and advancing interactive progress leaves RNG state unchanged", {
  local_mocked_bindings(
    .qcaert_is_interactive = function() TRUE,
    .package = "qcaERT"
  )

  set.seed(42)
  seed_before <- .Random.seed

  invisible(capture.output({
    progress_state <- .new_qcaert_progress(total = 2L, progress = TRUE)
    progress_state$tick()
    progress_state$set(2L)
    progress_state$close()
  }))

  expect_identical(.Random.seed, seed_before)
})

test_that("altset fixed-seed results do not depend on progress display", {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_direct6()
  args <- list(
    raw.data = fixture$raw,
    calib.data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    calib_spec = fixture$calib_spec,
    test.conditions = fixture$test.conditions,
    anchors_to_test = c("E1", "C1", "I1"),
    unit_step = 1,
    calib_max_steps = 1,
    incl.cut = 0.75,
    incl_step = 0.1,
    incl_max_steps = 1,
    n.cut = 1,
    ncut_step = 1,
    ncut_max_steps = 1,
    n_draws = 3,
    seed = 101,
    solution = "conservative"
  )

  without_progress <- qcaert_expect_no_warning(do.call(
    altset.test,
    c(args, list(progress = FALSE))
  ))

  local_mocked_bindings(
    .qcaert_is_interactive = function() TRUE,
    .package = "qcaERT"
  )

  with_progress <- NULL
  invisible(capture.output({
    with_progress <- qcaert_expect_no_warning(do.call(
      altset.test,
      c(args, list(progress = TRUE))
    ))
  }))

  expect_equal(with_progress, without_progress)
})

test_that("non-random diagnostics leave RNG untouched through progress", {
  skip_if_not_installed("QCA")

  local_mocked_bindings(
    .qcaert_is_interactive = function() TRUE,
    .package = "qcaERT"
  )

  fixture <- qcaert_fixture_direct6()
  set.seed(8675309)
  seed_before <- .Random.seed

  invisible(capture.output({
    qcaert_expect_no_warning(incl.test(
      data = fixture$calib,
      outcome = fixture$outcome,
      conditions = fixture$conditions,
      incl.cut = 0.75,
      step = 0.1,
      max_steps = 1,
      n.cut = 1,
      solution = "conservative",
      progress = TRUE
    ))
  }))

  expect_identical(.Random.seed, seed_before)
})
