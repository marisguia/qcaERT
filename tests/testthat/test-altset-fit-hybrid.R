test_that("hybrid fit comparison uses all matched solution names", {
  base_map <- list(
    "M1:A*B + C" = c(inclS = 0.84, PRI = 0.79, covS = 0.52),
    "M2:A + ~B" = c(inclS = 0.81, PRI = 0.75, covS = 0.49)
  )

  cur_map <- list(
    "M1:A*B + C" = c(inclS = 0.85, PRI = 0.79, covS = 0.50),
    "M2:A + ~B" = c(inclS = 0.84, PRI = 0.76, covS = 0.48)
  )

  cmp <- qcaERT:::.compare_fit_maps_hybrid(
    base_map = base_map,
    cur_map = cur_map,
    solution_type = "conservative",
    tol = 0.02
  )

  expect_true(cmp$fit_compared)
  expect_false(cmp$same_fit)

  expect_identical(
    cmp$matched_keys,
    c("M1:A*B + C", "M2:A + ~B")
  )

  expect_equal(cmp$max_abs_delta, 0.03, tolerance = 1e-12)

  expect_setequal(
    names(cmp$details),
    c("M1:A*B + C", "M2:A + ~B")
  )

  expect_identical(
    cmp$changed_names,
    "conservative::M2:A + ~B::inclS"
  )

  expect_equal(
    unname(cmp$details[["M1:A*B + C"]][["deltas"]]["covS"]),
    -0.02,
    tolerance = 1e-12
  )

  expect_equal(
    unname(cmp$details[["M2:A + ~B"]][["deltas"]]["inclS"]),
    0.03,
    tolerance = 1e-12
  )
})

test_that("hybrid fit comparison returns not compared when no solution names match", {
  base_map <- list(
    "M1:A*B + C" = c(inclS = 0.84, PRI = 0.79)
  )

  cur_map <- list(
    "M2:A + ~B" = c(inclS = 0.84, PRI = 0.79)
  )

  cmp <- qcaERT:::.compare_fit_maps_hybrid(
    base_map = base_map,
    cur_map = cur_map,
    solution_type = "conservative",
    tol = 0.02
  )

  expect_false(cmp$fit_compared)
  expect_true(is.na(cmp$same_fit))
  expect_identical(cmp$matched_keys, character(0))
  expect_null(cmp$details)
})
