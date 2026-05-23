test_that("cluster solution = all follows family rules without dir.exp", {
  out <- qcaERT:::.resolve_solution_controls(
    solution = "all",
    include = NULL,
    dir.exp = NULL,
    caller = "cluster.test",
    style = "std"
  )

  expect_identical(out$solution, "all")
  expect_null(out$include)
  expect_identical(out$monitored, c("conservative", "parsimonious"))
})

test_that("cluster solution = all includes intermediate only when dir.exp is supplied", {
  out <- qcaERT:::.resolve_solution_controls(
    solution = "all",
    include = NULL,
    dir.exp = c(1, 1),
    caller = "cluster.test",
    style = "std"
  )

  expect_identical(out$solution, "all")
  expect_null(out$include)
  expect_identical(
    out$monitored,
    c("conservative", "parsimonious", "intermediate")
  )
})

test_that("cluster solution = all now rejects supplied include", {
  expect_error(
    qcaERT:::.resolve_solution_controls(
      solution = "all",
      include = "?",
      dir.exp = c(1, 1),
      caller = "cluster.test",
      style = "std"
    ),
    "When `solution = \"all\"`, do not supply `include`.",
    fixed = TRUE
  )
})

test_that("cluster solution = NULL is now rejected", {
  expect_error(
    qcaERT:::.resolve_solution_controls(
      solution = NULL,
      include = NULL,
      dir.exp = NULL,
      caller = "cluster.test",
      style = "std"
    ),
    "`solution` must be one of 'all', 'con'/'conservative', 'par'/'parsimonious', or 'int'/'intermediate'.",
    fixed = TRUE
  )
})

test_that("cluster.test now defaults to solution = all", {
  expect_identical(
    formals(cluster.test)$solution,
    "all"
  )
})
