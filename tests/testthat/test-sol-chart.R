sol_chart_fixture <- function(branches = "C1P1") {
  data.frame(
    Solution = c("Conservative", "Conservative", rep("Intermediate", length(branches)), "Parsimonious"),
    Model = 1,
    Intermediate_CnPn = c("-", "-", branches, "-"),
    Prime_Implicants = c("A*B*C", "A*B*~C", rep("A*B", length(branches)), "A"),
    Consistency_PI = c(0.96, 0.95, rep(0.97, length(branches)), 0.98),
    PRI_PI = c(0.91, 0.9, rep(0.92, length(branches)), 0.93),
    Raw_Coverage_PI = c(0.24, 0.23, rep(0.3, length(branches)), 0.4),
    Unique_Coverage_PI = c(0.12, 0.11, rep(0.2, length(branches)), 0.31),
    Solution_Consistency = c(0.97, 0.97, rep(0.98, length(branches)), 0.99),
    Solution_PRI = c(0.92, 0.92, rep(0.93, length(branches)), 0.94),
    Solution_Coverage = c(0.45, 0.45, rep(0.5, length(branches)), 0.53),
    Cases = c("case1, case2", "case3, case4", rep("case1, case2, case3", length(branches)), "case1, case2, case3, case4"),
    stringsAsFactors = FALSE
  )
}

test_that("sol.chart parses prime implicants into condition states", {
  parsed <- .sol_chart_parse_term("A*~B*C")

  expect_identical(
    parsed,
    c(A = "present", B = "absent", C = "present")
  )
})

test_that("sol.chart validates sol.df-style input", {
  expect_error(
    sol.chart(data.frame(Solution = "Conservative")),
    "Missing column\\(s\\):",
    fixed = FALSE
  )
})

test_that("sol.chart aligns detailed prime implicants with simpler prime implicants", {
  chart <- .sol_chart_prepare(sol_chart_fixture(), digits = 2)

  expect_length(chart$aligned_prime_implicants, 2L)
  expect_identical(unname(chart$aligned_prime_implicants[[1]][["conservative"]]), 1L)
  expect_identical(unname(chart$aligned_prime_implicants[[1]][["intermediate"]]), 3L)
  expect_identical(unname(chart$aligned_prime_implicants[[1]][["parsimonious"]]), 4L)
  expect_identical(unname(chart$aligned_prime_implicants[[2]][["conservative"]]), 2L)
  expect_identical(unname(chart$aligned_prime_implicants[[2]][["intermediate"]]), 3L)
  expect_identical(unname(chart$aligned_prime_implicants[[2]][["parsimonious"]]), 4L)
  expect_true(any(chart$labels$label == "PI 1"))

  expect_true(any(chart$states$state == "absent"))
  expect_true(any(chart$text$label == "0.96"))
  expect_true(any(grepl("case1", chart$plain_text$label, fixed = TRUE)))
})

test_that("sol.chart merges solution-fit body cells", {
  chart <- .sol_chart_prepare(sol_chart_fixture(), digits = 2)
  solution_rows <- chart$row_structure$row[chart$row_structure$kind == "solution_fit"]
  solution_grid <- chart$grid[chart$grid$row %in% solution_rows, , drop = FALSE]

  expect_identical(sum(solution_grid$row == solution_rows[[1L]]), 2L)
  expect_true(any(is.na(solution_grid$col)))
  expect_true(any(solution_grid$width == chart$n_prime_implicants))
})

test_that("sol.chart requires an intermediate branch when several are present", {
  x <- sol_chart_fixture(branches = c("C1P1", "C1P2"))

  expect_error(
    .sol_chart_prepare(x),
    "`intermediate_branch` must be supplied",
    fixed = TRUE
  )

  chart <- .sol_chart_prepare(x, intermediate_branch = "C1P2")
  expect_length(chart$aligned_prime_implicants, 2L)
})

test_that("sol.chart requires a model when several are present", {
  x <- rbind(
    sol_chart_fixture(),
    transform(sol_chart_fixture(), Model = 2L)
  )

  expect_error(
    .sol_chart_prepare(x),
    "`model` must be supplied when more than one model is present.",
    fixed = TRUE
  )

  chart <- .sol_chart_prepare(x, model = 2)
  expect_true(all(chart$terms$Model == 2L))
})

test_that("sol.chart returns a ggplot object", {
  skip_if_not_installed("ggplot2")

  p <- sol.chart(sol_chart_fixture(), legend = TRUE)
  expect_s3_class(p, "ggplot")
})
