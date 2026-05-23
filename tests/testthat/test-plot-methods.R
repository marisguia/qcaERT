test_that("incl_test plot methods return ggplot objects", {
  skip_if_not_installed("QCA")
  skip_if_not_installed("ggplot2")

  fixture <- qcaert_fixture_direct6()
  out <- suppressWarnings(incl.test(
    data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    incl.cut = 0.75,
    step = 0.1,
    max_steps = 1,
    n.cut = 1,
    solution = "all",
    dir.exp = fixture$dir.exp,
    progress = FALSE
  ))

  expect_error(
    plot(out),
    "`solution_type` must be supplied when more than one solution type is present.",
    fixed = TRUE
  )
  expect_error(
    plot(out, solution = "conservative"),
    "Use `solution_type`",
    fixed = TRUE
  )
  expect_error(
    plot(out, solution_type = "all"),
    "`solution_type` must use",
    fixed = TRUE
  )
  expect_s3_class(plot(out, solution_type = "conservative"), "ggplot")
  expect_s3_class(plot(out, solution_type = "conservative", type = "trace", direction = "lower"), "ggplot")
  expect_error(
    plot(out, solution_type = "all"),
    "`solution_type` must use",
    fixed = TRUE
  )
})

test_that("calib_test plot methods return ggplot objects for direct-six anchors", {
  skip_if_not_installed("QCA")
  skip_if_not_installed("ggplot2")

  fixture <- qcaert_fixture_direct6()
  out <- suppressWarnings(calib.test(
    raw.data = fixture$raw,
    calib.data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    calib_spec = fixture$calib_spec,
    test.conditions = fixture$test.conditions,
    unit_step = 1,
    max_steps = 1,
    incl.cut = 0.75,
    n.cut = 1,
    solution = "all",
    dir.exp = fixture$dir.exp,
    progress = FALSE
  ))

  expect_error(
    plot(out),
    "`solution_type` must be supplied when more than one solution type is present.",
    fixed = TRUE
  )
  expect_s3_class(plot(out, solution_type = "conservative"), "ggplot")
  expect_s3_class(plot(out, solution_type = "conservative", type = "heatmap"), "ggplot")
  expect_s3_class(
    plot(out, solution_type = "conservative", type = "trace", set = "A", anchor = "E1", direction = "lower"),
    "ggplot"
  )
  expect_error(
    plot(out, solution = "all"),
    "Use `solution_type`",
    fixed = TRUE
  )
})

test_that("calib_test plot methods return ggplot objects for indirect anchors", {
  skip_if_not_installed("QCA")
  skip_if_not_installed("ggplot2")

  fixture <- qcaert_fixture_indirect()
  out <- suppressWarnings(calib.test(
    raw.data = fixture$raw,
    calib.data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    calib_spec = fixture$calib_spec,
    test.conditions = fixture$test.conditions,
    unit_step = 1,
    max_steps = 1,
    incl.cut = 0.75,
    n.cut = 1,
    solution = "conservative",
    progress = FALSE
  ))

  expect_s3_class(plot(out), "ggplot")
  expect_s3_class(plot(out, type = "heatmap", cell = "anchor"), "ggplot")
  expect_s3_class(
    plot(out, type = "trace", set = "A", anchor = "T1", direction = "upper"),
    "ggplot"
  )
})

test_that("theory_test plot selects one solution_type and returns a ggplot object", {
  skip_if_not_installed("QCA")
  skip_if_not_installed("ggplot2")

  fixture <- qcaert_fixture_lr()
  theories <- list(
    development = c("DEV", "URB", "LIT"),
    industrial = c("DEV", "URB", "IND"),
    broad = c("DEV", "URB", "LIT", "IND", "STB")
  )
  dir_exp <- list(
    development = c("1", "1", "1"),
    industrial = c("1", "1", "1"),
    broad = c("1", "1", "1", "1", "1")
  )

  out <- theory.test(
    data = fixture$calib,
    outcome = fixture$outcome,
    theories = theories,
    incl.cut = 0.8,
    n.cut = 1,
    solution = "all",
    dir.exp = dir_exp,
    progress = FALSE
  )

  expect_error(
    plot(out),
    "`solution_type` must be supplied when more than one solution type is present.",
    fixed = TRUE
  )
  expect_s3_class(plot(out, solution_type = "conservative"), "ggplot")
  expect_s3_class(plot(out, solution_type = "conservative", label_line = FALSE), "ggplot")
  expect_s3_class(plot(out, solution_type = "par"), "ggplot")
  expect_s3_class(plot(out, solution_type = "intermediate"), "ggplot")
  expect_error(
    plot(out, solution_type = "conservative", intermediate_branch = "C1P1"),
    "`intermediate_branch` is only used when solution_type = 'intermediate'.",
    fixed = TRUE
  )
})
