tiny_qca <- data.frame(
  Y = c(1, 1, 0, 0),
  A = c(1, 1, 0, 0),
  B = c(1, 0, 1, 0)
)

qcaert_schema_raw <- function() {
  data.frame(
    A_raw = c(0, 5, 10, 25, 35, 50, 65, 75, 90, 100),
    B_raw = c(0, 100, 0, 100, 0, 100, 0, 100, 0, 100)
  )
}

qcaert_schema_calib3 <- function(raw = qcaert_schema_raw()) {
  A <- QCA::calibrate(raw$A_raw, type = "fuzzy", thresholds = c(20, 45, 75))
  data.frame(
    A = A,
    B = QCA::calibrate(raw$B_raw, type = "crisp", thresholds = 50),
    Y = as.integer(A > 0.5)
  )
}

qcaert_schema_calib6 <- function(raw = qcaert_schema_raw()) {
  A <- QCA::calibrate(
    raw$A_raw,
    type = "fuzzy",
    method = "direct",
    thresholds = c(10, 20, 30, 70, 80, 90),
    logistic = FALSE
  )

  data.frame(
    A = A,
    B = QCA::calibrate(raw$B_raw, type = "crisp", thresholds = 50),
    Y = as.integer(A > 0.5),
    cluster = rep(c("C1", "C2"), each = 5),
    unit = paste0("U", seq_len(nrow(raw)))
  )
}

qcaert_schema_indirect <- function(raw = qcaert_schema_raw()) {
  A <- QCA::calibrate(
    raw$A_raw,
    type = "fuzzy",
    method = "indirect",
    thresholds = c(25, 50, 75)
  )

  data.frame(
    A = A,
    B = QCA::calibrate(raw$B_raw, type = "crisp", thresholds = 50),
    Y = as.integer(A > 0.5)
  )
}

qcaert_schema_calib6_spec <- function() {
  list(
    A = list(
      raw = "A_raw",
      type = "fuzzy",
      method = "direct",
      thresholds = c(10, 20, 30, 70, 80, 90),
      calibrate = list(logistic = FALSE)
    ),
    B = list(
      raw = "B_raw",
      type = "crisp",
      thresholds = 50
    )
  )
}

qcaert_schema_indirect_spec <- function() {
  list(
    A = list(
      raw = "A_raw",
      type = "fuzzy",
      method = "indirect",
      thresholds = c(25, 50, 75)
    ),
    B = list(
      raw = "B_raw",
      type = "crisp",
      thresholds = 50
    )
  )
}

qcaert_truth_table <- function(data, outcome, conditions, incl.cut = 0.75, n.cut = 1, ...) {
  skip_if_not_installed("QCA")

  args <- c(
    list(
      data = data,
      outcome = outcome,
      conditions = conditions,
      incl.cut = incl.cut,
      n.cut = n.cut
    ),
    list(...)
  )

  suppressWarnings(do.call(QCA::truthTable, args))
}

qcaert_fixture_direct6 <- function() {
  skip_if_not_installed("QCA")

  raw <- qcaert_schema_raw()
  calib <- qcaert_schema_calib6(raw)

  list(
    raw = raw,
    calib = calib,
    outcome = "Y",
    conditions = c("A", "B"),
    test.conditions = "A",
    calib_spec = qcaert_schema_calib6_spec(),
    dir.exp = c("1", "1")
  )
}

qcaert_fixture_outcome_calibration <- function() {
  skip_if_not_installed("QCA")

  raw <- qcaert_schema_raw()
  raw$Y_raw <- raw$A_raw

  calib <- qcaert_schema_calib6(raw)
  calib$Y <- QCA::calibrate(
    raw$Y_raw,
    type = "fuzzy",
    method = "direct",
    thresholds = c(10, 20, 30, 70, 80, 90),
    logistic = FALSE
  )

  calib_spec <- qcaert_schema_calib6_spec()
  calib_spec$Y <- list(
    raw = "Y_raw",
    type = "fuzzy",
    method = "direct",
    thresholds = c(10, 20, 30, 70, 80, 90),
    calibrate = list(logistic = FALSE)
  )

  list(
    raw = raw,
    calib = calib,
    outcome = "Y",
    conditions = c("A", "B"),
    calib_spec = calib_spec,
    dir.exp = c("1", "1")
  )
}

qcaert_fixture_indirect <- function() {
  skip_if_not_installed("QCA")

  raw <- qcaert_schema_raw()
  calib <- qcaert_schema_indirect(raw)

  list(
    raw = raw,
    calib = calib,
    outcome = "Y",
    conditions = c("A", "B"),
    test.conditions = "A",
    calib_spec = qcaert_schema_indirect_spec(),
    dir.exp = c("1", "1")
  )
}

qcaert_fixture_cluster <- function() {
  skip_if_not_installed("QCA")

  direct <- qcaert_fixture_direct6()
  data <- direct$calib
  data$cluster <- rep(c("C1", "C2"), each = 5)
  data$unit <- rep(paste0("U", seq_len(5)), times = 2)

  tt <- qcaert_truth_table(
    data = data,
    outcome = direct$outcome,
    conditions = direct$conditions,
    incl.cut = 0.75,
    n.cut = 1
  )

  list(
    data = data,
    truth_table = tt,
    outcome = direct$outcome,
    conditions = direct$conditions,
    cluster_id = "cluster",
    unit_id = "unit",
    dir.exp = direct$dir.exp
  )
}

qcaert_fixture_lr <- function() {
  skip_if_not_installed("QCA")

  data_env <- new.env(parent = emptyenv())
  utils::data("LR", package = "QCA", envir = data_env)
  raw <- data_env$LR

  conditions <- c("DEV", "URB", "LIT", "IND", "STB")
  outcome <- "SURV"
  variables <- c(conditions, outcome)
  threshold_groups <- c(DEV = 7, URB = 4, LIT = 4, IND = 4, STB = 4, SURV = 4)
  thresholds <- lapply(variables, function(variable) {
    QCA::findTh(raw[[variable]], groups = threshold_groups[[variable]])
  })
  names(thresholds) <- variables

  calib <- raw
  for (variable in variables) {
    calib[[variable]] <- QCA::calibrate(
      raw[[variable]],
      type = "fuzzy",
      thresholds = thresholds[[variable]]
    )
  }

  calib_spec <- lapply(conditions, function(condition) {
    list(
      raw = condition,
      type = "fuzzy",
      method = "direct",
      thresholds = thresholds[[condition]]
    )
  })
  names(calib_spec) <- conditions

  list(
    raw = raw,
    calib = calib,
    thresholds = thresholds,
    calib_spec = calib_spec,
    outcome = outcome,
    conditions = conditions,
    test.conditions = conditions,
    dir.exp = rep("1", length(conditions))
  )
}

qcaert_fixture_lr_solutions <- function() {
  skip_if_not_installed("QCA")

  fixture <- qcaert_fixture_lr()
  tt <- qcaert_truth_table(
    data = fixture$calib,
    outcome = fixture$outcome,
    conditions = fixture$conditions,
    incl.cut = 0.8,
    n.cut = 1,
    complete = TRUE,
    show.cases = TRUE,
    sort.by = c("incl", "n")
  )
  enhanced <- QCA::findRows(tt, type = 2)

  list(
    fixture = fixture,
    truth_table = tt,
    conservative = QCA::minimize(
      tt,
      details = TRUE,
      show.cases = FALSE,
      use.tilde = FALSE
    ),
    intermediate = QCA::minimize(
      tt,
      details = TRUE,
      show.cases = FALSE,
      use.tilde = FALSE,
      dir.exp = fixture$dir.exp,
      include = "?",
      exclude = enhanced
    ),
    parsimonious = QCA::minimize(
      tt,
      details = TRUE,
      show.cases = FALSE,
      use.tilde = FALSE,
      include = "?"
    )
  )
}

expect_qcaert_schema <- function(obj, class, top, diagnostics, results, settings = NULL) {
  expect_identical(class(obj), class)
  expect_identical(names(obj), top)
  expect_identical(names(obj$diagnostics), diagnostics)

  if (is.data.frame(obj$results)) {
    expect_identical(names(obj$results), results)
  } else {
    expect_identical(names(obj$results), results)
  }

  expect_identical(names(as.data.frame(obj)), if (is.data.frame(obj$results)) results else names(obj$results$overview))

  if (!is.null(settings)) {
    expect_identical(names(obj$settings), settings)
  }
}
