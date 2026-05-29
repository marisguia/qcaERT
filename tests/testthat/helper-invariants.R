qcaert_regular_result_classes <- c(
  "altset_test",
  "calib_test",
  "incl_test",
  "loo_test",
  "ncut_test",
  "subsample_test"
)

qcaert_expect_no_warning <- function(expr) {
  expr <- substitute(expr)
  env <- parent.frame()
  out <- NULL

  expect_warning(out <- eval(expr, env), NA)
  out
}

qcaert_expect_warnings <- function(expr, regexp) {
  expr <- substitute(expr)
  env <- parent.frame()
  warnings <- character()

  out <- withCallingHandlers(
    eval(expr, env),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_true(
    length(warnings) > 0L,
    info = paste("Expected at least one warning matching:", regexp)
  )
  expect_true(
    all(grepl(regexp, warnings)),
    info = paste("Unexpected warnings:", paste(warnings, collapse = " | "))
  )
  out
}

qcaert_expect_clean_names <- function(names, label = "object") {
  expect_false(
    is.null(names),
    info = paste(label, "must have names")
  )
  expect_false(
    anyNA(names),
    info = paste(label, "must not have missing names")
  )
  expect_false(
    any(names == ""),
    info = paste(label, "must not have empty names")
  )
  expect_false(
    anyDuplicated(names) > 0L,
    info = paste(label, "must not have duplicated names")
  )
  expect_false(
    any(grepl("\\.[0-9]+$", names)),
    info = paste(label, "must not contain repaired duplicate names")
  )

  invisible(names)
}

qcaert_expect_table <- function(x, label = "table", required = NULL, exact = NULL, allow_null = FALSE) {
  if (allow_null && is.null(x)) {
    return(invisible(x))
  }

  expect_true(is.data.frame(x), info = paste(label, "must be a data frame"))
  qcaert_expect_clean_names(names(x), label)

  if (!is.null(required)) {
    missing <- setdiff(required, names(x))
    expect_true(
      length(missing) == 0L,
      info = paste(label, "is missing columns:", paste(missing, collapse = ", "))
    )
  }

  if (!is.null(exact)) {
    expect_identical(names(x), exact)
  }

  invisible(x)
}

qcaert_expect_print_ok <- function(x) {
  expect_error(capture.output(print(x)), NA)
  invisible(x)
}

qcaert_expect_settings <- function(settings, required = NULL) {
  expect_type(settings, "list")

  if (!is.null(names(settings))) {
    qcaert_expect_clean_names(names(settings), "settings")
  }

  if (!is.null(required)) {
    missing <- setdiff(required, names(settings))
    expect_true(
      length(missing) == 0L,
      info = paste("settings is missing fields:", paste(missing, collapse = ", "))
    )
  }

  invisible(settings)
}

expect_qcaert_result_structure <- function(
  x,
  class = NULL,
  diagnostics = NULL,
  results = NULL,
  settings = NULL,
  top = NULL,
  print = TRUE
) {
  if (!is.null(class)) {
    expect_s3_class(x, class)
    expect_false(
      identical(class, "cluster_test"),
      info = "Use expect_qcaert_cluster_structure() for cluster_test objects."
    )
  }

  expect_type(x, "list")
  qcaert_expect_clean_names(names(x), "result object")

  required_top <- unique(c("diagnostics", "results", top))
  missing_top <- setdiff(required_top, names(x))
  expect_true(
    length(missing_top) == 0L,
    info = paste("result object is missing components:", paste(missing_top, collapse = ", "))
  )

  qcaert_expect_table(x$diagnostics, "diagnostics", required = diagnostics)
  qcaert_expect_table(x$results, "results", required = results)
  expect_identical(as.data.frame(x), x$results)

  if ("settings" %in% names(x)) {
    qcaert_expect_settings(x$settings, required = settings)
  } else if (!is.null(settings)) {
    expect_true(FALSE, info = "result object is missing settings")
  }

  if (print) {
    qcaert_expect_print_ok(x)
  }

  invisible(x)
}

expect_qcaert_cluster_structure <- function(
  x,
  diagnostics = NULL,
  overview = NULL,
  clusters = NULL,
  units = NULL,
  settings = NULL,
  top = NULL,
  print = TRUE
) {
  expect_s3_class(x, "cluster_test")
  expect_type(x, "list")
  qcaert_expect_clean_names(names(x), "cluster result object")

  required_top <- unique(c("diagnostics", "results", top))
  missing_top <- setdiff(required_top, names(x))
  expect_true(
    length(missing_top) == 0L,
    info = paste("cluster result object is missing components:", paste(missing_top, collapse = ", "))
  )

  qcaert_expect_table(x$diagnostics, "diagnostics", required = diagnostics)

  expect_type(x$results, "list")
  expect_identical(names(x$results), c("overview", "clusters", "units"))
  qcaert_expect_table(x$results$overview, "results$overview", required = overview)
  qcaert_expect_table(x$results$clusters, "results$clusters", required = clusters, allow_null = TRUE)
  qcaert_expect_table(x$results$units, "results$units", required = units, allow_null = TRUE)
  expect_identical(as.data.frame(x), x$results$overview)

  if ("settings" %in% names(x)) {
    qcaert_expect_settings(x$settings, required = settings)
  } else if (!is.null(settings)) {
    expect_true(FALSE, info = "cluster result object is missing settings")
  }

  if (print) {
    qcaert_expect_print_ok(x)
  }

  invisible(x)
}
