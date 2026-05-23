qcaert_truthy_env <- function(name) {
  tolower(Sys.getenv(name, unset = "false")) %in% c("1", "true", "yes", "on")
}

qcaert_stress_enabled <- function() {
  qcaert_truthy_env("QCAERT_STRESS")
}

skip_qcaert_stress <- function() {
  skip_if_not(
    qcaert_stress_enabled(),
    "Set QCAERT_STRESS=true to run optional stress tests."
  )
}
