.empty_change_trace <- function(value_col, value_type = c("numeric", "integer")) {
  value_type <- match.arg(value_type)

  value_vec <- switch(
    value_type,
    numeric = numeric(0),
    integer = integer(0)
  )

  out <- data.frame(
    step = integer(0),
    changed = logical(0),
    status = character(0),
    change_kind = character(0),
    stringsAsFactors = FALSE
  )

  out[[value_col]] <- value_vec
  out <- out[, c("step", value_col, "changed", "status", "change_kind"), drop = FALSE]
  out
}

.append_change_trace <- function(trace, step, value_col, value, changed, status, change_kind = NA_character_) {
  row <- data.frame(
    step = step,
    changed = changed,
    status = status,
    change_kind = change_kind,
    stringsAsFactors = FALSE
  )

  row[[value_col]] <- value
  row <- row[, c("step", value_col, "changed", "status", "change_kind"), drop = FALSE]

  rbind(trace, row)
}

.empty_same_trace <- function() {
  data.frame(
    step = integer(0),
    value = numeric(0),
    same = logical(0),
    stringsAsFactors = FALSE
  )
}

.append_same_trace <- function(trace, step, value, same) {
  rbind(
    trace,
    data.frame(
      step = step,
      value = value,
      same = same,
      stringsAsFactors = FALSE
    )
  )
}
