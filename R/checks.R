new_check <- function(fn, scope, ...) {
  stopifnot(is.function(fn))
  structure(fn, class = 'cf_check', scope = scope, ...)
}

get_check_scope <- function(check) {
  attributes(check)$scope
}

new_check_result <- function(result, msg) {
  stopifnot(is.logical(result), length(result) == 1)
  if (result) {
    msg <- ""
  }
  structure(result, class = 'check_result', msg = msg)
}


#' @export
format.check_result <- function(x, ...) {
  if (x) {
    return(cli::cli_alert_success("Passed check"))
  }
  cli::cli({
    cli::cli_alert_danger("Failed Check")
    cli::cli_alert_info(check_info(x))
  })
}

#' @export
print.check_result <- function(x, ...) {
  cat(format(x))
  invisible(x)
}

check_info <- function(x) {
  attributes(x)$msg
}

compare_vecs <- function(given, canonical) {
  waldo::compare(given, canonical, x_arg = 'given', y_arg = 'canonical')
}

check_canonical_colnames <- function() {
  fn <- function(x) {
    check_result(compare_vecs(x, get_canonical_col_names()))
  }
  new_check(fn, "global")
}
