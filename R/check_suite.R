check_suite <- function(checks) {
  stopifnot(
    "`checks` must be a list" = is.list(checks),
    "`checks` must be named" = !is_named(checks)
  )

  structure(
    checks,
    class = 'check_suite'
  )
}

