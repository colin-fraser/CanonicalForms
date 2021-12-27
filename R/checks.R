#' Built-in checks
#'
#' @param form canonical form to check against
#' @name checks
#' @return logical indicating whether the check is passed


#' @export
#' @rdname checks
check_class <- function(form) {
  object_class <- form$object_class
  inner <- function(x) {
    all(class(x) == object_class)
  }
  body(inner) <- do.call("substitute", list(body(inner)))
  inner
}

#' @export
#' @rdname checks
check_colnames <- function(form) {
  col_names <- form$col_names
  inner <- function(x) {
    all(names(x) == col_names)
  }
  body(inner) <- do.call("substitute", list(body(inner)))
  inner
}

#' @export
#' @rdname checks
check_col_classes <- function(form) {
  col_classes <- form$col_classes
  inner <- function(x) {
    all(classes(x) == col_classes)
  }
  body(inner) <- do.call("substitute", list(body(inner)))
  inner
}



#' Checks added to canonical forms by default
#'
#' @param form a CanonicalForm
#' @export
default_checks <- function(form) {
  list(
    check_class = check_class(form),
    check_colnames = check_colnames(form),
    check_col_classes = check_col_classes(form)
  )
}


#' Add a check to a canonical form
#'
#' @param cf CanonicalForm object
#' @param ... named arguments where each name is the name of the check, and each value
#'   is a check function
#'
#' @return a CanonicalForm object with the additional check
#' @export
#'
add_check <- function(cf, ...) {
  kwargs <- list(...)
  stopifnot(
    "Arguments must be named" = !is.null(names(kwargs)),
    "Check functions must have arguments c('x', 'form')" = verify_checks_are_well_formed(kwargs)
  )
  cf$checks <- c(cf$checks, kwargs)
  cf
}

verify_checks_are_well_formed <- function(list_of_checks) {
  all(sapply(list_of_checks, function(f) names(formals(f)) == c("x", "form")))
}

#' Built-in check makers
#'
#' @param cols columns to apply the check to
#'
#' @return a function that can be added to canonical form checks
#' @export
#'
make_check_no_nas <- function(cols) {
  function(x, form) {
    out <- apply(x[,cols], 2, function(y) !anyNA(y))
    return(all(out))
  }
}
