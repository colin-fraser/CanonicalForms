#' Built-in checks
#'
#' @param x the object to check
#' @param canonical_form canonical form to check against
#' @name checks
#' @return logical indicating whether the check is passed


#' @export
#' @rdname checks
check_class <- function(x, canonical_form) {
  all(class(x) == canonical_form$object_class)
}

#' @export
#' @rdname checks
check_colnames <- function(x, canonical_form) {
  all(names(x) == canonical_form$col_names)
}

#' @export
#' @rdname checks
check_col_classes <- function(x, canonical_form) {
  all(classes(x) == canonical_form$col_classes)
}

#' Checks added to canonical forms by default
#' @export
default_checks <- function(form) {
  list(check_class = \(x) check_class(x, form),
       check_colnames = \(x) check_colnames(x, form),
       check_col_classes = \(x) check_col_classes(x, form))
}
