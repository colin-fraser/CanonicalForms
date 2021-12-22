classes <- function(x) {
  sapply(x, class, USE.NAMES = FALSE)
}

#' Title
#'
#' @param col_names
#' @param col_classes
#' @param transformers
#'
#' @return
#' @export
#'
#' @examples
canonical_form <- function(col_names, col_classes, transformers = list(), checks = list()) {
  structure(
    list(col_names = col_names,
         col_classes = col_classes,
         transformers = transformers,
         checks = checks),
    class = 'CanonicalForm'
  )
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
extract_canonical_form <- function(x) {
  canonical_form(col_names = names(x),
                 col_classes = classes(x))
}

is_canonical <- function(x, form, verbose = TRUE) {
  stopifnot(class(form) == 'CanonicalForm')
  left_names <- setdiff(names(x), form$col_names)
  right_names <- setdiff(form$col_names, names$x)
  left_types <- setdiff(classes(x), form$col_classes)
  right_types <- setdiff(form$col_classes, classes(x))
  checks <- c()
  if (length(form$checks) > 0) {
    for (i in seq_along(form$checks)) {
      checks[names(form$checks)] <- form$checks[i](x)
    }
  }
  is_canonical <- all(names(x) == form$col_names) & all(classes(x) == form$col_names) & all(checks)
}

#' Format a canonical form
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
format.CanonicalForm <- function(x) {
  coltypes <- paste(paste0('  ', x$col_names), x$col_classes, sep = ': ', collapse = '\n')
  glue::glue("Canonical Form
              {coltypes}")
}

#' Print Canonical Form
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
print.CanonicalForm <- function(x) {
  cat(format(x))
}
