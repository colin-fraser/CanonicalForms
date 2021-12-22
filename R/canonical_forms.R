classes <- function(x) {
  unname(sapply(x, class, USE.NAMES = FALSE))
}

#' Create a CanonicalForm
#'
#' @param col_names a vector of column names
#' @param col_classes a vector of column classes
#' @param transformers a list of canonicalization functions
#' @param checks a list of functions that will be used to check
#'   if a dataset is canonical
#'
#' @return an object of class CanonicalForm
#' @export
#'
canonical_form <- function(object_class, col_names, col_classes, transformers = list(),
                           checks = list(check_class = check_class,
                                         check_colnames = check_colnames,
                                         check_col_classes = check_col_classes)) {
  structure(
    list(
      object_class = object_class,
      col_names = col_names,
      col_classes = col_classes,
      transformers = transformers,
      checks = checks
    ),
    class = "CanonicalForm"
  )
}

#' Create a CanonicalForm from a data frame
#'
#' @param x dataframe-like
#'
#' @return a canonical form
#' @export
#'
#' @examples extract_canonical_form(cars)
extract_canonical_form <- function(x, transformers = list(),
                                   checks = default_checks()) {
  canonical_form(
    object_class = class(x),
    col_names = names(x),
    col_classes = classes(x),
    transformers = transformers,
    checks = checks
  )
}

is_canonical <- function(x, form, verbose = TRUE) {
  stopifnot(class(form) == "CanonicalForm")
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
format.CanonicalForm <- function(x) {
  coltypes <- paste(paste0("  ", x$col_names), x$col_classes, sep = ": ", collapse = "\n")
  glue::glue("Canonical Form for object of class: {dput_to_str(x$object_class)}
              {coltypes}")
}

#' Print Canonical Form
#'
#' @param x
#'
#' @return
#' @export
#'
print.CanonicalForm <- function(x) {
  cat(format(x))
}


#' Get the R code to create a CanonicalForm
#'
#' @param x a canonical form
#'
#' @return
#' @export
#'
to_r_code <- function(x) {
  calls <- lapply(x, dput_to_str)
  out <- glue::glue_data(
    calls,
    "canonical_form(object_class = {object_class},
                    col_names = {col_names},
                    col_classes = {col_classes},
                    transformers = {transformers},
                    checks = {checks})"
  )
  styler::style_text(out)
}
