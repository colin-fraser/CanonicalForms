classes <- function(x) {
  unname(sapply(x, class, USE.NAMES = FALSE))
}

#' Create a CanonicalForm
#'
#' @param object_class the class of the object
#' @param col_names a vector of column names
#' @param col_classes a vector of column classes
#' @param transformers a list of canonicalization functions
#' @param checks a list of functions that will be used to check
#'   if a dataset is canonical
#' @param add_default_checks should the default checks be added?
#'
#' @return an object of class CanonicalForm
#' @export
#'
canonical_form <- function(object_class,
                           col_names,
                           col_classes,
                           transformers = list(),
                           checks = list(),
                           add_default_checks = TRUE) {
  check_env <- env(
    .object_class = object_class,
    .col_names = col_names,
    .col_classes = col_classes
  )

  out <- structure(
    list(
      check_env = check_env,
      transformers = transformers,
      checks = checks
    ),
    class = "CanonicalForm"
  )
  if (add_default_checks) {
    out$checks <- c(default_checks(), out$checks)
  }
  out
}

#' Create a CanonicalForm from a data frame
#'
#' @param x dataframe-like
#' @param transformers a list of transformers
#' @param checks  list of checks
#'
#' @return a canonical form
#' @export
#'
#' @examples extract_canonical_form(cars)
extract_canonical_form <- function(x, transformers = list(), checks = list()) {
  canonical_form(
    object_class = class(x),
    col_names = names(x),
    col_classes = classes(x),
    transformers = transformers,
    checks = checks
  )
}

#' Check if a dataset is canonical
#'
#' @param x a dataset
#' @param form a CanonicalForm
#' @param verbose print check results?
#'
#' @return logical value indicating if the dataset is canonical
#' @export
#'
is_canonical <- function(x, form, verbose = TRUE) {
  all(result_list_to_logical(run_all_checks(x, form)))
}

#' Format a canonical form
#'
#' @param x a CanonicalForm
#' @param ... other arguments passed to format
#'
#' @export
#'
format.CanonicalForm <- function(x, ...) {
  coltypes <- paste(
    paste0("  ", canonical_col_names(x)),
    canonical_col_classes(x),
    sep = ": ",
    collapse = "\n"
  )
  glue::glue(
    "Canonical Form for object of class: {dput_to_str(canonical_object_class(x))}
              {coltypes}"
  )
}

#' Print Canonical Form
#'
#' @param x a CanonicalForm
#' @param ... other arguments
#'
#' @export
#'
print.CanonicalForm <- function(x, ...) {
  cat(format(x))
}


get_check_env <- function(cf) {
  cf$check_env
}

get_checks <- function(cf) {
  cf$checks
}



#' Get the R code to create a CanonicalForm
#'
#' @param x a canonical form
#'
#' @return a character vector containing an R call
#' @export
#' @importFrom utils capture.output
#'
to_r_code <- function(x) {
  calls <- list(
    object_class = dput_to_str(canonical_object_class(x)),
    col_names = dput_to_str(canonical_col_names(x)),
    col_classes = dput_to_str(canonical_col_classes(x)),
    transformers = dput_to_str(get_transformers(x)),
    checks = dput_to_str(get_checks(x))
  )
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

add_transformer <- function(canonical_form, ...) {
  kwargs <- list(...)
  if (is.null(names(kwargs))) {
    stop("Arguments must be named")
  }
  canonical_form$transformers <- c(canonical_form$transformers, kwargs)
  canonical_form
}

get_transformers <- function(cf) {
  cf$transformers
}

transformers <- function(cf) {
  names(cf$transformers)
}

checks <- function(cf) {
  names(cf$checks)
}

n_checks <- function(cf) {
  length(checks(cf))
}


transform_canonical <- function(x, cf, transformer, handle = c("warn", "stop", "none")) {
  return(FALSE)
}
