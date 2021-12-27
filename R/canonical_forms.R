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
canonical_form <- function(object_class, col_names, col_classes, transformers = list(),
                           checks = list(), add_default_checks = TRUE) {
  out <- structure(
    list(
      object_class = object_class,
      col_names = col_names,
      col_classes = col_classes,
      transformers = transformers,
      checks = checks
    ),
    class = "CanonicalForm"
  )
  if (add_default_checks) {
    out$checks <- c(default_checks(out), out$checks)
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
  nchecks <- n_checks(form)
  check_names <- checks(form)
  if (nchecks == 0) {
    warning("No checks for canonical form")
    return(TRUE)
  }
  result <- logical(nchecks)
  names(result) <- check_names
  for (name in check_names) {
    fn <- form$checks[[name]]
    result[name] <- fn(x)
  }
  is_canonical <- all(result)
  if (verbose) {
    pass <- "\u2705"
    fail <- "\u274C"
    emoji <- ifelse(result, pass, fail)
    max_name_length <- max(nchar(check_names))
    msg <- paste0("Checks:\n", bullets(paste0(stringr::str_pad(check_names,
      width = max_name_length + 4,
      side = "right", pad = "."
    ), emoji)))
    message(msg)
  }
  is_canonical
}

#' Format a canonical form
#'
#' @param x a CanonicalForm
#' @param ... other arguments passed to format
#'
#' @export
#'
format.CanonicalForm <- function(x, ...) {
  coltypes <- paste(paste0("  ", x$col_names), x$col_classes, sep = ": ", collapse = "\n")
  as.character(glue::glue("Canonical Form for object of class: {dput_to_str(x$object_class)}
              {coltypes}"))
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


#' Get the R code to create a CanonicalForm
#'
#' @param x a canonical form
#'
#' @return a character vector containing an R call
#' @export
#' @importFrom utils capture.output
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

add_transformer <- function(canonical_form, ...) {
  kwargs <- list(...)
  if (is.null(names(kwargs))) {
    stop("Arguments must be named")
  }
  canonical_form$transformers <- c(canonical_form$transformers, kwargs)
  canonical_form
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
  handle <- match.arg(handle)
  stopifnot(transformer %in% transformers(cf))
  transform <- cf$transformers[[transformer]]
  out <- transform(x)
  if (handle == "none") {
    return(out)
  }
  canonical <- is_canonical(out, cf)
  error_msg <- "Transformer does not lead to canonical form!"
  if (handle == "warn") {
    warning(error_msg)
    return(out)
  }
  if (handle == "none") {
    stop(error_msg)
  }
}
