#' Create a CanonicalForm
#'
#' @param object_class the class of the object
#' @param col_names a vector of column names
#' @param col_classes a vector of column classes
#' @param transformers a list of canonicalization functions
#' @param global_checks a list of functions that will be used to check
#'   if a dataset is canonical
#' @param column_checks a list of checks
#' @param uncategorized_checks a list of checks
#' @param add_default_checks should the default checks be added?
#'
#' @return an object of class CanonicalForm
#' @export
#'
canonical_form <- function(object_class,
                           col_names,
                           col_classes,
                           checks = list(),
                           transformers = list(),
                           add_default_checks = TRUE) {
  env <- env()
  env$global_properties <- list(object_class = object_class,
                                     col_names = col_names,
                                     col_classes = col_classes)

  env$check_suite <- check_suite(checks)
  env$transformers <- transformers

  out <- structure(
    env,
    class = "CanonicalForm"
  )
  if (add_default_checks) {
    TRUE
    # out$checks <- c(default_checks(), out$checks)
  }
  out
}

is_canonical_form <- function(x) {
  class(x) == "CanonicalForm"
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
  results <- run_all_checks(x, form)
  passing <- all(result_list_to_logical(results))
  if (verbose) {
    rlang::inform(result_list_summary(results))
  }
  passing
}

#' @describeIn is_canonical check if a dataset is canonical and return the dataset
#' @param behavior what to do if the dataset fails the check
#' @export
check_canonical <- function(x, form, behavior = c("warn", "stop", "inform")) {
  behavior <- rlang::arg_match(behavior)
  results <- run_all_checks(x, form)
  passing <- all(as.logical(results))
  msg <- result_list_summary(results)
  if (!passing) {
    switch(behavior,
      "warn" = rlang::warn(msg),
      "stop" = rlang::abort(msg),
      "inform" = rlang::inform(msg)
    )
  }
  x
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
    paste0("  ", get_canonical_col_names(x)),
    get_canonical_col_classes(x),
    sep = ": ",
    collapse = "\n"
  )
  glue::glue(
    "Canonical Form for object of class: {dput_to_str(get_canonical_object_class(x))}
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

#' @export
Ops.CanonicalForm <- function(e1, e2) {
  if (!testthat::is_testing()) {
    rlang::warn("Comparison of canonical forms is implemented to ease testing, but shouldn't be trusted in general
        as functions are not easy to compare.")
  }
  switch(.Generic,
    "==" = {
      identical(get_check_names(e1), get_check_names(e2)) &&
        identical(get_transformer_names(e1), get_transformer_names(e2)) &&
        identical(get_properties(e1), get_properties(e2))
    },
    stop(glue::glue("Comparison `{.Generic}` not implemented"))
  )
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
                    checks = {checks},
                    add_default_checks = FALSE)"
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

get_transformer_names <- function(cf) {
  names(cf$transformers)
}

#' List the names of checks associated with a CanonicalForm
#'
#' @param cf a CanonicalForm object
#'
#' @return character vector of names of checks
#' @export
#'
#' @examples
#' cf <- extract_canonical_form(cars)
#' get_check_names(cf)
get_check_names <- function(cf) {
  names(cf$checks)
}

n_checks <- function(cf) {
  length(get_check_names(cf))
}





find_cf_in_call_stack <- function(i = 1, recursion_limit = 100) {
  if ((i-1)/2 >= recursion_limit) {
    stop("Recursion limit reached")
  }
  envir <- parent.frame(i)
  if (is_canonical_form(envir)) {
    return(envir)
  } else {
    find_cf_in_call_stack(i + 2)
  }
}

get_global_properties <- function(cf = NULL) {
  if (is.null(cf)) {
    cf <- find_cf_in_call_stack()
  } else {
    stopifnot(is_canonical_form(cf))
  }
  cf$global_properties
}

get_global_property <- function(nm, cf = NULL) {
  gp <- get_global_properties(cf)
  gp[[nm]]
}

get_canonical_col_names <- function(cf = NULL) {
  get_global_property("col_names", cf)
}

get_canonical_col_classes <- function(cf = NULL) {
  get_global_property("col_classes", cf)
}

get_canonical_object_class <- function(cf = NULL) {
  get_global_property("object_class", cf)
}

execute_fn_in_cf <- function(fn, cf, args = list()) {
  do.call(what = fn, args = args, envir = cf)
}

transform_canonical <- function(x, cf, transformer, handle = c("warn", "stop", "none")) {
  return(FALSE)
}

