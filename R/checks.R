#' Run a check
#'
#' This is the primary way that checks are run programatically.
#' This runs the check in the \code{check_env} environment, which
#' allows it to access properties about the CanonicalForm such as
#' canonical_col_names().
#'
#' @param check a function to run
#' @param cf a CanonicalForm object
#' @param ... additional arguments passed to the check function
#'
#' @return the return value of check
#' @export
#'
#' @examples
#' cf <- extract_canonical_form(cars)
#' run_check("check_class", cf, x = cars)
run_check <- function(check, cf, ...) {
  if (is.character(check)) {
    check <- cf$checks[[check]]
  }
  run_in_cf_env(cf, check, ...)
}


#' Run all checks
#'
#' Run all the tests registered to CanonicalForm
#'
#' @param x the object to check against, probably a data.frame or tibble
#' @param cf a canonical_form
#'
#' @return a list of check_result objects
#' @export
#'
#' @examples
#' cf <- extract_canonical_form(cars)
#' run_all_checks(cars, cf)
run_all_checks <- function(x, cf) {
  checks <- get_checks(cf)
  lapply(checks, run_check, cf = cf, x = x)
}

#' Low-level accessors for getting properties
#'
#' \code{get_from_caller_env_recursive(nm)} gets the object called
#' \code{nm} from the call stack. \code{get_property} is a friendlier
#' api for specifically getting properties out of CanonicalForm environments.
#'
#' @param nm the name of the object to get
#' @param i the number of levels up to climb in the call stack. This should usually
#'   not be set manually, but is incremented recursively by the function.
#' @param recursion_limit a limit that probably shouldn't be used, except for testing
#'
#' @return the property to get
#'
#' @examples
#' e1 <- new.env()
#' e1$a <- "a"
#' do.call(CanonicalForms:::get_from_caller_env_recursive, list(nm = "a"), envir = e1)
#'
#' cf <- extract_canonical_form(cars)
#' get_property(".col_names", cf)
#' f <- function() get_property(".col_names")
#' do.call(f, list(), envir = CanonicalForms:::get_check_env(cf))
#' CanonicalForms:::run_in_cf_env(cf, get_property, nm = ".col_names")
get_from_caller_env_recursive <- function(nm, i = 1, recursion_limit = 100) {
  if (i > recursion_limit) {
    abort("recursion limit reached")
  }
  env <- caller_env(i)
  if (env_has(env, nm)) {
    return(env_get(env, nm, default = NA))
  } else if (identical(env, global_env())) {
    abort(glue::glue("{nm} not found"))
  } else {
    return(get_from_caller_env_recursive(nm, i + 2))
  }
}


#' @param cf a CanonicalForm object
#' @export
#'
#' @describeIn get_from_caller_env_recursive get a property from a CanonicalForm
get_property <- function(nm, cf = NULL) {
  if (is.null(cf)) {
    return(get_from_caller_env_recursive(nm = nm))
  } else {
    env <- get_check_env(cf)
    env_get(env, nm, default = NULL)
  }
}

#' @param f function to execute
#' @param ... arguments to pass to f
#' @describeIn get_from_caller_env_recursive run function in CanonicalForm environment
run_in_cf_env <- function(cf, f, ...) {
  exec(f, ..., .env = get_check_env(cf))
}


determine_env <- function(cf) {
  if (is.null(cf)) {
    env <- caller_env()
  } else {
    env <- cf$check_env
  }
  env
}

result_list_to_logical <- function(result_list) {
  vapply(result_list, as.logical, logical(1))
}

result_list_to_string <- function(result_list) {
  paste0(vapply(result_list, function(x) if (as.logical(x)) "P" else "F", character(1)), collapse = "")
}

#' Add checks or delete check
#'
#' Note that this does not work in place. See examples
#'
#' @param cf a CanonicalForm object
#' @param ... named arguments. Each name will be the name of the check, and
#'   each value should be a check function
#'
#' @return modified CanonicalForm object
#' @export
#'
#' @examples
#' cf <- extract_canonical_form(cars)
#' check <- function(x) TRUE
#' cf <- add_checks(cf, new_check = check) # note this does not work in place
#' print(get_check_names(cf))
#' cf <- delete_check(cf, "new_check")
#' print(get_check_names(cf))
add_checks <- function(cf, ...) {
  kwargs <- list(...)
  checknames <- names(kwargs)
  if (is.null(checknames) || any(checknames == "")) {
    abort("All arguments to ... must be named")
  }
  existing_checks <- get_check_names(cf)
  if (any(checknames %in% existing_checks)) {
    overlap <- intersect(checknames, existing_checks)
    names(overlap) <- "x"
    abort(c(
      "New check names must not already exist in CanonicalForm. Existing names:",
      overlap
    ))
  }
  cf$checks <- c(cf$checks, list(...))
  cf
}

#' @describeIn add_checks Delete a check
#' @param check_name the name of the check to delete
#'
#' @export
#'
delete_check <- function(cf, check_name) {
  cf$checks[[check_name]] <- NULL
  cf
}


#' Access the properties of a CanonicalForm
#'
#' These functions allow you to access the properties of a CanonicalForm
#' from inside of another function. See examples.
#'
#' @param cf a CanonicalForm object
#'
#' @return The value of a property
#' @export
#'
#' @examples
#' check_col_names_in_canonical <- function(x) all(colnames(x) %in% canonical_col_names())
#' cf <- extract_canonical_form(cars)
#' run_check(check_col_names_in_canonical, cf, x = cars)
canonical_object_class <- function(cf = NULL) {
  get_property(".object_class", cf)
}

#' @describeIn canonical_object_class Canonical object class
#' @export
canonical_col_names <- function(cf = NULL) {
  get_property(".col_names", cf)
}

#' @describeIn canonical_object_class Canonical column classes
#' @export
canonical_col_classes <- function(cf = NULL) {
  get_property(".col_classes", cf)
}

check_result <- function(result, msg) {
  stopifnot(
    "result must be logical" = class(result) == "logical",
    "result must be of length 1" = length(result) == 1
  )
  structure(list(result = result, msg = msg), class = "check_result")
}

check_descriptor <- function(r) {
  stopifnot("r must be of type `check_result`" = class(r) == "check_result")
  if (as.logical(r)) {
    return("passed")
  } else {
    return("failed")
  }
}

check_info <- function(r) {
  r$msg
}

#' @export
format.check_result <- function(x, ...) {
  out <- paste(check_descriptor(x), "check")
  if (!as.logical(x)) {
    out <- paste(out, "\nAdditional info:", check_info(x), sep = "\n")
  }
  out
}

#' @export
print.check_result <- function(x, ...) {
  cat(format(x))
}

#' @export
as.logical.check_result <- function(x, ...) {
  x$result
}

compare_vecs <- function(canonical, given) {
  msg <- waldo::compare(canonical, given, x_arg = "canonical", y_arg = "given")
  result <- length(msg) == 0
  check_result(result, msg)
}

check_class <- function(x) {
  compare_vecs(canonical_object_class(), class(x))
}

check_col_names <- function(x) {
  compare_vecs(canonical_col_names(), colnames(x))
}

check_col_classes <- function(x) {
  compare_vecs(canonical_col_classes(), classes(x))
}

default_checks <- function() {
  list(
    check_class = check_class,
    check_col_names = check_col_names,
    check_col_classes = check_col_classes
  )
}
