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



compare_vecs <- function(canonical, given) {
  msg <- waldo::compare(canonical, given, x_arg = "canonical", y_arg = "given")
  result <- length(msg) == 0
  check_result(result, msg)
}

#' Built-in checks
#'
#' These checks are built-in
#' @param x a dataframe to check
check_class <- function(x) {
  compare_vecs(canonical_object_class(), class(x))
}

#' @describeIn check_class check that the column names match the Canonical Form
check_col_names <- function(x) {
  compare_vecs(canonical_col_names(), colnames(x))
}

#' @describeIn check_class check that the classes match the Canonical Form
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

logical_vector_to_check_result <- function(vec, header) {
  test_passed <- all(vec)
  msg <- ""
  if (!test_passed) {
    badcols <- names(vec)[!vec]
    names(badcols) <- rep("x", length(badcols))
    msg <- paste(header, rlang::format_error_bullets(badcols), sep = "\n")
  }
  check_result(test_passed, msg)
}

#' Check makers
#'
#' Create checks to add to Canonical Forms. Each of these functions returns a function which takes
#' a dataframe and returns a CheckResult object.
#'
#' @param cols character vector of columns to include in the check
#'
#' @export
check_no_nas <- function(cols) {
  function(x) {
    result <- apply(x[cols], 2, function(y) !anyNA(y))
    logical_vector_to_check_result(result, "Unexpected NAs in the following column(s):")
  }
}


#' @describeIn check_no_nas check that all values are above specified values
#'
#' @param ... named arguments of the form (col = constraint)
#' @param .strict should values equal to the constraint be disallowed?
#'
#' @details for \code{check_greater_than} and \code{check_less_than}, different values can be specified for each
#'    column. See examples.
#' @export
#'
#' @examples
#' df <- data.frame(a = c(1, 2, NA), b = c(4, 5, 6))
#' check_na <- check_no_nas("a")
#' check_na(df) # fails
#' check_na2 <- check_no_nas("b")
#' check_na2(df) # passes
#'
#' check_gt <- check_greater_than(a = 0, b = 3)
#' check_gt(df)  # passes
#' check_gt2 = check_greater_than(a = 1, b = 4, .strict = FALSE)
#' check_gt2(df)  # passes (because .strict=FALSE)
#' check_gt3 <- check_greater_than(a = 1, b = 1, .strict = TRUE)
#' check_gt3(df)  # fails
check_greater_than <- function(..., .strict = TRUE) {
  if (.strict) {
    comparison <- ">"
  } else {
    comparison <- ">="
  }
  check_comparison(..., comparison = comparison)
}

#' @export
#' @describeIn check_no_nas check all values are less than constraints
check_less_than <- function(..., .strict = TRUE) {
  if (.strict) comparison <- "<" else comparison <- "<="
  check_comparison(..., comparison = comparison)
}

check_comparison <- function(..., comparison) {
  comp_fn <- get_comparison_operator_from_name(comparison)
  violation_descriptor <- switch(comparison,
    ">" = "below minimum",
    ">=" = "below minimum",
    "<" = "above maximum",
    "<=" = "above maximum",
    stop("unknown")
  )
  msg <- paste("Values found", violation_descriptor, "in the following column(s):")
  function(x) {
    result <- apply_comparisons_from_named_list(x, comp_fn, list(...))
    logical_vector_to_check_result(result, msg)
  }
}

apply_comparisons_from_named_list <- function(x, comparison, args) {
  cols <- names(args)
  purrr::imap_lgl(args, ~ all(comparison(x[.y], .x), na.rm = TRUE))
}

get_comparison_operator_from_name <- function(comparison) {
  function(e1, e2) do.call(comparison, list(e1, e2))
}
