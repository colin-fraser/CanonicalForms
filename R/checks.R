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

compare_vecs <- function(canonical, given, order_matters = TRUE) {
  max_diffs <- if (testthat::is_testing()) Inf else 10
  stopifnot("`compare_vecs` should only take a vector" = is.vector(given))
  if (!order_matters) {
    canonical <- sort(canonical)
    give <- sort(given)
  }
  msg <- waldo::compare(canonical, given,
    x_arg = "canonical", y_arg = "given",
    max_diffs = max_diffs
  )
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

#' @describeIn check_class returns a list of the default checks.
default_checks <- function() {
  list(
    check_class = check_class,
    check_col_names = check_col_names,
    check_col_classes = check_col_classes
  )
}

#' Create a CheckResult from a named logical vector
#'
#' This takes a named logical vector and turns it into a CheckResult
#' object, using the names to create a message.
#'
#' @param vec a named logical vector
#' @param header the header for the message that will be created.
#'
#' @return a CheckResult object
#' @keywords internal
#'
#' @examples
#' vec <- c(a = TRUE, b = FALSE)
#' # test fails, and message includes "b" as the reason for failing
#' res <- CanonicalForms:::named_logical_vector_to_check_result(vec, "A test!")
#' print(res)
#' #> failed check
#' #>
#' #> Additional info:
#' #>   A test!
#' #>   x b
named_logical_vector_to_check_result <- function(vec, header) {
  test_passed <- all(vec)
  msg <- ""
  if (!test_passed) {
    badcols <- names(vec)[!vec]
    names(badcols) <- rep("x", length(badcols))
    msg <- paste(header, rlang::format_error_bullets(badcols), sep = "\n")
  }
  check_result(test_passed, msg)
}

#' Internal: apply a function to columns of a dataset,
#'
#' With different arguments to each column. This is used by many of the
#' check functions.
#'
#' @param .x a data.frame-like object
#' @param .f a function, probably with arguments representing (actual, expected)
#' @param function_args a named list, where the names are columns of .x
#'   and the values are arguments to the function
#'
#' @return a named list with the same names as function_args
apply_function_to_cols <- function(x, f, function_args, agg = c('all', 'any', 'none')) {
  agg <- match.arg(agg)
  agg <- switch(agg,
    all = all,
    any = any,
    none = identity
  )
  purrr::imap(function_args, ~ agg(f(x[[.y]], .x), na.rm = TRUE))
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
    named_logical_vector_to_check_result(result, "Unexpected NAs in the following column(s):")
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
#' check_gt(df) # passes
#' check_gt2 <- check_greater_than(a = 1, b = 4, .strict = FALSE)
#' check_gt2(df) # passes (because .strict=FALSE)
#' check_gt3 <- check_greater_than(a = 1, b = 1, .strict = TRUE)
#' check_gt3(df) # fails
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

dots_to_bounds <- function(...) {
  args <- list(...)
  lower <- sapply(args, function(x) x[1], USE.NAMES = TRUE)
  upper <- sapply(args, function(x) x[2], USE.NAMES = TRUE)
  list(lower = lower, upper = upper)
}

#' Check that values are within a set of bounds
#'
#' @param ... named arguments where each name is a column, and each value is a length-2 numeric
#'   vector of the form c(lower_bound, upper_bound)
#' @param .strict_upper should the upper bound be strict?
#' @param .strict_lower should the lower bound be strict?
#'
#' @return a CheckResult object
#' @export
#'
#' @examples
#' df <- data.frame(a = c(1, 2, 3, NA), b = c(4, 5, 6, 7))
#' cb1 <- check_between(a = c(1, 4), b = c(3, 7))
#' cb1(df) # passes
#' cb2 <- check_between(a = c(1, 4), b = c(3, 7), .strict_lower = TRUE)
#' cb2(df) # fails
#' cb3 <- check_between(a = c(1, 4), b = c(3, 7), .strict_upper = TRUE)
#' cb3(df) # fails
#' cb4 <- check_between(a = c(1, 4), b = c(3, 7), .strict_lower = TRUE, .strict_upper = TRUE)
#' cb4(df) # fails
check_between <- function(..., .strict_lower = FALSE, .strict_upper = FALSE) {
  bounds <- dots_to_bounds(...)
  lower <- do.call(check_greater_than, args = c(bounds$lower, list(.strict = .strict_lower)))
  upper <- do.call(check_less_than, args = c(bounds$upper, list(.strict = .strict_upper)))
  function(x) {
    conjunction(lower(x), upper(x))
  }
}

#' Check factor levels
#'
#' @param ... named list where each name is a column name
#'   and each value is the expected factor vector
#'
#' @return a check result which passes if each column's levels
#'   are equal to the given canonical levels
#' @export
#'
#' @examples
#' df <- data.frame(
#'   a = as.factor(c("a", "b", "c", "c")),
#'   b = as.factor(c("a", "b", "c", "d"))
#' )
#' passer <- check_factor_levels(a = c("a", "b", "c"), b = c("a", "b", "c", "d"))
#' failer <- check_factor_levels(a = c("a", "b", "c"), b = c("d", "e", "f", "g"))
#' passer(df)
#' failer(df)
#' passer2 <- check_factor_levels(a = c('b', 'c', 'a'),
#'                                b = c('b', 'c', 'a', 'd'),
#'                                .order_matters = FALSE)
#' failer2 <- check_factor_levels(a = c('b', 'c', 'a'),
#'                                b = c('b', 'c', 'a', 'd'),
#'                                .order_matters = TRUE)
#' passer2(df)
#' failer2(df)
check_factor_levels <- function(..., .order_matters = TRUE) {
  stop_if_dots_not_named(...)
  args <- list(...)

  function(x) {
    apply_fn_to_x(.check_col_factor_levels, x, args, .order_matters)
  }
}

.check_col_factor_levels <- function(given_factor, canonical_levels, .order_matters) {
  if (!is.factor(given_factor)) {
    return(check_result(FALSE, "Column is not a factor variable"))
  }
  compare_vecs(canonical_levels, levels(given_factor), order_matters = .order_matters)
}

check_comparison <- function(..., comparison) {
  comp_fn <- col_check_compare(comparison)
  function(x) {
    apply_fn_to_x(comp_fn, x, list(...))
  }
}


col_check_compare <- function(comparison, na.rm = TRUE) {
  compare <- function(e1, e2) do.call(comparison, list(e1, e2))
  violation_descriptor <- switch(comparison,
                                 ">" = "below minimum",
                                 ">=" = "below minimum",
                                 "<" = "above maximum",
                                 "<=" = "above maximum",
                                 stop(sprintf("unknown operator '%s'", comparison))
  )
  function(given, canonical) {
    result <- all(compare(given, canonical), na.rm = na.rm)
    msg <- sprintf("Value(s) found %s of %s", violation_descriptor, canonical)
    check_result(result, msg)
  }
}



#' Apply comparisons from a named list
#'
#' Apply a comparison function to multiple columns of a data frame,
#' which different known values specified for each column.
#'
#' @param x data.frame or similar
#' @param comparison a comparison function
#' @param compare_to a named list of the form list(colname = known_values).
#'
#'
#' @return a named logical vector
#' @keywords internal
#'
#' @examples
#'
#' x <- data.frame(a = 1:2, b = 3:4)
#' comparison <- function(e1, e2) any(e1 == e2)
#' # returns c(a = TRUE, b = FALSE)
#' CanonicalForms:::apply_comparisons_from_named_list(x, comparison, list(a = 1, b = 1))
apply_comparisons_from_named_list <- function(x, comparison, compare_to) {
  purrr::imap_lgl(compare_to, ~ all(comparison(x[.y], .x), na.rm = TRUE))
}

# fn should take arguments "given, canonical"
apply_fn_to_x <- function(fn, x, args, ...) {
  res <- sapply(X = names(args),
         FUN = function(i) fn(x[[i]], args[[i]], ...),
         USE.NAMES = TRUE, simplify = FALSE)
  result_list(res, 'column')
}
