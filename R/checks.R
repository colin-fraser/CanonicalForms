run_check <- function(check_name, cf, ...) {
  run_in_cf_env(cf, check_name, ...)
}

run_all_checks <- function(x, cf) {
  checks <- get_checks(cf)
  lapply(checks, run_check, cf = cf, x = x)
}

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

get_property <- function(nm, cf = NULL) {
  if (is.null(cf)) {
    return(get_from_caller_env_recursive(nm = nm))
  } else {
    env <- get_check_env(cf)
    env_get(env, nm, default = NULL)
  }
}

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

add_check <- function(cf, ...) {
  cf$checks <- c(cf$checks, list(...))
  cf
}


canonical_object_class <- function(cf = NULL) {
  get_property(".object_class", cf)
}

canonical_col_names <- function(cf = NULL) {
  get_property(".col_names", cf)
}

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
