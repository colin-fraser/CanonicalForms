CHECKMARK <- "\U2714"
XMARK <- "x"
ALL_PASSED <- "\U1F60E"


check_result <- function(result, msg) {
  stopifnot(
    "result must be logical" = class(result) == "logical",
    "result must be of length 1" = length(result) == 1
  )
  structure(list(result = result, msg = msg), class = "CheckResult")
}

check_descriptor <- function(r) {
  stopifnot("r must be of type `check_result`" = class(r) == "CheckResult")
  if (as.logical(r)) {
    return("passed")
  } else {
    return("failed")
  }
}

#' @export
format.CheckResult <- function(x, ...) {
  out <- paste(check_descriptor(x), "check")
  if (!as.logical(x)) {
    out <- paste(out, "\nAdditional info:", check_info(x), sep = "\n")
  }
  out
}

#' @export
print.CheckResult <- function(x, ...) {
  cat(format(x))
}

#' @export
as.logical.CheckResult <- function(x, ...) {
  x$result
}

check_info <- function(r) {
  r$msg
}

result_list_to_logical <- function(result_list) {
  vapply(result_list, as.logical, logical(1))
}

result_list_to_string <- function(result_list, pass = "P", fail = "F") {
  paste0(vapply(result_list, function(x) if (as.logical(x)) pass else fail, character(1)), collapse = "")
}

result_list_summary <- function(result_list) {
  results <- result_list_to_logical(result_list)
  title <- crayon::bold("CHECKS SUMMARY")
  topline_summary <- format_result_lines(results)
  if (all(results)) {
    addl_info <- paste("All checks passed", ALL_PASSED)
  } else {
    failure_indices <- which(!results, useNames = TRUE)
    failure_summary <- format_failed_tests(result_list[failure_indices])
    addl_info <- paste("Additional information:", failure_summary, sep = "\n")
  }
  paste(title, topline_summary, addl_info, "\r", sep = "\n")
}

format_result_line <- function(text, outcome, width = 40, pass = CHECKMARK, fail = XMARK,
                               min_dots = 2, crayon = TRUE) {
  text_length <- nchar(text)
  max_text_length <- width - min_dots - 1
  if (text_length > max_text_length) {
    text <- paste0(substr(text, 1, max_text_length - 5), "[...]")
    text_length <- max_text_length
  }
  n_dots <- width - text_length - 1
  mark <- if (outcome) pass else fail
  col <- if (outcome) crayon::green else crayon::red
  dots <- paste0(rep(".", n_dots), collapse = "")
  col(paste0(text, dots, mark))
}

format_result_lines <- function(logical_results) {
  lines <- ""
  testnames <- names(logical_results)
  for (i in seq_along(logical_results)) {
    lines <- paste0(lines, format_result_line(testnames[[i]], logical_results[[i]]), "\n")
  }
  lines
}

format_failed_tests <- function(result_list) {
  test_names <- paste("Failed check:", names(result_list))
  messages <- sapply(result_list, function(x) x$msg)
  paste(test_names, messages, sep = "\n", collapse = "\n\n")
}
