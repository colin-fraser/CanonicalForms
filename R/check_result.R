CHECKMARK <- "\U2714"
XMARK <- "x"
ALL_PASSED <- "\U1F60E"


#' Create a CheckResult object
#'
#' @param result logical value indicating if the result passes
#' @param msg character indicating the failure message. If the result passed,
#'   this is converted to ""
#'
#' @noRd
check_result <- function(result, msg) {
  stopifnot(
    "result must be logical" = class(result) == "logical",
    "result must be of length 1" = length(result) == 1
  )
  if (result) msg <- ""
  structure(result, class = "CheckResult", msg = msg)
}

check_descriptor <- function(r) {
  stopifnot("r must be of type `check_result`" = class(r) == "CheckResult")
  if (r) {
    return("passed")
  } else {
    return("failed")
  }
}

#' @export
format.CheckResult <- function(x, ...) {
  out <- paste(check_descriptor(x), "check")
  if (!x) {
    out <- paste(out, "\nAdditional info:", check_info(x), sep = "\n")
  }
  out
}

#' @export
print.CheckResult <- function(x, ...) {
  cat(format(x))
}



check_info <- function(r) {
  attributes(r)$msg
}

result_list <- function(list_of_results, scope = c('column', 'global')) {
  scope = match.arg(scope)
  structure(list_of_results, class = 'ResultList', scope = scope)
}

result_list_to_logical <- function(x) {
  names <- names(x)
  setNames(as.logical(x), names)
}

all_pass <- function(result_list) {
  all(result_list_to_logical(result_list))
}

result_list_to_logical <- function(result_list) {
  vapply(result_list, as.logical, logical(1))
}

result_list_to_string <- function(result_list, pass = "P", fail = "F") {
  paste0(ifelse(as.logical(result_list), pass, fail), collapse = '')
}

result_list_scope <- function(result_list) {
  attributes(result_list)$scope
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
  messages <- sapply(result_list, function(x) check_info(x))
  paste(test_names, messages, sep = "\n", collapse = "\n\n")
}

conjunction <- function(result_list) {
  result <- all(as.logical(result_list))
  msg <- aggregate_result_list_messages(result_list)
  check_result(result, msg)
}

aggregate_result_list_messages <- function(result_list) {
  failures <- result_list[!as.logical(result_list)]
  messages <- sapply(seq_along(failures),
                     function(i)
                       paste(names(failures)[i], indent_msg(check_info(failures[[i]])), sep = '\n'))
  names(messages) <- rep('x', length(messages))
  rlang::format_error_bullets(messages)
}

