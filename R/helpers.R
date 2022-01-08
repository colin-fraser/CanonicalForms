dput_to_str <- function(x) {
  paste(capture.output(dput(x)), collapse = "\n")
}

classes <- function(x) {
  unname(sapply(x, class, USE.NAMES = FALSE))
}

stop_if_dots_not_named <- function(...) {
  dotlist <- list(...)
  dotnames <- names(dotlist)
  if (is.null(dotnames) || any(dotnames == "")) {
    abort("Arguments to `...` must be named")
  }
}

indent_msg <- function(msg, indent = 2) {
  indent <- paste0(rep(' ', indent), collapse = '')
  paste(sapply(strsplit(msg, '\n'), function(x) paste0(indent, x)), collapse = '\n')
}
