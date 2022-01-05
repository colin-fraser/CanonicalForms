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
