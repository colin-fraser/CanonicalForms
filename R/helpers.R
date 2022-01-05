dput_to_str <- function(x) {
  paste(capture.output(dput(x)), collapse = "\n")
}

classes <- function(x) {
  unname(sapply(x, class, USE.NAMES = FALSE))
}
