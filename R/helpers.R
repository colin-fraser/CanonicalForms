dput_to_str <- function(x) {
  paste(capture.output(dput(x)), collapse = "\n")
}

bullets <- function(x) {
  paste("  *", x, collapse = "\n")
}
