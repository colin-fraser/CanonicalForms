dput_to_str <- function(x) {
  paste(capture.output(dput(x)), collapse = "\n")
}

bullets <- function(header, ..., info = NULL) {
  bullets <- vec_c(..., .name_spec = "{outer}")

  paste0(
    header,
    format_error_bullets(ensure_full_stop(bullets)),
    if (!is.null(info)) paste0("\n", format_error_bullets(c(i = info)))
  )
}
