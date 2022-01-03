dput_to_str <- function(x) {
  paste(capture.output(dput(x)), collapse = "\n")
}

classes <- function(x) {
  unname(sapply(x, class, USE.NAMES = FALSE))
}

list2keyvalpairs <- function(lst, key_name = "key", val_name = "val") {
  if (!all_values_are_named(lst)) {
    abort("list passed to list2keyvalpairs must be named")
  }
  list_names <- names(lst)
  out <- list()
  for (i in seq_along(lst)) {
    out[[i]] <- list()
    out[[i]][[key_name]] <- list_names[[i]]
    out[[i]][[val_name]] <- lst[[i]]
  }
  out
}

all_values_are_named <- function(x) {
  names <- names(x)
  !is.null(names) && !any(names == "")
}
