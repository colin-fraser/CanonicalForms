dummy_cf <- function() {
  canonical_form('class', c('a', 'b', 'c'), c('x', 'y', 'z'), add_default_checks = FALSE)
}

test_that("creating a CanonicalForm works", {
  cf <- canonical_form('class', c('a', 'b', 'c'), c('x', 'y', 'z'), add_default_checks = FALSE)
  expect_s3_class(cf, 'CanonicalForm')
})

test_that("find_cf_in_call_stack", {
  expect_error(find_cf_in_call_stack(), 'Recursion limit')
  cf <- dummy_cf()
  f <- function() find_cf_in_call_stack()
  do.call(f, list(), envir = cf)
  g <- function() f()
  do.call(f, list(), envir = cf)
})

test_that("get_global_properties", {
  cf <- dummy_cf()
  props <- get_global_properties(cf)
  expected <- list(object_class = "class",
                   col_names = c("a", "b", "c"),
                   col_classes = c("x", "y", "z"))
  expect_identical(props, expected)

  f <- function() get_global_properties()
  expect_error(f(), 'Recursion limit')
  props2 <- do.call(f, list(), envir = cf)
  expect_identical(props, expected)
}
)


test_that("Canonical property getters", {
  cf <- dummy_cf()
  expected <- list("class", c("x", "y", "z"), c("a", "b", "c"))
  f <- function(cf = NULL) {
    list(
      get_canonical_object_class(cf),
      get_canonical_col_classes(cf),
      get_canonical_col_names(cf)
    )
  }
  expect_error(f(), 'Recursion limit')
  expect_equal(f(cf), expected)
  expect_equal(do.call(f, list(), envir = cf), expected)
  expect_equal(execute_fn_in_cf(f, cf), expected)
})
