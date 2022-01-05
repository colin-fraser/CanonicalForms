test_that("dput_to_str works as expected", {
  expect_equal(dput_to_str(c(1, 2, 3)), "c(1, 2, 3)")
})

test_that("classes function works as expected", {
  expect_identical(classes(cars), c("numeric", "numeric"))
})

test_that("stop if dots not named works", {
  f <- function(...) {
    stop_if_dots_not_named(...)
    TRUE
  }
  expect_true(f(a = 1))
  expect_error(f(a = 1, 2))
  expect_error(f(1, a = 2))
})
