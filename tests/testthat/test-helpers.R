test_that("dput_to_str works as expected", {
  expect_equal(dput_to_str(c(1, 2, 3)), "c(1, 2, 3)")
})

test_that("classes function works as expected", {
  expect_identical(classes(cars), c("numeric", "numeric"))
})

test_that("has_names", {
  expect_true(all_values_are_named(c(a = 1)))
  expect_false(all_values_are_named(1:3))
  expect_false(all_values_are_named(c(a = 1, 2)))
})

test_that("list2keyvalpairs", {
  expect_error(list2keyvalpairs(list(1, 2, 3)), regexp = "must be named")
  expect_equal(
    list2keyvalpairs(list(a = 1, b = 2)),
    list(list(key = "a", val = 1), list(key = "b", val = 2))
  )
})
