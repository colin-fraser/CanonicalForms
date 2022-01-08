test_that("is_named", {
  l1 <- list(a = 1, b = 2)
  l2 <- list(a = 1, 2)
  l3 <- list(1, 2)
  expect_true(is_named(l1))
  expect_false(is_named(l2))
  expect_false(is_named(l3))
})
