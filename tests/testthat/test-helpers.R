test_that("dput_to_str works as expected", {
  expect_equal(dput_to_str(c(1, 2, 3)), "c(1, 2, 3)")
})
