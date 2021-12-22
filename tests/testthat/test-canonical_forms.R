test_that("formatter works", {
  cf <- extract_canonical_form(cars)
  expected <- "Canonical Form\n  speed: numeric\n  dist: numeric"
  expect_equal(format(cf), expected)
})

