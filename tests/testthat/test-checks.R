test_that("built in checks", {
  cf <- extract_canonical_form(dplyr::starwars)
  sw <- dplyr::starwars

  # test class checker
  expect_true(check_class(sw, cf))
  expect_false(check_class(as.data.frame(sw), cf))

  # test colnames checker
  expect_true(check_colnames(sw, cf))
  expect_false(check_colnames(dplyr::rename(sw, Name = name), cf))

  # test col_classes checker
  expect_true(check_col_classes(sw, cf))
  expect_false(check_col_classes(dplyr::mutate(sw, height = as.character(height)), cf))
})

test_that("adding checks", {
  cf <- extract_canonical_form(dplyr::starwars)
  cf <- add_check(cf, "max_height" = function(x) all(x$height < 300, na.rm = TRUE))
  expect_true(is_canonical(dplyr::starwars, cf, verbose = FALSE))
  cf <- add_check(cf, "min_height" = function(x) all(x$height>100, na.rm = TRUE))
  expect_false(is_canonical(dplyr::starwars, cf, verbose = FALSE))
  expect_message(is_canonical(dplyr::starwars, cf), regexp = "\\bmin_height.+\u274C")
}
)
