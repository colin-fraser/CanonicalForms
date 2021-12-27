test_that("built in checks return correct results", {
  cf <- extract_canonical_form(dplyr::starwars)
  sw <- dplyr::starwars

  # test class checker
  expect_true(check_class(cf)(sw))
  expect_false(check_class(cf)(as.data.frame(sw)))

  # test colnames checker
  expect_true(check_colnames(cf)(sw))
  expect_false(check_colnames(cf)(dplyr::rename(sw, Name = name)))

  # test col_classes checker
  expect_true(check_col_classes(cf)(sw))
  expect_false(check_col_classes(cf)(dplyr::mutate(sw, height = as.character(height))))
})

test_that("adding checks", {
  cf <- extract_canonical_form(dplyr::starwars)
  cf <- add_check(cf, "max_height" = function(x, form) all(x$height < 300, na.rm = TRUE))
  expect_true(is_canonical(dplyr::starwars, cf, verbose = FALSE))
  cf <- add_check(cf, "min_height" = function(x, form) all(x$height > 100, na.rm = TRUE))
  expect_false(is_canonical(dplyr::starwars, cf, verbose = FALSE))
  expect_message(is_canonical(dplyr::starwars, cf), regexp = "\\bmin_height.+\u274C")
})

test_that("verifying checks", {
  bad_checks <- list(abc = function(x) 1)
  good_checks <- list(abc = function(x, form) 1)
  expect_false(verify_checks_are_well_formed(bad_checks))
  expect_true(verify_checks_are_well_formed(good_checks))
})

test_that("built-in check makers", {
  pass <- make_check_no_nas(c("name", "films"))
  fail <- make_check_no_nas(c("name", "height"))
  expect_true(pass(dplyr::starwars, NULL))
  expect_false(fail(dplyr::starwars, NULL))
})
