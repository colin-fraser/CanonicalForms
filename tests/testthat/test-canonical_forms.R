test_that("formatter works", {
  cf <- extract_canonical_form(cars)
  expected <- "Canonical Form for object of class: \"data.frame\"\n  speed: numeric\n  dist: numeric"
  expect_equal(format(cf), expected)
})

test_that("Extracting canonical forms for simple data.frame", {
  extracted <- extract_canonical_form(cars)
  expect_equal(canonical_col_names(extracted), c("speed", "dist"))
  expect_equal(canonical_col_classes(extracted), c("numeric", "numeric"))
  expect_equal(canonical_object_class(extracted), "data.frame")
})

test_that("Extracting canonical forms for tbl_df", {
  extracted <- extract_canonical_form(dplyr::starwars)
  expect_equal(canonical_col_names(extracted), c(
    "name", "height", "mass", "hair_color", "skin_color", "eye_color",
    "birth_year", "sex", "gender", "homeworld", "species", "films",
    "vehicles", "starships"
  ))
  expect_equal(canonical_col_classes(extracted), c(
    "character", "integer", "numeric", "character", "character",
    "character", "numeric", "character", "character", "character",
    "character", "list", "list", "list"
  ))
})

test_that("Get property works", {
  cf <- canonical_form("a", c("b", "c"), c("d", "e"), add_default_checks = FALSE)
  expect_error(get_property(".object_class"))
  expect_equal(run_in_cf_env(cf, function() get_property(".object_class")), "a")
  expect_equal(get_property(".object_class", cf), "a")
})

test_that("Check if dataset is canonical", {
  cf <- extract_canonical_form(cars)
  expect_true(is_canonical(cars, cf, verbose = FALSE))
  expect_false(is_canonical(dplyr::rename(cars, SPEED = speed), cf, verbose = FALSE))
  expect_false(is_canonical(tibble::as_tibble(cars), cf, verbose = FALSE))
  expect_false(is_canonical(dplyr::mutate(cars, speed = is.character(speed)), cf, verbose = FALSE))
})
