dummy_cf <- function() {
  canonical_form("class", c("col1", "col2"), c("class1", "class2"), add_default_checks = FALSE)
}

dummy_obj <- function() {
  structure(
    list(
      colnames = c("col1", "col2"),
      colclasses = c("class1", "class2")
    ),
    class = "class"
  )
}

chickwts_cf <- function() {
  canonical_form(
    object_class = "data.frame", col_names = c("weight", "feed"),
    col_classes = c("numeric", "factor"), add_default_checks = FALSE
  )
}

test_that("get_from_caller_env_recursive works", {
  e1 <- rlang::env(a = "a")
  get_a <- function() {
    get_from_caller_env_recursive(nm = "a")
  }
  get_a_2 <- function() {
    get_a()
  }

  expect_equal(exec(get_a, .env = e1), "a")
  expect_equal(exec(get_a_2, .env = e1), "a")
  expect_error(get_a())
})

test_that("determine_env correctly determines an environment", {
  cf <- dummy_cf()
  expect_identical(determine_env(NULL), rlang::current_env())
  expect_identical(determine_env(cf), cf$check_env)
})

test_that("getting properties from a CanonicalForm", {
  cf <- dummy_cf()
  env <- get_check_env(cf)
  f <- function() get_property(".col_names")
  expect_equal(run_in_cf_env(cf, get_property, nm = ".col_names"), c("col1", "col2"))
  expect_equal(run_in_cf_env(cf, f), c("col1", "col2"))
})

test_that("main canonical helper functions work", {
  cf <- dummy_cf()
  f <- function() {
    list(
      object_class = canonical_object_class(),
      col_names = canonical_col_names(),
      col_classes = canonical_col_classes()
    )
  }
  expected <- list(
    object_class = "class",
    col_names = c("col1", "col2"),
    col_classes = c("class1", "class2")
  )
  expect_equal(run_in_cf_env(cf, f), expected)
  expect_equal(canonical_object_class(cf), "class")
  expect_equal(canonical_col_names(cf), c("col1", "col2"))
  expect_equal(canonical_col_classes(cf), c("class1", "class2"))
  expect_error(canonical_col_classes())
})

test_that("running a dummy check with no argument works", {
  cf <- dummy_cf()
  check <- function() canonical_object_class() == "class"
  expect_true(run_check(check, cf))
})

test_that("running a check with parameter works", {
  cf <- dummy_cf()
  check <- function(x) canonical_object_class() == x
  expect_true(run_check(check, cf, x = "class"))
  expect_false(run_check(check, cf, x = "d"))
})

test_that("running an actual check with a dataset works", {
  cf <- chickwts_cf()
  df <- datasets::chickwts
  check <- function(x) {
    identical(
      levels(x[["feed"]]),
      c("casein", "horsebean", "linseed", "meatmeal", "soybean", "sunflower")
    )
  }

  expect_true(run_check(check, cf, x = df))
})

test_that("check descriptor", {
  r <- compare_vecs(1, 2)
  expect_equal(check_descriptor(r), "failed")
  r <- compare_vecs(1, 1)
  expect_equal(check_descriptor(r), "passed")
})

test_that("compare_vecs works", {
  v1 <- c("a", "b")
  v2 <- c("a")
  r <- compare_vecs(v1, v2)
  expect_false(as.logical(r))
  expect_true(startsWith(format(r), "failed"))
  r <- compare_vecs(v1, v1)
  expect_true(as.logical(r))
  expect_true(startsWith(format(r), "passed"))
})

test_that("built-in checks work", {
  df <- datasets::chickwts
  cf <- chickwts_cf()
  class_check_passing <- run_check(check_class, cf, x = df)
  cols_check_passing <- run_check(check_col_names, cf, x = df)
  classes_check_passing <- run_check(check_col_classes, cf, x = df)
  expect_true(as.logical(class_check_passing))
  expect_true(as.logical(cols_check_passing))
  expect_true(as.logical(classes_check_passing))

  df2 <- tibble::as_tibble(df)
  names(df2) <- c("wt", "feed")
  df2[["feed"]] <- as.character(df2[["feed"]])
  class_check_failing <- run_check(check_class, cf, x = df2)
  cols_check_failing <- run_check(check_col_names, cf, x = df2)
  classes_check_failing <- run_check(check_col_classes, cf, x = df2)
  expect_false(as.logical(class_check_failing))
  expect_false(as.logical(cols_check_failing))
  expect_false(as.logical(classes_check_failing))
})

test_that("result list representations", {
  r1 <- check_result(T, "hello")
  r2 <- check_result(F, "goodbye")
  rl <- list(r1, r2)
  expect_identical(result_list_to_logical(rl), c(T, F))
  expect_identical(result_list_to_string(rl), "PF")
})

test_that("test run_all_checks", {
  cf <- dummy_cf()
  obj <- dummy_obj()
  check1 <- function(x) compare_vecs(canonical_col_names(), c("col1", "col2"))
  check2 <- function(x) compare_vecs(canonical_col_classes(), c("class1", "class2"))
  check3 <- function(x) compare_vecs(canonical_col_names(), c("col1", "col2", "col3"))
  cf <- add_check(cf, check1 = check1, check2 = check2, check3 = check3)
  results <- run_all_checks(obj, cf)
  expect_equal(result_list_to_string(results), "PPF")
})

#
# test_that("getting properties works", {
#   dummy_cf <- canonical_form("a", c("b", "c"), c("d", "e"), add_default_checks = FALSE)
#   check_class <- function(x) {
#     canonical_object_class() == "a"
#   }
#   expect_true(run_check(NULL, check_class, dummy_cf))
#
#   check_colnames <- function(x) {
#     identical(canonical_col_names(), c("b", "c"))
#   }
#   expect_true(run_check(NULL, check_colnames, dummy_cf))
#
#   check_col_classes <- function(x) {
#     identical(canonical_col_classes(), c("d", "e"))
#   }
#   expect_true(run_check(NULL, check_col_classes, dummy_cf))
#
# }
# )
#
# test_that("Adding a check works", {
#   dummy_check <- function(x) canonical_object_class() == 'a'
#   cf <- canonical_form("a", c("b", "c"), c("d", "e"), add_default_checks = FALSE)
#   cf <- add_check(cf, dummy_check = dummy_check)
#   expect_true("dummy_check" %in% names(cf$checks))
#
# })
#
#
# test_that("Running a check works", {
#   dummy_cf <- canonical_form("a", c("b", "c"), c("d", "e"), add_default_checks = FALSE)
#   dummy_check <- function(x) canonical_object_class() == 'a'
#   expect_true(run_check(NULL, dummy_check, dummy_cf))
# })
#
# test_that("Running checks works", {
#   dummy_cf <- canonical_form("a", c("b", "c"), c("d", "e"), add_default_checks = FALSE)
#   check_class <- function(x) canonical_object_class() == 'a'
#   check_names <- function(x) identical(canonical_col_names(), c("b", "c"))
#   dummy_cf <- add_check(dummy_cf, check_class = check_class, check_names = check_names)
#   expect_true(all(run_all_checks(NULL, dummy_cf)))
# })
