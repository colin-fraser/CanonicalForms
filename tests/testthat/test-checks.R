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

expect_pass <- function(check_result) {
  expect_true(check_result)
}

expect_fail <- function(check_result, snapshot = TRUE) {
  expect_false(check_result)
  if (snapshot) {
    expect_snapshot(check_result)
  }
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
  expect_error(check_descriptor("hello"))
})

test_that("compare_vecs works", {
  v1 <- c("a", "b")
  v2 <- c("a")
  r <- compare_vecs(v1, v2)
  expect_false(r)
  expect_true(startsWith(format(r), "failed"))
  r <- compare_vecs(v1, v1)
  expect_true(r)
  expect_true(startsWith(format(r), "passed"))
})

test_that("built-in checks work", {
  df <- datasets::chickwts
  cf <- chickwts_cf()
  class_check_passing <- run_check(check_class, cf, x = df)
  cols_check_passing <- run_check(check_col_names, cf, x = df)
  classes_check_passing <- run_check(check_col_classes, cf, x = df)
  expect_true(class_check_passing)
  expect_true(cols_check_passing)
  expect_true(classes_check_passing)

  df2 <- tibble::as_tibble(df)
  names(df2) <- c("wt", "feed")
  df2[["feed"]] <- as.character(df2[["feed"]])
  class_check_failing <- run_check(check_class, cf, x = df2)
  cols_check_failing <- run_check(check_col_names, cf, x = df2)
  classes_check_failing <- run_check(check_col_classes, cf, x = df2)
  expect_false(class_check_failing)
  expect_false(cols_check_failing)
  expect_false(classes_check_failing)

  # call checks by name
  dummy_check <- function() TRUE
  cf <- add_checks(cf, dummy = dummy_check)
  expect_true(run_check("dummy", cf))
})

test_that("result list representations", {
  r1 <- check_result(T, "hello")
  r2 <- check_result(F, "goodbye")
  rl <- result_list(list(r1, r2))
  expect_identical(c(r1, r2), c(T, F))
  expect_identical(result_list_to_string(rl), "PF")
})

test_that("add_checks", {
  cf <- dummy_cf()
  check <- function(x) TRUE
  cf <- add_checks(cf, check1 = check)
  expect_true("check1" %in% get_check_names(cf))

  expect_error(add_checks(cf, check))
  expect_error(add_checks(cf, c1 = check, check))
  expect_error(add_checks(cf, check, c1 = check))
  expect_error(add_checks(cf, check1 = check))
})

test_that("delete check", {
  cf <- dummy_cf()
  check <- function(x) TRUE
  cf <- add_checks(cf, c1 = check)
  expect_true("c1" %in% get_check_names(cf))
  cf <- delete_check(cf, "c1")
  expect_false("c1" %in% get_check_names(cf))
})

test_that("test run_all_checks", {
  cf <- dummy_cf()
  obj <- dummy_obj()
  check1 <- function(x) compare_vecs(canonical_col_names(), c("col1", "col2"))
  check2 <- function(x) compare_vecs(canonical_col_classes(), c("class1", "class2"))
  check3 <- function(x) compare_vecs(canonical_col_names(), c("col1", "col2", "col3"))
  cf <- add_checks(cf, check1 = check1, check2 = check2, check3 = check3)
  results <- run_all_checks(obj, cf)
  expect_equal(result_list_to_string(results), "PPF")
})

test_that("no nas test", {
  df <- data.frame(a = c(1, NA), b = c(NA, NA), d = c(1, 2))

  no_nas <- check_no_nas(c("a", "b"))
  expect_false(as.logical(no_nas(df)))
  expect_snapshot(no_nas(df))

  no_nas2 <- check_no_nas(c("a", "d"))
  expect_false(as.logical(no_nas2(df)))
  expect_snapshot(no_nas2(df))

  no_nas3 <- check_no_nas("d")
  expect_true(as.logical(no_nas3(df)))
})

test_that("gt test", {
  df <- data.frame(a = c(-1, 0, NA), b = c(1, 1, NA))
  gtt <- check_greater_than(a = 0)
  expect_false(all_pass(gtt(df)))
  gtt2 <- check_greater_than(a = -2)
  expect_true(as.logical(gtt2(df)))
  gtt3 <- check_greater_than(a = -1, .strict = FALSE)
  expect_true(as.logical(gtt3(df)))

  cf <- extract_canonical_form(df) %>%
    add_checks(gt = gtt2)

  expect_true(is_canonical(df, cf, verbose = F))

  cf <- extract_canonical_form(df) %>%
    add_checks(gt = gtt)

  expect_false(is_canonical(df, cf, verbose = F))

  expect_snapshot(check_canonical(df, cf))
})

test_that("less than", {
  df <- data.frame(a = c(-1, 0, NA), b = c(1, 1, NA))
  lt1 <- check_less_than(a = 1)(df)
  expect_true(all_pass(lt1))
  lt2 <- check_less_than(a = 0, .strict = FALSE)
  expect_true(lt2(df))
  lt3 <- check_less_than(a = 0, .strict = TRUE)
  expect_false(lt3(df))
  lt4 <- check_less_than(a = -2)
  expect_false(lt4(df))
  expect_snapshot(lt1(df))
  expect_snapshot(lt2(df))
  expect_snapshot(lt3(df))
  expect_snapshot(lt4(df))
})

test_that("logical_vector_to_test_result", {
  test_fn <- function(vec, msg = "hello") {
    cr <- named_logical_vector_to_check_result(vec, msg)
    as.logical(cr)
  }
  expect_true(test_fn(c(a = T)))
  expect_false(test_fn(c(a = F)))
  expect_true(test_fn(c(a = T, b = T)))
  expect_false(test_fn(c(a = T, b = F)))
})

test_that("dots to bounds", {
  expect_equal(dots_to_bounds(a = c(1, 2), b = c(3, 4)), list(lower = c(a = 1, b = 3), upper = c(a = 2, b = 4)))
})

test_that("check between", {
  df <- data.frame(a = c(1, 2, 3, NA), b = c(4, 5, 6, 7))
  cb1 <- check_between(a = c(1, 4), b = c(3, 7))
  expect_pass(cb1(df))
  cb2 <- check_between(a = c(1, 4), b = c(3, 7), .strict_lower = TRUE)
  expect_fail(cb2(df))
  cb3 <- check_between(a = c(1, 4), b = c(3, 7), .strict_upper = TRUE)
  expect_fail(cb3(df))
  cb4 <- check_between(a = c(1, 4), b = c(3, 7), .strict_lower = TRUE, .strict_upper = TRUE)
  expect_fail(cb4(df))
})

test_that("test internal factor levels checker", {
  fct <- as.factor(month.abb)
  expect_pass(.check_col_factor_levels(sort(month.abb), fct))
  fct2 <- as.factor(month.abb[1:5])
  expect_fail(.check_col_factor_levels(sort(month.abb), fct2))
})

test_that("check factor levels", {
  df <- data.frame(a = as.factor(month.abb))
  df2 <- data.frame(a = month.abb)
  df3 <- data.frame(a = as.factor(month.abb[1:11]))
  check <- check_factor_levels(a = sort(month.abb))
  expect_true(check(df))
  expect_fail(check(df2))
  expect_fail(check(df3))

  cf <- extract_canonical_form(df)
  cf <- add_checks(cf, check_factor_levels = check)
  expect_snapshot(is_canonical(df, cf))
  expect_snapshot(is_canonical(df2, cf))
  expect_snapshot(is_canonical(df3, cf))
})

test_that("apply function to cols", {
  x <- data.frame(a = c(1, 2), b = c(3, 4))
  args <- list(a = 3, b = 8)
  f <- function(x, y) sum(x) == y
  expect_equal(apply_function_to_cols(x, f, args), list(a = TRUE, b = FALSE))
})
