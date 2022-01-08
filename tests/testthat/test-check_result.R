test_that("formatting the summary text works", {
  expect_equal(
    format_result_line("hello", TRUE, width = 10),
    "hello....\u2714"
  )
  expect_equal(
    format_result_line("hello", FALSE, width = 10),
    "hello....x"
  )
  expect_equal(
    format_result_line("hellothere!", FALSE, width = 10),
    "he[...]..x"
  )
})

test_that("ResultList basic functions", {
  rs <- result_list(list(a = check_result(TRUE, 'passing'), b = check_result(FALSE, 'failing')))
  expect_identical(as.logical(rs), c(TRUE, FALSE))
  expect_identical(result_list_to_logical(rs), c(a=TRUE, b=FALSE))
  expect_false(all_pass(rs))
  expect_equal(result_list_to_string(rs), "PF")
})
