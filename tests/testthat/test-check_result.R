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
