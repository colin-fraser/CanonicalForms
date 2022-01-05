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

test_that("conjuction works", {
          pass1 <- check_result(TRUE, '')
          fail1 <- check_result(FALSE, rlang::format_error_bullets(c('a', 'b')))
          fail2 <- check_result(FALSE, rlang::format_error_bullets(c('x', 'y')))
          expect_true(as.logical(conjunction(pass1, pass1)))
          expect_false(as.logical(conjunction(pass1, fail1)))
          expect_false(as.logical(conjunction(fail1, pass1)))
          expect_false(as.logical(conjunction(fail1, fail2)))
          expect_snapshot(conjunction(pass1, fail1))
          expect_snapshot(conjunction(fail1, fail2))
}
)
