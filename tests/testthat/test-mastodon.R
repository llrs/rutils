test_that("join_messages works", {

  x <- "whatever https://google.com"
  xx <- "Google url is https://google.com or http://google.com"
  y <- rep_len("A", 501L)
  yy <- paste(y, collapse = "")
  expect_length(join_messages(c(x, y, xx), 500L, ". "), 4)
  expect_length(join_messages(c(x, yy, xx), 500L, ". "), 3)
  expect_length(join_messages(c(yy, x, xx), 500L, ". "), 3)
  join_messages(c(x, xx, yy), 500L, ". ")
  join_messages(c(x, x, xx, yy), 500L, ". ")
})
