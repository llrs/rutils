test_that("join_messages works", {

  x <- "whatever https://google.com"
  xx <- "Google url is https://google.com or http://google.com"
  y <- rep_len("A", 501L)
  yy <- paste(y, collapse = "")
  expect_length(join_messages(c(x, y, xx), 500L, ". "), 4)
  expect_length(join_messages(c(x, yy, xx), 500L, ". "), 3)
  expect_length(join_messages(c(yy, x, xx), 500L, ". "), 2)
  expect_length(join_messages(c(x, xx, yy), 500L, ". "), 2)
  expect_length(join_messages(c(x, x, xx, yy), 500L, ". "), 2)
})

test_that("Splits correctly messages", {
  message <- c(sample("dlksjflñkasd.", size = rnorm(1, mean = 41), replace = TRUE),
  "df sdjfñl jfñasljf asdlñfj https://google.com",
  "fasdlkñj fsfj sd https://google2.com")
  expect_false(anyNA(llrs_send_toot(paste0(message, collapse = ". "))))
})
