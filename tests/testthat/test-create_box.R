test_that("create_box works", {
  box10 <- llrs_box(10)
  expect_equal(NROW(box10), 100L)
  box9 <- llrs_box(9)
  expect_equal(NROW(box9), 81L)
})
