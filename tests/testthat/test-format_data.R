test_that("format_data runs without error", {
  expect_silent(format_data("C:/Users/user/Downloads/20211124_Offers.csv"))
  expect_silent(format_data("C:/Users/user/Downloads/offers"))
})
