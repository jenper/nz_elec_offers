test_that("Invalid inputs for plot_offer_curve fail as expected", {
expect_error(plot_offer_curve(correlation(data = c(offers,demand, test))), "Failed to merge")
})

test_that("Valid inputs for correlation succeed as expected", {
  expect_silent(correlation(data = c(offers,demand)))
})
