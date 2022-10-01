## incorrect input
test_that("wrong input to format_data throws error", {
  expect_error(format_data(NULL), 'documentation')
})
