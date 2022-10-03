test_that("Invalid inputs for create_report fail as expected", {
  expect_error(create_report(data = offers, response = 'Test'), "not found in data")
  #expect_error(create_report(data = demand, response = 'MegawattHours'), "") #response not being passed to render? 
})

test_that("Valid inputs for create_report succeed as expected", {
  expect_invisible(create_report())
  expect_invisible(create_report(data = offers, response = 'DollarsPerMegawattHour', report_title = "Report", output_dir = getwd(), output_file = "report.html"))
  expect_invisible(create_report(data = offers, response = 'Megawatts'))
  expect_invisible(create_report(output_dir='Test'))
})
 
test_that("create_report results as expected", {
  #expect_invisible(create_report())
  expect_output(create_report())
})
