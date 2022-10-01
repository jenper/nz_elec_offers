# test_that("Invalid inputs for anomaly_detect fail as expected", {
#   expect_error(ymd("Test"), "First argument to ymd must be a date string or a vector of date strings.")
#   expect_error(ymd(c(1:3)), "First argument to ymd must be a date string or a vector of date strings.")
#   expect_error(ymd(c("2011/01/19","argument")), "First argument to ymd must be a date string or a vector of date strings.")
# })
# 
# test_that("Valid inputs for anomaly_detect succeed as expected", {
#   expect_silent(ymd("2021-05-03"))
#   expect_silent(ymd(c("2021-05-03","2022-06-25")))
# })
# 
# test_that("anomaly_detect results as expected", {
#   expect_identical(ymd("2021-05-03"), as.Date("2021-05-03"))
#   expect_identical(ymd(c("2021-05-03","2022-06-25")), as.Date(c("2021-05-03", "2022-06-25")))
# })
