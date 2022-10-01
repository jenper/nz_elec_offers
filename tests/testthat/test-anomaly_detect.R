test_that("Invalid inputs for anomaly_detect fail as expected", {
#  expect_error(anomaly_detect(data=as.factor(NULL)), "Check input is a data.frame object")
  expect_error(anomaly_detect(data=demand), "Check x and y are in data")
  expect_error(anomaly_detect(data=offers, x=ParticipantCode, y=VWAP), "Check x and y are in data")
  expect_error(anomaly_detect(data=offers, x=Datetime, y=ParticipantCode), "Check x and y are in data")
#  expect_error(anomaly_detect(data=offers, x=Datetime, y=Megawatts), "x values are a date and y values are numeric format")
})

test_that("Valid inputs for anomaly_detect succeed as expected", {
  expect_silent(anomaly_detect())
#  expect_silent(anomaly_detect(data = offers, x = Datetime, y = VWAP, gsplit=FALSE))
#  expect_silent(anomaly_detect(data = offers, x = Datetime, y = VWAP, gsplit=TRUE))
#  expect_silent(anomaly_detect(data = offers, x = Datetime, y = Megawatts, gsplit=FALSE))
})

test_that("anomaly_detect results as expected", {
#placeholder
})

