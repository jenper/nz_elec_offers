test_that("Invalid inputs for anomaly_detect fail as expected", {
#  expect_error(anomaly_detect(data=as.factor(NULL)), "Check input is a data.frame object")
  expect_error(anomaly_detect(data=demand), "Invalid x and y columns")
  expect_error(anomaly_detect(data=offers, x='Test', y='VWAP'), "Check x and y are in data") 
  expect_error(anomaly_detect(data=offers, x='ParticipantCode', y='VWAP'), "Check x values are a date")
  expect_error(anomaly_detect(data=offers, x='Datetime', y='ParticipantCode'), "Check y values are a numeric format")
#  expect_s3_class(anomaly_detect(data = offers, x = 'TradingDate', y = 'Megawatts', gsplit=FALSE), 'anomaly', exact = FALSE) #Edit code to allow this to error
  
})

test_that("Valid inputs for anomaly_detect succeed as expected", {
  expect_s3_class(anomaly_detect(), 'anomaly', exact = FALSE)
  expect_s3_class(anomaly_detect(data = offers, x = 'Datetime', y = 'VWAP', gsplit=FALSE), 'anomaly', exact = FALSE)
  expect_s3_class(anomaly_detect(data = offers, x = 'Datetime', y = 'Megawatts', gsplit=FALSE), 'anomaly', exact = FALSE)
# expect_s3_class(anomaly_detect(data = offers, x = Datetime, y = VWAP, gsplit=TRUE), 'anomaly', exact = FALSE)
})

test_that("anomaly_detect results as expected", {
  skip('placeholder')
  # expect_identical() #matching python results here 
})

