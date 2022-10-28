test_that("Invalid inputs for change_points fail as expected", {
  expect_error(change_points(data=offers,x='Dollars', group='TradingDate'), "columns not found in data. check column names")
  expect_error(change_pointst(data=offers,x=c('DollarsPerMegawattHours', 'Megawatts'), group='TradingDate'), "Check additional arguemnts are single values") 
  expect_error(change_points(data=offers,x='PointOfConnectionCode', group='TradingDate'), "Check x is numeric type")
})

# test_that("Valid inputs for change_points succeed as expected", {
#   expect_s3_class(change_points(data=offers,x='DollarsPerMegawattHour', group='TradingDate'), "offers_changepoint")
#   expect_s3_class(change_points(data=offers,x='Megawatts', group='TradingDate'), "offers_changepoint")
#   expect_s3_class(change_points(data=demand,x='MegawattHours', group='TradingDate'), "offers_changepoint")
# })

test_that("change_points results as expected", {
  expect_equal(change_points(data=offers,x='DollarsPerMegawattHour', group='TradingDate')$changepoint, c(5,14,17,30)) #matching pkg results here
  expect_equal(change_points(data=demand,x='MegawattHours', group='TradingDate')$changepoint, c(5,29)) #matching pkg results here 
  
})

