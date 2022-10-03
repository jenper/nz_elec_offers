test_that("Invalid inputs for plot_offer_curve fail as expected", {
  p = ggplot2::ggplot(data=offers)
  expect_error(plot_offer_curve(data = p), "Check input is a data.frame object")
  expect_error(plot_offer_curve(data = offers, x='Test', y='DollarsPerMegawattHour'), "columns not found in data. check column names")
  expect_error(plot_offer_curve(data = offers,  x='PointOfConnection', y='DollarsPerMegawattHour'), "argument x needs to be of numeric type")
  expect_error(plot_offer_curve(data = offers, x='Megawatts', y='DollarsPerMegawattHour', group='Test'), "not found in data. check column names")
  expect_error(plot_offer_curve(data = offers,  x=Megawatts, y=DollarsPerMegawattHour), "not found")
  expect_visible(plot_offer_curve(data=demand, 'MegawattHours', 'TradingDate'))
})

test_that("Valid inputs for plot_offer_curve succeed as expected", {
  expect_s3_class(plot_offer_curve(data = offers, x='Megawatts', y='DollarsPerMegawattHour'), 'ggplot')
  expect_s3_class(plot_offer_curve(data = offers, x='Megawatts', y='DollarsPerMegawattHour', group='ParticipantCode'), 'ggplot')
  expect_s3_class(plot_offer_curve(data = offers, x='Megawatts', y='DollarsPerMegawattHour', group='TradingPeriod'), 'ggplot')
  expect_s3_class(plot_offer_curve(data = offers, x='DollarsPerMegawattHour', y='Megawatts', group='ParticipantCode'), 'ggplot')
})

test_that("plot_offer_curve plots as expected", {
  p <- plot_offer_curve(data = offers, x='Megawatts', y='DollarsPerMegawattHour')
  vdiffr::expect_doppelganger("default_curve", p)
})
