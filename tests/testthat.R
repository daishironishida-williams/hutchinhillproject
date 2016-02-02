library(testthat)
library(hutchinhillproject)

test_that("the mean and the standard deviation are correct", {
  read_data()
  expect_equal( mean_age, 49.0255754475703 )
  expect_equal( deviation, 12.1557988953172 )
})
