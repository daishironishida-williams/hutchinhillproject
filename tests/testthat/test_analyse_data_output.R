library(hutchinhillproject)
context("Mean and sd of allprofs")

data(allprofs)
analyse_data( allprofs )

test_that("the mean and the standard deviation are correct", {
  expect_equal( mean_age, 49.0255754475703 )
  expect_equal( deviation, 12.1557988953172 )
})
