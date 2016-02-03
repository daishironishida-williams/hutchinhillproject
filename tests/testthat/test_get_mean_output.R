library(hutchinhillproject)
context("Info on each division")

analyse_data( allprofs )
get_mean()

test_that("the mean and the standard deviation are correct", {
  expect_equal( div1_mean, 50.4503816793893 )
  expect_equal( div2_mean, 48.8620689655172 )
  expect_equal( div3_mean, 48.6756756756757 )
  expect_equal( pe_mean, 45.1212121212121)
})
