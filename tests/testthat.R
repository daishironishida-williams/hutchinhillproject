library(testthat)
library(hutchinhillproject)

test_that({
  expect_equal( read_data(), 49.02558 )
})
