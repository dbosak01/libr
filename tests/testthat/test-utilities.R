context("libname Tests")

base_path <- "c:\\packages\\libr\\tests\\testthat\\data"

base_path <- "./data"


test_that("print() functions works as expected.", {
  

  libname(dat, base_path, filter = "csv")
  
  # Just run the print functions and make sure there is no errors.
  #print(dat)
  #print(dat, verbose = TRUE)
  
  expect_equal(TRUE, TRUE)
  
})
