context("libname Tests")

base_path <- "c:\\packages\\libr\\tests\\testthat\\data"

base_path <- "./data"

test_that("libname() function works as expected with csv.", {
  
  lib <- libname(base_path, filter = "csv")
  
  
  expect_equal(class(lib)[[1]], "lib")
  expect_equal(length(lib), 2) 
  expect_equal(nrow(lib[[1]]), 10)
  expect_equal(ncol(lib[[1]]), 9)
  expect_equal(nrow(lib[[2]]), 2)
  expect_equal(ncol(lib[[2]]), 9)
})
  

test_that("libname() function works as expected with rds", {
  
  lib <- libname(base_path, filter = "rds")
  
  
  expect_equal(class(lib)[[1]], "lib")
  expect_equal(length(lib), 2) 
  expect_equal(nrow(lib[[1]]), 10)
  expect_equal(ncol(lib[[1]]), 9)
  expect_equal(nrow(lib[[2]]), 2)
  expect_equal(ncol(lib[[2]]), 9)
})



test_that("libname() function works as expected with xls", {
  
  lib <- libname(base_path, filter = "xls")
  
  
  expect_equal(class(lib)[[1]], "lib")
  expect_equal(length(lib), 2) 
  expect_equal(nrow(lib[[1]]), 10)
  expect_equal(ncol(lib[[1]]), 9)
  expect_equal(nrow(lib[[2]]), 2)
  expect_equal(ncol(lib[[2]]), 9)
})

test_that("libname() function works as expected with xlsx", {
  
  lib <- libname(base_path, filter = "xlsx")
  
  expect_equal(class(lib)[[1]], "lib")
  expect_equal(length(lib), 2) 
  expect_equal(nrow(lib[[1]]), 10)
  expect_equal(ncol(lib[[1]]), 9)
  expect_equal(nrow(lib[[2]]), 2)
  expect_equal(ncol(lib[[2]]), 9)
})

test_that("libname() function works as expected with sas7bdat", {
  
  lib <- libname(base_path, filter = "sas7bdat")
  
  expect_equal(class(lib)[[1]], "lib")
  expect_equal(length(lib), 2) 
  expect_equal(nrow(lib[[1]]), 10)
  expect_equal(ncol(lib[[1]]), 9)
  expect_equal(nrow(lib[[2]]), 2)
  expect_equal(ncol(lib[[2]]), 9)
})


test_that("libname() function works as expected with multiple data formats", {
  
  
  lib = suppressWarnings(libname(base_path))
  #lib = libname(base_path)
  
  expect_equal(class(lib)[[1]], "lib")
  expect_equal(length(lib), 2) 
  expect_equal(nrow(lib[[1]]), 10)
  expect_equal(ncol(lib[[1]]), 9)
  expect_equal(nrow(lib[[2]]), 2)
  expect_equal(ncol(lib[[2]]), 9)
  
})


