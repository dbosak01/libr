context("libname Tests")

base_path <- "c:\\packages\\libr\\tests\\testthat\\data"

base_path <- "./data"

test_that("libname() function works as expected with csv.", {
  
  libname(dat, base_path, type = "csv")
  
  expect_equal(class(dat)[[1]], "lib")
  expect_equal(length(dat), 2) 
  expect_equal(nrow(dat[[1]]), 10)
  expect_equal(ncol(dat[[1]]), 9)
  expect_equal(nrow(dat[[2]]), 2)
  expect_equal(ncol(dat[[2]]), 9)
})
  

test_that("libname() function works as expected with rds", {
  
  libname(dat, base_path, type = "rds")
  
  
  expect_equal(class(dat)[[1]], "lib")
  expect_equal(length(dat), 2) 
  expect_equal(nrow(dat[[1]]), 10)
  expect_equal(ncol(dat[[1]]), 9)
  expect_equal(nrow(dat[[2]]), 2)
  expect_equal(ncol(dat[[2]]), 9)
})



test_that("libname() function works as expected with xls", {
  
  libname(dat, base_path, type = "xls")
  
  
  expect_equal(class(dat)[[1]], "lib")
  expect_equal(length(dat), 2) 
  expect_equal(nrow(dat[[1]]), 10)
  expect_equal(ncol(dat[[1]]), 9)
  expect_equal(nrow(dat[[2]]), 2)
  expect_equal(ncol(dat[[2]]), 9)
})

test_that("libname() function works as expected with xlsx", {
  
  libname(dat, base_path, type = "xlsx")
  
  expect_equal(class(dat)[[1]], "lib")
  expect_equal(length(dat), 2) 
  expect_equal(nrow(dat[[1]]), 10)
  expect_equal(ncol(dat[[1]]), 9)
  expect_equal(nrow(dat[[2]]), 2)
  expect_equal(ncol(dat[[2]]), 9)
})

test_that("libname() function works as expected with sas7bdat", {
  
  libname(dat, base_path, type = "sas7bdat")
  
  expect_equal(class(dat)[[1]], "lib")
  expect_equal(length(dat), 2) 
  expect_equal(nrow(dat[[1]]), 10)
  expect_equal(ncol(dat[[1]]), 9)
  expect_equal(nrow(dat[[2]]), 2)
  expect_equal(ncol(dat[[2]]), 9)
})


test_that("libname() function works as expected with multiple data formats", {
  
  
  suppressWarnings(libname(dat, base_path))
  
  expect_equal(class(dat)[[1]], "lib")
  expect_equal(length(dat), 2) 
  expect_equal(nrow(dat[[1]]), 10)
  expect_equal(ncol(dat[[1]]), 9)
  expect_equal(nrow(dat[[2]]), 2)
  expect_equal(ncol(dat[[2]]), 9)
  
})

test_that("libname() function works as expected with new directory", {
  
  fp <- file.path(base_path, "fork")
  
  libname(fork, fp)
  
  expect_equal(file.exists(fp), TRUE)
  
  if (file.exists(fp))
    unlink(fp, recursive = TRUE)
  
})


