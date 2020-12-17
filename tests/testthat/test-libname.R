context("Libname Tests")

base_path <- "c:\\packages\\libr\\tests\\testthat\\data"

base_path <- "./data"

test_that("libname() function works as expected with csv.", {
  
  libname(dat, base_path, engine = "csv")
  
  expect_equal(class(dat)[[1]], "lib")
  expect_equal(length(dat), 2) 
  expect_equal(nrow(dat[[1]]), 10)
  expect_equal(ncol(dat[[1]]), 9)
  expect_equal(nrow(dat[[2]]), 2)
  expect_equal(ncol(dat[[2]]), 9)
})
  

test_that("libname() function works as expected with rds", {
  
  libname(dat, base_path, engine = "rds")
  
  
  expect_equal(class(dat)[[1]], "lib")
  expect_equal(length(dat), 2) 
  expect_equal(nrow(dat[[1]]), 10)
  expect_equal(ncol(dat[[1]]), 9)
  expect_equal(nrow(dat[[2]]), 2)
  expect_equal(ncol(dat[[2]]), 9)
})



test_that("libname() function works as expected with xls", {
  
  libname(dat, base_path, engine = "xls")
  
  
  expect_equal(class(dat)[[1]], "lib")
  expect_equal(length(dat), 2) 
  expect_equal(nrow(dat[[1]]), 10)
  expect_equal(ncol(dat[[1]]), 9)
  expect_equal(nrow(dat[[2]]), 2)
  expect_equal(ncol(dat[[2]]), 9)
})

test_that("libname() function works as expected with xlsx", {
  
  libname(dat, base_path, engine = "xlsx")
  
  expect_equal(class(dat)[[1]], "lib")
  expect_equal(length(dat), 2) 
  expect_equal(nrow(dat[[1]]), 10)
  expect_equal(ncol(dat[[1]]), 9)
  expect_equal(nrow(dat[[2]]), 2)
  expect_equal(ncol(dat[[2]]), 9)
})

test_that("libname() function works as expected with sas7bdat", {
  
  libname(dat, base_path, engine = "sas7bdat")
  
  expect_equal(class(dat)[[1]], "lib")
  expect_equal(length(dat), 2) 
  expect_equal(nrow(dat[[1]]), 10)
  expect_equal(ncol(dat[[1]]), 9)
  expect_equal(nrow(dat[[2]]), 2)
  expect_equal(ncol(dat[[2]]), 9)
})

test_that("libname() function works as expected with dbf", {
  
  tmp <- tempdir()
  libname(dat, tmp, "dbf")
  
  lib_add(dat, mtcars)
  
  libname(dat2, tmp, "dbf")
  
  
  expect_equal(length(dat2), 1) 
  expect_equal(nrow(dat2[[1]]), 32)
  expect_equal(ncol(dat2[[1]]), 11)

  lib_delete(dat)
  lib_delete(dat2)
})

test_that("libname() function works as expected with xpt", {
  
  tmp <- tempdir()
  libname(dat, tmp, "xpt")
  
  lib_add(dat, mtcars)
  
  libname(dat2, tmp, "xpt")
  
  
  expect_equal(length(dat2), 1) 
  expect_equal(nrow(dat2[[1]]), 32)
  expect_equal(ncol(dat2[[1]]), 11)
  
  lib_delete(dat)
  lib_delete(dat2)
})


# test_that("libname() function works as expected with multiple data formats", {
#   
#   
#   suppressWarnings(libname(dat, base_path))
#   
#   expect_equal(class(dat)[[1]], "lib")
#   expect_equal(length(dat), 2) 
#   expect_equal(nrow(dat[[1]]), 10)
#   expect_equal(ncol(dat[[1]]), 9)
#   expect_equal(nrow(dat[[2]]), 2)
#   expect_equal(ncol(dat[[2]]), 9)
#   
# })

# test_that("libname() function works as expected with multiple data formats", {
#   
#   tmp <- tempdir()
#   libname(dat, tmp)
#   
#   lib_add(dat, mtcars, "xlsx")
#   lib_add(dat, iris, "csv")
#   lib_add(dat, beaver1, "dbf")
#   
#   libname(dat2, tmp, c("xlsx", "dbf", "rds"))
#   
#   
#   
#   expect_equal(class(dat)[[1]], "lib")
#   expect_equal(length(dat), 2) 
#   expect_equal(nrow(dat[[1]]), 10)
#   expect_equal(ncol(dat[[1]]), 9)
#   expect_equal(nrow(dat[[2]]), 2)
#   expect_equal(ncol(dat[[2]]), 9)
#   
# })

test_that("libname() function works as expected with new directory", {
  
  fp <- file.path(tempdir(), "fork")
  
  libname(fork, fp)
  
  expect_equal(file.exists(fp), TRUE)
  
  if (file.exists(fp))
    unlink(fp, recursive = TRUE)
  
})


test_that("libname() function works as expected within a function.", {
  
  libname(dat, tempdir())
  
  lib_load(dat)
  
  dat.cars <- mtcars
  dat.iris <- iris
  
  lib_unload(dat)
  
  expect_equal(length(dat), 2)
  
  
})

test_that("libname() function works as expected within multiple functions.", {
  
  libname(dat, base_path, "csv")
  
  
  func1 <- function() {
    libname(dat, tempdir())
    
    lib_add(dat, mtcars)
    
    ret <- names(dat)
  
    lib_delete(dat)
    
    return(ret)
  }
  
  
  func2 <- function() {
    
    alt <- file.path(tempdir(), "data2")
    libname(dat, alt)
    
    lib_add(dat, iris, beaver1)
    
    ret <- names(dat)

    
    lib_delete(dat)
    
    return(ret)
  }
  
  res1 <- func1()
  res2 <- func2()
  
  
  expect_equal(names(dat), c("demo_studya", "demo_studyb")) 
  expect_equal(res1, c("mtcars")) 
  expect_equal(res2, c("iris", "beaver1")) 
})

test_that("libname() function works as expected with nested functions.", {
  
  libname(dat, base_path, "csv")
  
  
  func1 <- function() {
    libname(dat, tempdir())
    
    lib_add(dat, mtcars)
    
    ret <- names(dat)

    lib_delete(dat)
    
    return(ret)
  }
  
  func2 <- function() {
    
    alt <- file.path(tempdir(), "data2")
    libname(dat, alt)
    
    lib_add(dat, iris, beaver1)
    
    ret <- list()
    ret[["func2"]] <- names(dat)
    ret[["func1"]] <- func1()
    
    lib_delete(dat)
    
    return(ret)
  }
  
  res <- func2()

  expect_equal(names(dat), c("demo_studya", "demo_studyb")) 
  expect_equal(res[["func1"]], c("mtcars")) 
  expect_equal(res[["func2"]], c("iris", "beaver1")) 
})

test_that("libname() function works as expected with filter", {
  
  libname(dat, file.path( base_path, "SDTM"), 
          engine = "sas7bdat", filter = c("ae", "dm", "lb", "vs"))
  
  d <- lib_info(dat)[["Name"]]
  
  expect_equal(d, c("ae", "dm", "lb", "vs"))
  
  libname(dat, file.path( base_path, "SDTM"), 
          engine = "sas7bdat", filter = c("*e"))
  
  d <- lib_info(dat)[["Name"]]
  
  expect_equal(d, c("ae", "ie", "pe")) 
  
  libname(dat, file.path( base_path, "SDTM"), 
          engine = "sas7bdat", filter = c("d*", "ex", "qs"))
  
  d <- lib_info(dat)[["Name"]]
  
  expect_equal(d, c("da", "dm", "ds", "ds_ihor", "ex", "qs")) 
})
