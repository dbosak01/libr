context("libname Tests")

base_path <- "c:\\packages\\libr\\tests\\testthat\\data"

base_path <- "./data"


test_that("lib_load() and lib_unload() functions works as expected.", {
  
  # Should be able to create new directory if desired
  #libname(dat, file.path(base_path, "forker"))
  
  
  libname(dat, base_path, type = "csv")

  
  # Should not get an error here
  lib_unload(dat)

  lib_load(dat)
  
  expect_equal(nrow(dat.demo_studya), 10)
  expect_equal(nrow(dat.demo_studyb), 2)
  
  lib_unload(dat) 
  
  expect_equal("dat.demo_studya" %in% ls(), FALSE)
  expect_equal("dat.demo_studyb" %in% ls(), FALSE)
  
})


test_that("lib_size() works as expected.", {
  
 
  libname(dat, base_path, "csv")
  
  expect_equal(lib_size(dat) > 0, TRUE)
  expect_equal(lib_size(dat), 802)
  
  
})

test_that("lib_info() works as expected.", {
  
  
  libname(dat, base_path, "csv")
  
  info <- lib_info(dat)
  
  expect_equal(nrow(info) > 0, TRUE)
  expect_equal(info[1, "Size"],  "10.7 Kb")
  
  
})



test_that("lib_path() works as expected.", {
  
  
  libname(dat, base_path, "csv")
  
  pth <- lib_path(dat)
  
  expect_equal(pth, base_path)
  
  
})


test_that("lib_sync() function works as expected.", {


  libname(dat, base_path, type = "csv")

  ld <- is.loaded.lib("dat")

  expect_equal(ld, FALSE)

  lib_load(dat)

  ld <- is.loaded.lib("dat")

  expect_equal(ld, TRUE)

  acount <- nrow(dat.demo_studya)
  bcount <- nrow(dat.demo_studyb)

  tmp <- dat.demo_studya
  dat.demo_studya <<- dat.demo_studyb
  dat.demo_studyb <<- tmp

  dat <- lib_sync(dat)

  expect_equal(nrow(dat$demo_studya), bcount)
  expect_equal(nrow(dat$demo_studyb), acount)

  lib_unload(dat)

  ld <- is.loaded.lib("dat")

  expect_equal(ld, FALSE)

})




test_that("lib_sync() function can add a new item from workspace.", {
  
  
  libname(dat, base_path, type = "csv")

  
  lib_load(dat)

  dat.demo_studyc <<- mtcars
  
  lib_sync(dat)
  
  expect_equal(length(dat), 3)
  
})

test_that("lib_unload() function can add a new item from workspace.", {
  
  
  libname(dat, base_path, type = "csv")
  
  
  lib_load(dat)
  
  dat.demo_studyc <<- mtcars
  
  lib_unload(dat)
  
  expect_equal(length(dat), 3)
  
})


test_that("lib_add() function works as expected unloaded.", {
  
  alt_path <- paste0(base_path, "2")
  libname(dat, alt_path)
  
  lib_add(dat, mtcars, iris)
  
  inf <- lib_info(dat)
  
  expect_equal(length(dat), 2)
  expect_equal(nrow(inf), 2)
  
  lib_add(dat, mtcars, iris, name = c("fork", "bork"))
  
  expect_equal(length(dat), 4)
  
  lib_delete(dat)
  
})

test_that("lib_add() function works as expected loaded.", {
  
  alt_path <- paste0(base_path, "2")
  libname(dat, alt_path)
  
  lib_load(dat)
  
  lib_add(dat, mtcars, iris)
  
  inf <- lib_info(dat)
  
  expect_equal(length(dat), 2)
  expect_equal(nrow(inf), 2)
  
  lib_add(dat, mtcars, iris, name = c("fork", "bork"))
  
  expect_equal(length(dat), 4)
  
  lib_unload(dat)  
  
  lib_delete(dat)
  
})

test_that("lib_add(), lib_remove() functions work as expected unloaded.", {
  
  alt_path <- paste0(base_path, "2")
  libname(dat, alt_path)
  
  lib_add(dat, mtcars, iris)
  
  expect_equal(length(dat), 2)
  
  lib_remove(dat, "mtcars")
  
  expect_equal(length(dat), 1)
  
  lib_delete(dat)
  
})


test_that("lib_add(), lib_remove() functions work as expected loaded.", {
  
  alt_path <- paste0(base_path, "2")
  libname(dat, alt_path)
  
  lib_load(dat)
  
  lib_add(dat, mtcars, iris)
  
  expect_equal(length(dat), 2)
  
  lib_remove(dat, "mtcars")
  
  expect_equal(length(dat), 1)
  
  lib_remove(dat, "iris")
  
  expect_equal(length(dat), 0)
  
  lib_unload(dat)
  
  lib_delete(dat)
})
  


test_that("lib_add() function can add a new items of different types.", {
  
  alt_path <- paste0(base_path, "2")
  libname(dat, alt_path)
  
  # RDS
  lib_add(dat, mtcars)
  
  res <- file.exists(file.path(alt_path, "mtcars.rds"))
  
  expect_equal(res, TRUE)
  
  lib_remove(dat, "mtcars")
  
  # CSV
  
  lib_add(dat, mtcars, type = "csv")
  
  res <- file.exists(file.path(alt_path, "mtcars.csv"))
  
  expect_equal(res, TRUE)
  
  lib_remove(dat, "mtcars")
  
  
  
  # sas7bdat
  
  lib_add(dat, mtcars, type = "sas7bdat")
  
  res <- file.exists(file.path(alt_path, "mtcars.sas7bdat"))
  
  expect_equal(res, TRUE)
  
  lib_remove(dat, "mtcars")
  
  

  # XLS
  lib_add(dat, mtcars, type = "xls")
  
  res <- file.exists(file.path(alt_path, "mtcars.xls"))
  
  expect_equal(res, TRUE)
  
  lib_remove(dat, "mtcars")
  
  
  
  # xlsx
  
  lib_add(dat, mtcars, type = "xlsx")
  
  res <- file.exists(file.path(alt_path, "mtcars.xlsx"))
  
  expect_equal(res, TRUE)
  
  lib_remove(dat, "mtcars")
  
  # Clear out
  
  lib_delete(dat)
})


test_that("lib_write() function can add a new item and save as rds.", {
  
  alt_path <- paste0(base_path, "2")
  libname(dat, alt_path)
  
  lib_add(dat, mtcars)
  
  lib_write(dat)
  
  res <- file.exists(file.path(alt_path, "mtcars.rds"))
  
  expect_equal(res, TRUE)
  
  lib_remove(dat, "mtcars")
  
  lib_delete(dat)
  
})


test_that("lib_add() function can add a new item and save as csv", {
  
  alt_path <- paste0(base_path, "2")
  libname(dat, alt_path, type = "csv")
  
  lib_add(dat, mtcars)
  
  res <- file.exists(file.path(alt_path, "mtcars.csv"))
  
  expect_equal(res, TRUE)
  
  lib_delete(dat)
  
})


test_that("lib_add() function can add a new item and save as xslx", {
  
  alt_path <- paste0(base_path, "2")
  libname(dat, alt_path, type = "xlsx")
  
  lib_add(dat, mtcars)
  
  res <- file.exists(file.path(alt_path, "mtcars.xlsx"))
  
  expect_equal(res, TRUE)
  
  lib_delete(dat)
  
})


test_that("lib_add() function can add a new item and save as rds", {
  
  alt_path <- paste0(base_path, "2")
  libname(dat, alt_path, type = "rds")
  
  lib_add(dat, mtcars)
  
  res <- file.exists(file.path(alt_path, "mtcars.rds"))
  
  expect_equal(res, TRUE)
  
  lib_delete(dat)
  
})


test_that("lib_add() function can add a new item and save as sas7bdat", {
  
  alt_path <- paste0(base_path, "2")
  libname(dat, alt_path, type = "sas7bdat")
  
  lib_add(dat, mtcars)
  
  res <- file.exists(file.path(alt_path, "mtcars.sas7bdat"))
  
  expect_equal(res, TRUE)
  
  lib_delete(dat)
  
})


test_that("lib_write() function can add a new item from workspace.", {

  alt_path <- paste0(base_path, "2")
  libname(dat, alt_path, type = "csv")


  lib_load(dat)

  dat.demo_studyc <<- mtcars

  lib_write(dat)

  pth <- file.path(lib_path(dat), "demo_studyc.csv")
  
  res <- file.exists(pth)
  
  expect_equal(res, TRUE)
  
  lib_delete(dat)

})


test_that("lib_copy() works as expected.", {
  
  alt_path <- paste0(base_path, "2")
  
  libname(dat, base_path, "csv")
  
  lib_copy(dat, dat2, alt_path)
  
  expect_equal(file.exists(file.path(alt_path, "demo_studyb.csv")), TRUE)
  
  lib_delete(dat2)
  
  
})

test_that("read-only flag works as expected.", {


  libname(dat, base_path, "csv", read_only = TRUE)
  
  expect_error(lib_add(dat, mtcars))
  expect_error(lib_remove(dat, "demo_study"))
  expect_error(lib_delete(dat))
  
  
})


# test_that("lib_add(), lib_write(), and lib_delete() functions work as expected.", {
#   
#   alt_path <- paste0(base_path, "2")
#   libname(dat, alt_path)
#   
#   
#   lib_add(dat, mtcars)
#   lib_add(dat, iris)
#   
#   inf <- lib_info(dat)
#   
#   expect_equal(length(inf), 2)
#   
#   lib_write(dat)
#   
#   lst <- list.files(alt_path)
#   
#   expect_equal(length(lst), 2)
#   
#   lib_delete(dat)
#   
#   lst <- list.files(alt_path)
#   
#   expect_equal(length(lst), 0)
#   
# })
# 
# test_that("lib_env() works as expected.", {
#   
#   env1 <- new.env()
# 
#   # Assign new environment
#   lib_env(env1)
# 
#   # Create temp directory
#   tmp <- tempdir()
# 
#   # Create library
#   libname(dat, tmp)
# 
#   # Add data to library
#   lib_add(dat, mtcars)
#   lib_add(dat, iris)
# 
#   # Load library
#   lib_load(dat)
# 
#   # Examine global environment
#   ls()
#   # [1] "env1" "tmp"
# 
#   # Examine new environment
#   ls(envir = env1)
# 
#   # Clean up
#   lib_delete(dat)
#   
#   
# })
