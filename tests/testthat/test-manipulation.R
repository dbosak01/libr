context("libname Tests")

base_path <- "c:\\packages\\libr\\tests\\testthat\\data"

base_path <- "./data"


test_that("lib_load() and lib_unload() functions works as expected.", {
  
  
  expect_error(libname(dat, file.path(base_path, "forker")))
  
  
  libname(dat, base_path, filter = "csv")

  
  # Should not get an error here
  lib_unload(dat)

  lib_load(dat)
  
  expect_equal(nrow(dat.demo_studya), 10)
  expect_equal(nrow(dat.demo_studyb), 2)
  
  lib_unload(dat) 
  
  expect_equal("dat.demo_studya" %in% ls(), FALSE)
  expect_equal("dat.demo_studyb" %in% ls(), FALSE)
  
})

# test_that("lib_create()  works as expected.", {
#   
#   fp <- file.path(base_path, "data4/data5")
#   
#   expect_error(suppressWarnings(lib_create(fp)))
#   
#   
#   fp <- file.path(base_path, "data3")
#   
#   l <- lib_create(fp)
#   
#   expect_equal(dir.exists(fp), TRUE)
#   
#   if (dir.exists(fp))
#     unlink(fp, force = TRUE, recursive = TRUE)
# 
# 
# })

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


  libname(dat, base_path, filter = "csv")

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
  
  
  libname(dat, base_path, filter = "csv")

  
  lib_load(dat)

  dat.demo_studyc <<- mtcars
  
  lib_sync(dat)
  
  expect_equal(length(dat), 3)
  
})

test_that("lib_unload() function can add a new item from workspace.", {
  
  
  libname(dat, base_path, filter = "csv")
  
  
  lib_load(dat)
  
  dat.demo_studyc <<- mtcars
  
  lib_unload(dat)
  
  expect_equal(length(dat), 3)
  
})


test_that("lib_append() function works as expected unloaded.", {
  
  alt_path <- paste0(base_path, "2")
  libname(dat, alt_path)
  
  lib_append(dat, mtcars, iris)
  
  inf <- lib_info(dat)
  
  expect_equal(length(dat), 2)
  expect_equal(nrow(inf), 2)
  
  lib_append(dat, mtcars, iris, .name = c("fork", "bork"))
  
  expect_equal(length(dat), 4)
  
})

test_that("lib_append() function works as expected loaded.", {
  
  alt_path <- paste0(base_path, "2")
  libname(dat, alt_path)
  
  lib_load(dat)
  
  lib_append(dat, mtcars, iris)
  
  inf <- lib_info(dat)
  
  expect_equal(length(dat), 2)
  expect_equal(nrow(inf), 2)
  
  lib_append(dat, mtcars, iris, .name = c("fork", "bork"))
  
  expect_equal(length(dat), 4)
  
  lib_unload(dat)  
  
})

test_that("lib_append(), lib_remove() functions work as expected unloaded.", {
  
  alt_path <- paste0(base_path, "2")
  libname(dat, alt_path)
  
  lib_append(dat, mtcars, iris)
  
  expect_equal(length(dat), 2)
  
  lib_remove(dat, "mtcars")
  
  expect_equal(length(dat), 1)
  
  
  
})


test_that("lib_append(), lib_remove() functions work as expected loaded.", {
  
  alt_path <- paste0(base_path, "2")
  libname(dat, alt_path)
  
  lib_load(dat)
  
  lib_append(dat, mtcars, iris)
  
  expect_equal(length(dat), 2)
  
  lib_remove(dat, "mtcars")
  
  expect_equal(length(dat), 1)
  
  lib_remove(dat, "iris")
  
  expect_equal(length(dat), 0)
  
  lib_unload(dat)
  
})
  
# 
# test_that("lib_write() function can add a new item from workspace.", {
#   
#   
#   libname(dat, base_path, filter = "csv")
#   
#   
#   lib_load(dat)
#   
#   dat.demo_studyc <<- mtcars
#   
#   lib_write(dat, type = "csv")
#   
#   libname(dat2, base_path, filter = "csv")
#   
#   expect_equal(length(dat), 3)
#   
# })

# test_that("lib_append(), lib_write(), and lib_delete() functions work as expected.", {
#   
#   alt_path <- paste0(base_path, "2")
#   libname(dat, alt_path)
#   
#   
#   lib_append(dat, mtcars)
#   lib_append(dat, iris)
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

