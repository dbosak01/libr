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
  expect_equal(info[1, "size"],  626)
  
  
})


# 
# test_that("lib_sync() function works as expected.", {
#   
#   
#   lb <- libname(base_path, filter = "csv")
#   
#   attr(lb, "loaded")
#   
#   lb <- lib_load(lb)
#   
#   acount <- nrow(lb.demo_studya)
#   bcount <- nrow(lb.demo_studyb)
#   
#   tmp <- lb.demo_studya
#   lb.demo_studya <- lb.demo_studyb
#   lb.demo_studyb <- tmp
#   
#   lb <- lib_sync(lb)
#   
#   expect_equal(lb$demo_studya, bcount)
#   expect_equal(lb$demo_studyb, acount)
#   
#   lib_unload(lb) 
#   
# })
