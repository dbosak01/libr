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
  expect_equal(info[1, "Bytes"],  626)
  
  
})



test_that("lib_path() works as expected.", {
  
  
  libname(dat, base_path, "csv")
  
  pth <- lib_path(dat)
  
  expect_equal(pth, base_path)
  
  
})

# 
# test_that("lib_sync() function works as expected.", {
# 
# 
#   libname(dat, base_path, filter = "csv")
# 
#   ld <- e$libs[["dat"]]$loaded
# 
#   expect_equal(ld, FALSE)
# 
#   lib_load(dat)
#   print(dat)
# 
#   ld <- e$libs[["dat"]]$loaded
# 
#   expect_equal(ld, TRUE)
# 
#   acount <- nrow(dat.demo_studya)
#   bcount <- nrow(dat.demo_studyb)
# 
#   tmp <- dat.demo_studya
#   dat.demo_studya <- dat.demo_studyb
#   dat.demo_studyb <- tmp
# 
#   dat <- lib_sync(dat)
#   print(dat)
# 
#   expect_equal(nrow(dat$demo_studya), bcount)
#   expect_equal(nrow(dat$demo_studyb), acount)
# 
#   lib_unload(dat)
# 
#   ld <- e$libs[["dat"]]$loaded
# 
#   expect_equal(ld, FALSE)
# 
# })
