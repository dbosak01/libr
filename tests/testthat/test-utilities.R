context("Utilities Tests")

base_path <- "c:\\packages\\libr\\tests\\testthat\\data"

base_path <- "./data"

DEV <- FALSE

test_that("utils01: print() functions works as expected.", {
  
  if (DEV) {

  libname(dat, base_path, engine = "csv")

  
  dat
  
  expect_equal(is.lib(dat), TRUE)
  
  res <- capture.output(print(dat))
  
  expect_equal(length(res) > 0, TRUE)
  
  
  res2 <- capture.output(print(dat, verbose = TRUE))
  
  expect_equal(length(res2) > 0, TRUE)
  
  } else 
    expect_equal(TRUE, TRUE)
  
})


test_that("utils02: dofilter function works as expected for paths", {
  
  
  v1 <- c("/temp/fork.csv", "/temp/four.csv", "/temp/spork.csv")
  
  expect_equal(dofilter("fo*", v1, "csv"), c("/temp/fork.csv", "/temp/four.csv"))
  
  expect_equal(dofilter("fork", v1, "csv"), c("/temp/fork.csv"))
  
  expect_equal(is.null(dofilter("or*", v1, "csv")), TRUE)
  
  expect_equal(dofilter("*or*", v1, "csv"), c("/temp/fork.csv","/temp/spork.csv"))
  
  expect_equal(dofilter(c("fo*", "sp*"), v1, "csv"), c("/temp/fork.csv", 
                                                       "/temp/four.csv", 
                                                       "/temp/spork.csv"))
  
  expect_equal(dofilter(c("Fo*", "SP*"), v1, "csv"), c("/temp/fork.csv",
                                                       "/temp/four.csv",
                                                       "/temp/spork.csv"))
  
})


test_that("utils03: dofilter function works as expected for names", {
  
  
  v2 <- c("fork", "four", "spork")
  
  expect_equal(dofilter("fo*", v2),  c("fork", "four"))
  
  expect_equal(dofilter("fork", v2),  c("fork"))
  
  expect_equal(is.null(dofilter("or*", v2)),  TRUE)
  
  expect_equal(dofilter("*or*", v2),  c("fork", "spork"))
  
  expect_equal(dofilter(c("fo*", "sp*"), v2),  c("fork", "four", "spork"))
  
  expect_equal(dofilter(c("fork", "spork"), v2),  c("fork", "spork"))
  
  expect_equal(dofilter(c("FORK", "Sp*"), v2),  c("fork", "spork"))
  
})

test_that("utils04: copy_attributes function works as expected.", {
  
  d1 <- mtcars
  d2 <- mtcars
  
  attr(d1$mpg, "label") <- "Here1"
  attr(d1$disp, "label") <- "Here2"

  d3 <- copy_attributes(d1, d2)

  expect_equal(attr(d3$mpg, "label"), "Here1")
  expect_equal(attr(d3$disp, "label"), "Here2")
  
})


test_that("utils05: getExtension() works with two dots", {
  
  flnm <- "table_hemo.1.csv"
  
  res <- getExtension(flnm)
  
  expect_equal(length(res), 1)
  expect_equal(res, "csv")
  
  
})

test_that("utils06: captureSignatures() works as expected.", {
  
  
  m1 <- mtcars
  m2 <- mtcars
  m3 <- mtcars
  
  m2[1, "carb"] <- 3
  attr(m3, "fork") <- 1
  
  s1 <- captureSignatures(m1)
  s2 <- captureSignatures(m2)
  s3 <- captureSignatures(m3)
  
  expect_equal(s1$Length == s2$Length, TRUE)
  expect_equal(s1$Hex == s2$Hex, FALSE)
  expect_equal(s1$Length == s3$Length, TRUE)
  expect_equal(s1$Hex == s3$Hex, TRUE)
  
})

# 
# test_that("libname() var_name parameter works as expected.", {
#   
#   
#   
#   expect_equal(standard_eval(), FALSE)
#   
#   options("libr.standard_eval" = TRUE)
#   
#   expect_equal(standard_eval(), TRUE)
#   
#   options("libr.standard_eval" = FALSE)
#   
#   expect_equal(standard_eval(), FALSE)
#   
#   
#   
#   
# })

