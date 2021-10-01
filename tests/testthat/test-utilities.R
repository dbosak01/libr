context("Utilities Tests")

base_path <- "c:\\packages\\libr\\tests\\testthat\\data"

base_path <- "./data"

DEV <- FALSE

test_that("print() functions works as expected.", {
  
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


test_that("eq function work as expected.", {
  
  expect_equal(mtcars %eq% mtcars, TRUE)
  expect_equal(mtcars %eq% iris, FALSE)
  expect_equal(mtcars %eq% mtcars[1:10, ], FALSE)
  expect_equal(mtcars %eq% mtcars[, 1:5], FALSE)  
  d1 <- mtcars
  d1[1, 1] <- 2
  expect_equal(mtcars  %eq%  d1, FALSE)
  
  v1 <- mtcars[[1]]
  v2 <- mtcars[[1]]
  
  expect_equal(v1 %eq% v2, TRUE)
  expect_equal(v1 %eq% mtcars[[2]], FALSE)
  expect_equal(v1 %eq% v2[1:5], FALSE)
  v2[5] <-2
  expect_equal(v1  %eq% v2, FALSE)
  
  v1 <- c(1, 2, NA, NA)
  v2 <- c(NA, NA, 1, NA, 3)
  expect_equal(v1 %eq% v2, FALSE)
  
})

test_that("strong_eq function works as expected.", {
  
  v1 <- c(1, 2, 3, 4)
  v2 <- c(1, 2, 3, 4)
  expect_equal(all(strong_eq(v1, v2)), TRUE)
  
  v1 <- c(1, 2, 2, 4)
  v2 <- c(1, 2, 3, 4)
  expect_equal(all(strong_eq(v1, v2)), FALSE)
  
  v1 <- c(1, NA, 3, 4)
  v2 <- c(1, 2, 3, 4)
  expect_equal(all(strong_eq(v1, v2)), FALSE)
  
  v1 <- c(NA, NA, NA, NA)
  v2 <- c(NA, NA, NA, NA)
  expect_equal(all(strong_eq(v1, v2)), TRUE)
  
  v1 <- c(NA, NA, NA, NA)
  v2 <- c(NA, NA, 1, NA)
  expect_equal(all(strong_eq(v1, v2)), FALSE)
  
  
})


test_that("dofilter function works as expected for paths", {
  
  
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


test_that("dofilter function works as expected for names", {
  
  
  v2 <- c("fork", "four", "spork")
  
  expect_equal(dofilter("fo*", v2),  c("fork", "four"))
  
  expect_equal(dofilter("fork", v2),  c("fork"))
  
  expect_equal(is.null(dofilter("or*", v2)),  TRUE)
  
  expect_equal(dofilter("*or*", v2),  c("fork", "spork"))
  
  expect_equal(dofilter(c("fo*", "sp*"), v2),  c("fork", "four", "spork"))
  
  expect_equal(dofilter(c("fork", "spork"), v2),  c("fork", "spork"))
  
  expect_equal(dofilter(c("FORK", "Sp*"), v2),  c("fork", "spork"))
  
})

test_that("copy_attributes function works as expected.", {
  
  d1 <- mtcars
  d2 <- mtcars
  
  attr(d1$mpg, "label") <- "Here1"
  attr(d1$disp, "label") <- "Here2"

  d3 <- copy_attributes(d1, d2)

  expect_equal(attr(d3$mpg, "label"), "Here1")
  expect_equal(attr(d3$disp, "label"), "Here2")
  
})


test_that("equality operators are working as expected.", {
  
  
  expect_equal(NULL %eq% NULL, TRUE)
  expect_equal(NULL %eq% "fork", FALSE)
  expect_equal(1 %eq% "fork", FALSE)
  
  
  expect_equal(data.frame(A = 1) %eq% data.frame(B = 1), FALSE)
  
  expect_equal(strong_eq(1 , "fork"), FALSE)
  
  
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

