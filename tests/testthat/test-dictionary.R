context("Dictionary Tests")

base_path <- "c:\\packages\\libr\\tests\\testthat\\data"

base_path <- "./data"


test_that("getDictionary() function works as expected.", {
  
  crs <- data.frame(name = rownames(mtcars), mtcars)
  
  attr(crs$name, "label") <- "Car Name"
  attr(crs$name, "description") <- "Derived from rownames"
  crs$type <- ifelse(crs$mpg > 20, "High", "Low")
  attr(crs$mpg, "format") <- "%.1f"
  
  res <- getDictionary(crs, "crs")

  expect_equal(nrow(res), 13)
  expect_equal(as.character(res[1, "Label"]), "Car Name")
  expect_equal(as.character(res[1, "Description"]), "Derived from rownames")
  expect_equal(res[13, "Width"], 4)
  
})



test_that("dictionary() function works as expected with df.", {
  
  crs <- data.frame(name = rownames(mtcars), mtcars)
  
  attr(crs$name, "label") <- "Car Name"
  attr(crs$name, "description") <- "Derived from rownames"
  crs$type <- ifelse(crs$mpg > 20, "High", "Low")
  attr(crs$mpg, "format") <- "%.1f"
  
  res <- dictionary(crs)
  
  expect_equal(nrow(res), 13)
  
})



test_that("dictionary() function works as expected with tibble.", {
  
  library(tibble)
  
  crs <- tibble(name = rownames(mtcars), mtcars)
  
  attr(crs$name, "label") <- "Car Name"
  attr(crs$name, "description") <- "Derived from rownames"
  crs$type <- ifelse(crs$mpg > 20, "High", "Low")
  attr(crs$mpg, "format") <- "%.1f"
  attr(crs$hp, "justify") <- "left"
  
  res <- dictionary(crs)
  
  expect_equal(nrow(res), 13)
  
})


test_that("dictionary() function works as expected with lib.", {
  
  crs <- data.frame(name = rownames(mtcars), mtcars)
  
  attr(crs$name, "label") <- "Car Name"
  attr(crs$name, "description") <- "Derived from rownames"
  crs$type <- ifelse(crs$mpg > 20, "High", "Low")
  attr(crs$mpg, "format") <- "%.1f"
  attr(crs$name, "justify") <- "left"
  
  dat <- libname(dat, tempdir())
  
  dat <- lib_add(dat, crs)
  dat <- lib_add(dat, iris)
  dat <- lib_add(dat, beaver1)
  
  res <- dictionary(dat)
  
  expect_equal(nrow(res), 22)
  
  
  lib_delete(dat)
})



test_that("dictionary() function works as expected with df and standard_eval.", {
  
  
  options("libr.standard_eval" = TRUE)
  
  crs <- data.frame(name = rownames(mtcars), mtcars)
  
  attr(crs$name, "label") <- "Car Name"
  attr(crs$name, "description") <- "Derived from rownames"
  crs$type <- ifelse(crs$mpg > 20, "High", "Low")
  attr(crs$mpg, "format") <- "%.1f"
  
  res <- dictionary(crs)
  
  expect_equal(nrow(res), 13)
  
  options("libr.standard_eval" = FALSE)
  
})


test_that("dictionary() function works as expected with lib.", {
  
  options("libr.standard_eval" = TRUE)
  
  crs <- data.frame(name = rownames(mtcars), mtcars)
  
  attr(crs$name, "label") <- "Car Name"
  attr(crs$name, "description") <- "Derived from rownames"
  crs$type <- ifelse(crs$mpg > 20, "High", "Low")
  attr(crs$mpg, "format") <- "%.1f"
  attr(crs$name, "justify") <- "left"
  
  dat <- libname(dat, tempdir())
  
  dat <- lib_add(dat, crs)
  dat <- lib_add(dat, iris)
  dat <- lib_add(dat, beaver1)
  
  res <- dictionary(dat)
  
  expect_equal(nrow(res), 22)
  
  
  lib_delete(dat)
  
  options("libr.standard_eval" = FALSE)
})
