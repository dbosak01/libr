# context("Datastep Attributes Tests")
# 
# base_path <- "c:\\packages\\libr\\tests\\testthat\\data"
# 
# base_path <- "./data"
# 
# DEV <- FALSE
# 
# 
# test_that("dsattr() function works", {
#   
#   
#   dsa <- dsattr(class = "character", label = "My label", description = "Desc",
#                 format = "%s", width = 8, justify = "left", fork = "hi",
#                 bork = "bye")
#   
#   expect_equal(dsa$class, "character")
#   expect_equal(dsa$label, "My label")
#   expect_equal(dsa$description, "Desc")
#   expect_equal(dsa$format, "%s")
#   expect_equal(dsa$width, 8)
#   expect_equal(dsa$justify, "left")
#   expect_equal(dsa$fork, "hi")
#   expect_equal(dsa$bork, "bye")
#   
# })
# 
# 
# 
# 
# test_that("dsattr function works with datastep and new variable.", {
#   
#   dsa <- dsattr(class = "character", label = "My label", description = "Desc",
#                 format = "%s", width = 8, justify = "left", fork = "hi",
#                 bork = "bye")
#   
#   
#   dat <- mtcars[1:10, ]
#   
#   attr(dat$mpg, "label") <- "Miles Per Gallon"
#   
#   expect_equal(attr(dat$mpg, "label"), "Miles Per Gallon")
#   
#   
#   d1 <- datastep(dat, attrib = list(fork = dsa), {
#     
#     fork <- "Hello"
#     
#   })
#   
#   attributes(d1$mpg)
#   attributes(d1$fork)
# 
#   
#   expect_equal("fork" %in% names(d1), TRUE)
#   expect_equal(class(d1[["fork"]]), "character")
#   expect_equal(attr(d1[["fork"]], "label"), "My label")
#   expect_equal(attr(d1[["fork"]], "description"), "Desc")
#   expect_equal(attr(d1[["fork"]], "format"), "%s")
#   expect_equal(attr(d1[["fork"]], "width"), 8)
#   expect_equal(attr(d1[["fork"]], "justify"), "left")
#   expect_equal(attr(d1[["fork"]], "fork"), "hi")
#   expect_equal(attr(d1[["fork"]], "bork"), "bye")
#   expect_equal(attr(d1[["mpg"]], "label"), "Miles Per Gallon")
# })
# 
# 
# 
# 
# test_that("dsattr function works with datastep and two new variables.", {
#   
#   dsa1 <- dsattr(class = "character", label = "My label1")
#   dsa2 <- dsattr(class = "character", label = "My label2")
#   
#   
#   dat <- mtcars[1:10, ]
#   
#   attr(dat$mpg, "label") <- "Miles Per Gallon"
#   
#   expect_equal(attr(dat$mpg, "label"), "Miles Per Gallon")
#   
#   
#   d1 <- datastep(dat, attrib = list(fork = dsa1, bork = dsa2), {
#     
#     fork <- "Hello"
#     bork <- "Goodbye"
#     
#   })
#   
#   attributes(d1$fork)
#   attributes(d1$bork)
#   
#   d1
#   
#   
#   expect_equal("fork" %in% names(d1), TRUE)
#   expect_equal("bork" %in% names(d1), TRUE)
#   expect_equal(attr(d1[["fork"]], "label"), "My label1")
#   expect_equal(attr(d1[["bork"]], "label"), "My label2")
#   expect_equal(attr(d1[["mpg"]], "label"), "Miles Per Gallon")
# })
# 
# 
# test_that("dsattr function works with datastep and a tibble.", {
#   
#   library(tibble)
#   
#   dsa1 <- dsattr(class = "character", label = "My label1")
#   dsa2 <- dsattr(class = "character", label = "My label2")
#   
#   
#   dat <- as_tibble(mtcars[1:10, ])
#   
#   attr(dat$mpg, "label") <- "Miles Per Gallon"
#   
#   expect_equal(attr(dat$mpg, "label"), "Miles Per Gallon")
#   
#   
#   d1 <- datastep(dat, attrib = list(fork = dsa1, bork = dsa2), {
#     
#     fork <- "Hello"
#     bork <- "Goodbye"
#     
#   })
#   
#   attributes(d1$fork)
#   attributes(d1$bork)
#   
#   d1
#   
#   
#   expect_equal("fork" %in% names(d1), TRUE)
#   expect_equal("bork" %in% names(d1), TRUE)
#   expect_equal(attr(d1[["fork"]], "label"), "My label1")
#   expect_equal(attr(d1[["bork"]], "label"), "My label2")
#   expect_equal(attr(d1[["mpg"]], "label"), "Miles Per Gallon")
# })
# 
# 
# test_that("attrib parameter works with datastep and no dsattr.", {
#   
# 
#   
#   dat <- mtcars[1:10, ]
#   
#   
#   d1 <- datastep(dat, attrib = list(fork = "", bork = 0), {
#     
#     
#   })
#   
#   attributes(d1$fork)
#   attributes(d1$bork)
#   
#   d1
#   
#   
#   expect_equal("fork" %in% names(d1), TRUE)
#   expect_equal("bork" %in% names(d1), TRUE)
#   expect_equal(d1[["fork"]][1], "")
#   expect_equal(d1[["bork"]][1], 0)
#   
#   
#   
#   d2 <- datastep(dat, attrib = list(fork = "hello", bork = 23), {
#     
#     
#   })
# 
#   
#   d2
#   
#   
#   expect_equal("fork" %in% names(d2), TRUE)
#   expect_equal("bork" %in% names(d2), TRUE)
#   expect_equal(d2[["fork"]][1], "hello")
#   expect_equal(d2[["bork"]][1], 23)
#   
# })
# # 
# 
# 
