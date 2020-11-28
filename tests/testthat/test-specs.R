context("Utilities Tests")

base_path <- "c:\\packages\\libr\\tests\\testthat\\data"

base_path <- "./data"


test_that("import_spec() function works as expected.", {
  
  is <- import_spec(col1 = "character", col2 = "integer",
                   na = c("NA", ""),
                   trim_ws = FALSE, 
                   params = list(forker = TRUE))
  
  expect_equal(is$col_types, list(col1 = "character", col2 = "integer"))
  expect_equal(is$na, c("NA", ""))
  expect_equal(is$trim_ws, FALSE)
  expect_equal(length(is$params), 1)
  
})

test_that("specs() function works as expected.", {
  
  is1 <- import_spec(col1 = "character", col2 = "integer",
                     na = c("NA", "-"),
                     trim_ws = FALSE)
  
  is2 <- import_spec(col1 = "numeric", col2 = "date:%d%m%Y",
                     na = c(" ", ""),
                     trim_ws = TRUE)
  spcs <- specs(na = c("NA", "-", " ", ""),
               trim_ws = TRUE, AB = is1, AC = is2)
  
  expect_equal(length(spcs$specs), 2)
  expect_equal(spcs$na, c("NA", "-", " ", ""))
  expect_equal(spcs$trim_ws, TRUE)
  expect_equal(spcs$specs$AB$trim_ws, FALSE)
  expect_equal(spcs$specs$AB$col_types[[1]], "character")
  expect_equal(spcs$specs$AC$col_types[[1]], "numeric")
})


test_that("write_specs() and read_specs() functions works as expected.", {
  
  is1 <- import_spec(col1 = "character", col2 = "integer",
                     na = c("NA", "-"),
                     trim_ws = FALSE)
  
  is2 <- import_spec(col1 = "numeric", col2 = "date:%d%m%Y",
                     na = c(" ", ""),
                     trim_ws = TRUE)
  spcs <- specs(na = c("NA", "-", " ", ""),
                trim_ws = TRUE, AB = is1, AC = is2)
  
  tmp <- tempdir()
  pth2 <- write.specs(spcs, tmp)
  
  fe <- file.exists(file.path(tmp, "spcs.specs"))
  expect_equal(fe, TRUE)

  
  spcs2 <- read.specs(pth2)
  
  
  expect_equal(length(spcs2$specs), 2)
  expect_equal(spcs2$na, c("NA", "-", " ", ""))
  expect_equal(spcs2$trim_ws, TRUE)
  expect_equal(spcs2$specs$AB$trim_ws, FALSE)
  expect_equal(spcs2$specs$AB$col_types[[1]], "character")
  expect_equal(spcs2$specs$AC$col_types[[1]], "numeric")
  
  if (fe)
    unlink(pth2)
  
})

test_that("get_colspec_xlsx() works as expected.", {
  
  expect_error(get_colspec_xlsx(c(one = "character", 
                       two = "integer", 
                       four = "date:%d%m%y"),
                     5, c("one", "two", "three", "four")))
  
  res <- get_colspec_xlsx(c(one = "character", 
                       two = "integer", 
                       four = "date:%d%m%y"),
                     4, c("one", "two", "three", "four"))

  expect_equal(res[["one"]], "text")
  expect_equal(res[["two"]], "numeric")
  expect_equal(res[["three"]], "guess")
  expect_equal(res[["four"]], "date")
  
})

test_that("get_colspec_csv() works as expected.", {
  
  res <- get_colspec_csv(c(one = "character", 
                       two = "integer", 
                       four = "date=%d%m%y",
                       five = "numeric",
                       six = "guess",
                       seven = "time=%H:%M:%S",
                       eight = "datetime=%d%m%y %H:%M:%S",
                       nine = "logical"))
  
  expect_equal(res$cols[["one"]], col_character())
  expect_equal(res$cols[["two"]], col_integer())
  expect_equal(is.null(res$cols[["three"]]), TRUE)
  expect_equal(res$cols[["four"]], col_date(format = "%d%m%y"))
  expect_equal(res$cols[["five"]], col_double())
  expect_equal(res$cols[["six"]], col_guess())
  expect_equal(res$cols[["seven"]], col_time(format = "%H:%M:%S"))
  expect_equal(res$cols[["eight"]], col_datetime(format = "%d%m%y %H:%M:%S"))
  expect_equal(res$cols[["nine"]], col_logical())
  
})


test_that("libname works on PE data for csv.", {
  
  lst <- specs(PE = import_spec(PESTAT = "character",
                                na = "NA"))
  
  libname(dat, file.path(base_path, "PE"), "csv", import_specs = lst)
  

  expect_equal(length(dat), 1)
  

})

test_that("libname works on PE data for xlsx.", {
  
  lst <- specs(PE = import_spec(PESTAT = "character",
                                na = "NA"))
  
  
  libname(dat, file.path(base_path, "PE"), "xlsx", import_specs = lst)
  

  expect_equal(length(dat), 1)
  
  
})

test_that("libname works on SDTM data for csv.", {
  
  lst <- specs(PE = import_spec(PESTAT = "character",
                                na = "NA"))
  
  
  libname(dat, file.path(base_path, "SDTM"), "csv", import_specs = lst)
  

  expect_equal(length(dat), 13)
  
})

test_that("libname works on SDTM data for sas7bdat.", {
  
  

  libname(dat, file.path(base_path, "SDTM"), "sas7bdat")
  

  expect_equal(length(dat), 13)

})
