context("Datastep Tests")

base_path <- "c:\\packages\\libr\\tests\\testthat\\data"

base_path <- "./data"


test_that("datastep() function works as expected with mtcars.", {


  d1 <- datastep(mtcars, {

    if (mpg >= 20)
      mpgcat <- "High"
    else
      mpgcat <- "Low"

  })

  d1
  
  expect_equal("mpgcat" %in% names(d1), TRUE)

})



test_that("datastep() function works as expected with demo_studya.", {


  libname(dat, base_path, "csv")

  lib_load(dat)

  d1 <- datastep(dat.demo_studya, {

    if (sex == "M")
      sexc <- "Male"
    else if(sex == "F")
      sexc <- "Female"
    else
      sexc <- "Other"

  })
  
  d1

  expect_equal("sexc" %in% names(d1), TRUE)

})


test_that("datastep() keep parameter works as expected.", {

  libname(dat, base_path, "csv")

  lib_load(dat)

  d1 <- datastep(dat.demo_studya, keep = c("usubjid", "sexc", "race"), {

    usubjid <- paste0(study, "-", inv, "-", patient)
    if (sex == "M")
      sexc <- "Male"
    else if(sex == "F")
      sexc <- "Female"
    else
      sexc <- "Other"

  })
  
  d1

  expect_equal(names(d1), c("usubjid", "sexc", "race"))

})

test_that("datastep() drop parameter works as expected.", {

  libname(dat, base_path, "rds")

  lib_load(dat)

  d1 <- datastep(dat.demo_studya, drop = c("visit", "screendate", "sex"), {

    usubjid <- paste0(study, "-", inv, "-", patient)
    if (sex == "M")
      sexc <- "Male"
    else if(sex == "F")
      sexc <- "Female"
    else
      sexc <- "Other"

  })
  
  d1

  expect_equal(names(d1), c("study", "inv", "patient", "race",
                            "dob", "treatment", "sexc", "usubjid"))

})


test_that("datastep() by parameter first and last works as expected.", {

  libname(dat, base_path, "rds")

  lib_load(dat)


  d1 <- datastep(dat.demo_studya, by = c("treatment"), {

    f1 <- first.
    l1 <- last.

  })
  
  d1

  expect_equal(sum(d1$f1), 2)
  expect_equal(sum(d1$l1), 2)

})


test_that("datastep() summary functions works as expected.", {

  libname(dat, base_path, "rds")

  lib_load(dat)


  d1 <- datastep(dat.demo_studya, {

    pmean <- mean(data$patient)
    if (patient > pmean)
      pind <- TRUE
    else
      pind <- FALSE

    rownum <- n.
  })

  d1

  expect_equal(sum(d1$pind), 5)

})

test_that("datastep() calculate parameter works as expected.", {

  libname(dat, base_path, "rds")

  lib_load(dat)


  d1 <- datastep(dat.demo_studya,
    calculate = {
      pmean <- mean(patient)
    },{

    if (patient > pmean)
      pind <- TRUE
    else
      pind <- FALSE

    rownum <- n.
  })

  d1

  expect_equal(sum(d1$pind), 5)

})

test_that("datastep() auto-group-by works as expected.", {

  if (TRUE) {
    libname(dat, base_path, "rds")

    lib_load(dat)


    d1 <- dat.demo_studya %>% group_by(treatment) %>%
      datastep({
        p1 <- first.
        p2 <- last.
        rownum <- n.
      })

    d1

    expect_equal(sum(d1$p1), 2)
    expect_equal(sum(d1$p2), 2)
    expect_equal(sum(d1$rownum), 55)

    lib_unload(dat)

  } else
    expect_equal(TRUE, TRUE)
})

test_that("datastep() by parameter sort check works as expected.", {

  libname(dat, base_path, "rds")

  lib_load(dat)

  d1 <- datastep(dat.demo_studya, by = c("treatment"), {

    f1 <- first.
    l1 <- last.

  })

  expect_equal(sum(d1$f1), 2)
  expect_equal(sum(d1$l1), 2)

  d2 <- dat.demo_studya[order(dat.demo_studya$dob), ]

  expect_error(datastep(d2, by = c("treatment"), {

    f1 <- first.
    l1 <- last.

  }))

  d3 <- datastep(d2, by = c("treatment"), sort_check = FALSE, {

    f1 <- first.
    l1 <- last.

  })

  expect_equal(sum(d3$f1), 5)
  expect_equal(sum(d3$l1), 5)

})


test_that("datastep() retain parameter works as expected.", {

  libname(dat, base_path, "rds")

  lib_load(dat)

  d1 <- datastep(dat.demo_studya, retain = list("fork" = 0, bork = ""), {

    fork <- fork + 1

    if (first.)
      bork <- "begin"
    else if (last.)
      bork <- "end"
    else
      bork <- paste("middle", n.)

  })

  d1

  expect_equal(d1$fork[10], 10)
  expect_equal(d1$bork[1], "begin")
  expect_equal(d1$bork[2], "middle 2")
  expect_equal(d1$bork[10], "end")
})

test_that("datastep() retain class check works as expected", {


  expect_error(datastep(mtcars, retain = c(fork = 0), {fork <- fork + 1}))

})

test_that("datastep() array class check works as expected", {
  
  
  expect_error(datastep(mtcars, array = c(fork = 0), {fork <- fork + 1}))
  
})


test_that("datastep() attrib class check works as expected", {
  
  
  expect_error(datastep(mtcars, attrib = c(fork = 0), {fork <- fork + 1}))
  
})



test_that("Rename works as expected", {

  df <- datastep(mtcars[1:10, ],
    drop = c("disp", "hp", "drat", "qsec",
    "vs", "am", "gear", "carb"),
    retain = list(cumwt = 0 ),
    rename = c(mpg = "MPG", cyl = "Cylinders", wt = "Wgt",
    cumwt = "Cumulative Wgt"),
  {
    cumwt <- cumwt + wt
  })

  df

  expect_equal("MPG" %in% names(df), TRUE)
  expect_equal("Cylinders" %in% names(df), TRUE)
  expect_equal("Wgt" %in% names(df), TRUE)
  expect_equal("Cumulative Wgt" %in% names(df), TRUE)

})




test_that("datastep() attributes on data are maintained.", {
  
  library(dplyr)

  
  libname(dat, file.path(base_path, "SDTM"), "sas7bdat")
  
  attributes(dat$dm$USUBJID)
  
  prep <- dat$dm %>% 
    left_join(dat$vs, by = c("USUBJID" = "USUBJID")) %>% 
    select(USUBJID, VSTESTCD, VISIT, VISITNUM, VSSTRESN, ARM, VSBLFL) %>% 
    filter(VSTESTCD %in% c("PULSE", "RESP", "TEMP", "DIABP", "SYSBP"), 
           !(VISIT == "SCREENING" & VSBLFL != "Y")) %>% 
    arrange(USUBJID, VSTESTCD, VISITNUM) %>% 
    group_by(USUBJID, VSTESTCD) %>%

    datastep(retain = list(BSTRESN = 0), {
      
      # Combine treatment groups
      # And distingish baseline time points
      if (ARM == "ARM A") {
        
        if (VSBLFL %eq% "Y") {
          GRP <- "A_BASE"
        } else {
          GRP <- "A_TRT"
        }
        
      } else {
        
        if (VSBLFL %eq% "Y") {
          GRP <- "O_BASE"
        } else {
          GRP <- "O_TRT"
        }
        
      }
      
      # Populate baseline value
      if (first.)
        BSTRESN = VSSTRESN
      
    })
  
  
  expect_equal(attr(prep$USUBJID, "label"), "Unique Subject Identifier")
    
  
})

test_that("datastep retains class attributes.", {
  
  
  s1 <-  1:3
  dt1 <- c(Sys.Date(), Sys.Date() - 1, Sys.Date() - 2)
  df1 <- data.frame(s1, dt1)
  
  df2 <- datastep(df1, rename = c(dt1 = "dt2"), {
    csum <- 1
  })
  
  df2
  
  expect_equal(class(df2$s1), "integer")
  expect_equal(class(df2$dt2), "Date")
  expect_equal(class(df2$csum), "numeric")

})

test_that("datastep works on single column data frame.", {
  
  df <- data.frame(a = 1:10)
  
  
  df2 <- datastep(df, {
    
    if (a > 5)
      status <- "High"
    else 
      status <- "Low"
    
  })
  
  df2
  
  expect_equal(ncol(df2), 2)
  expect_equal(nrow(df2), 10)
  expect_equal(class(df2), "data.frame")
  
  
})


test_that("datastep works on single column tibble.", {
  
  df <- tibble(a = 1:10)
  
  
  df2 <- datastep(df, {
    
    if (a > 5)
      status <- "High"
    else 
      status <- "Low"
    
  })
  
  df2
  
  expect_equal(ncol(df2), 2)
  expect_equal(nrow(df2), 10)
  expect_equal(class(df2), c("tbl_df", "tbl", "data.frame"))
  
  
})


test_that("datastep() attributes on data are maintained on base dataframe.", {
  
  
  dat <- mtcars
  
  attr(dat$mpg, "label") <- "Miles Per Gallon"
  
  
  dat2 <- datastep(dat, {
    fork <- "Hello" 
  })

  dat2
  
  expect_equal(attr(dat2$mpg, "label"), "Miles Per Gallon")  
  
})
  
  

test_that("datastep works on tibble.", {
  

    
    library(tibble)
    
    l <- 1000
    
    df <- tibble(C1 = seq_len(l), C2 = runif(l), 
                 C3 = runif(l), C4 = runif(l))
    
    
    res <- datastep(df, attrib = list(C5 = 0, C6 = 0),
                    {
                      C5 <- C2 + C3 + C4
                      C6 <- max(C2, C3, C4)
                      
                    })
    
    res
    
    expect_equal("C5" %in% names(res), TRUE)
    expect_equal("C6" %in% names(res), TRUE)
    expect_equal(nrow(res), 1000)
    
  
})



test_that("datastep works on data.table", {
  
  
  
  library(data.table)
  
  l <- 1000
  
  df <- data.table(C1 = seq_len(l), C2 = runif(l), 
               C3 = runif(l), C4 = runif(l))
  
  
  res <- datastep(df, attrib = list(C5 = 0, C6 = 0),
                  {
                    C5 <- C2 + C3 + C4
                    C6 <- max(C2, C3, C4)
                    
                  })
  
  res
  
  expect_equal("C5" %in% names(res), TRUE)
  expect_equal("C6" %in% names(res), TRUE)
  expect_equal(nrow(res), 1000)
  
  
})



test_that("datastep() works on a dataframe with a factor.", {
  
  
  dat <- iris
  
  
  dat2 <- datastep(dat, {
    fork <- Petal.Length + Petal.Width 
  })
  
  dat2
  
  expect_equal("fork" %in% names(dat2), TRUE)  
  expect_equal(class(dat2$Species), "factor")
  
})
  
