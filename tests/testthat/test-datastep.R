context("Datastep Tests")

base_path <- "c:\\packages\\libr\\tests\\testthat\\data"

base_path <- "./data"


options("logr.output" = FALSE)

test_that("ds1: datastep() function works as expected with mtcars.", {


  d1 <- datastep(mtcars, {

    if (mpg >= 20)
      mpgcat <- "High"
    else
      mpgcat <- "Low"

  })

  d1
  
  expect_equal("mpgcat" %in% names(d1), TRUE)

})



test_that("ds2: datastep() function works as expected with demo_studya.", {


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


test_that("ds3: datastep() keep parameter works as expected.", {

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

test_that("ds4: datastep() drop parameter works as expected.", {

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
                            "dob", "treatment", "usubjid", "sexc"))

})


test_that("ds5: datastep() by parameter first and last works as expected.", {

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


test_that("ds6: datastep() summary functions works as expected.", {

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

test_that("ds7: datastep() calculate parameter works as expected.", {

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

test_that("ds8: datastep() auto-group-by works as expected.", {

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

test_that("ds9: datastep() by parameter sort check works as expected.", {

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


test_that("ds10: datastep() retain parameter works as expected.", {

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

test_that("ds11: datastep() retain class check works as expected", {


  expect_error(datastep(mtcars, retain = c(fork = 0), {fork <- fork + 1}))

})

test_that("ds12: datastep() array class check works as expected", {
  
  
  expect_error(datastep(mtcars, array = c(fork = 0), {fork <- fork + 1}))
  
})


test_that("ds13: datastep() attrib class check works as expected", {
  
  
  expect_error(datastep(mtcars, attrib = c(fork = 0), {fork <- fork + 1}))
  
})



test_that("ds14: Rename works as expected", {

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




test_that("ds15: datastep() attributes on data are maintained.", {
  
  library(dplyr)
  library(common)

  
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

test_that("ds16: datastep retains class attributes.", {
  
  
  s1 <-  1:3
  dt1 <- c(Sys.Date(), Sys.Date() - 1, Sys.Date() - 2)
  df1 <- data.frame(s1, dt1, stringsAsFactors = FALSE)
  
  df2 <- datastep(df1, rename = c(dt1 = "dt2"), {
    csum <- 1
  })
  
  df2
  
  expect_equal(class(df2$s1), "integer")
  expect_equal(class(df2$dt2), "Date")
  expect_equal(class(df2$csum), "numeric")

})

test_that("ds17: datastep works on single column data frame.", {
  
  df <- data.frame(a = 1:10, stringsAsFactors = FALSE)
  
  
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


test_that("ds18: datastep works on single column tibble.", {
  
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


test_that("ds19: datastep() attributes on data are maintained on base dataframe.", {
  
  
  dat <- mtcars
  
  attr(dat$mpg, "label") <- "Miles Per Gallon"
  
  
  dat2 <- datastep(dat, {
    fork <- "Hello" 
  })

  dat2
  
  expect_equal(attr(dat2$mpg, "label"), "Miles Per Gallon")  
  
})
  
  

test_that("ds20: datastep works on tibble.", {
  

    
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



test_that("ds21: datastep works on data.table", {
  
  
  
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



test_that("ds22: datastep() works on a dataframe with a factor.", {
  
  
  dat <- iris
  
  
  dat2 <- datastep(dat, {
    fork <- Petal.Length + Petal.Width 
  })
  
  dat2
  
  expect_equal("fork" %in% names(dat2), TRUE)  
  expect_equal(class(dat2$Species), "factor")
  
})

test_that("ds23: assign_attributes() works as expected.", {
  
  
  dat <- mtcars
  
  lst <- list(mpg = "hello", cyl = "goodbye")
  
  dat2 <- assign_attributes(dat, lst, "label")
  
  
  expect_equal(attr(dat2$mpg, "label"), "hello")
  expect_equal(attr(dat2$cyl, "label"), "goodbye")
})
  
test_that("ds24: label parameter on datastep works as expected.", {
  
  
  dat <- mtcars
  
  lst <- list(mpg = "hello", cyl = "goodbye")
  
  dat2 <- datastep(dat, label = lst, {})
    
    

  
  expect_equal(attr(dat2$mpg, "label"), "hello")
  expect_equal(attr(dat2$cyl, "label"), "goodbye")
})


test_that("ds25: format parameter on datastep works as expected.", {
  
  
  dat <- mtcars
  
  lst <- list(mpg = "%1.1f", cyl = "%1.2f")
  
  dat2 <- datastep(dat, format = lst, {})
  

  expect_equal(attr(dat2$mpg, "format"), "%1.1f")
  expect_equal(attr(dat2$cyl, "format"), "%1.2f")
})

test_that("ds26: Attributes on datastep input is retained inside datastep.", {
  
  library(common)
  
  dat <- mtcars
  
  labels(dat) <- list(mpg = "here", cyl = "there")
  
  attr(dat$mpg, "label")
  
  dat2 <- datastep(dat, format = list(cyl = "%1.1f"), {
    
    mpgf <- attr(mpg, "label")
    mpgf2 <- "Hello"
    cylf <- attr(cyl, "format")
  })
  
  dat2
  
  expect_equal("mpgf" %in% names(dat2), TRUE)
  expect_equal(dat2[1, "mpgf"], "here")
  expect_equal("cylf" %in% names(dat2), TRUE)
  expect_equal(dat2[1, "cylf"], "%1.1f")
  
})

test_that("ds27: date variables are retained as dates.", {
  
  
  ind <- mtcars
  ind$mydate <- Sys.Date()
  
  df <- datastep(ind, {

    if (mpg >= 20)
      mpgcat <- "High"
    else
      mpgcat <- "Low"

    recdt <- as.Date("1974-06-10")

    if (cyl == 8)
      is8cyl <- TRUE
    else
      is8cyl <- FALSE

  })
  

  df

  
  a1 <- attributes(df$recdt)
  a2 <- attributes(df$mydate)

  
  expect_equal(a1$class, "Date") 
  expect_equal(a2$class, "Date") 
    
})


test_that("ds28: where clause works.", {
  
  df <- datastep(mtcars,
                 where = expression(cyl == 8),
                  {
                   
                   if (mpg >= 20)
                     mpgcat <- "High"
                   else
                     mpgcat <- "Low"
                   
                   recdt <- as.Date("1974-06-10")
                   
                   if (cyl == 8)
                     is8cyl <- TRUE
                   else
                     is8cyl <- FALSE
                   
                 })
  
  df
  
  expect_equal(mean(df$cyl), 8)
  
})


test_that("ds29: attributes are retained with keep statement.", {
  
  
  ind <- mtcars
  ind$mydate <- Sys.Date()
  
  df <- datastep(ind, 
                 format = list(cyl = "%.1f", 
                               mydate = "%b %m %Y", 
                               recdt = "%b %m %y"),
                 keep = c("mpg", "cyl", "recdt", "mydate"), {
    

    recdt <- as.Date("1974-06-10")
    
    
  })
  
  df
  
  a1 <- attributes(df$recdt)
  a2 <- attributes(df$mydate)
  a3 <- attributes(df$cyl)
  
  
  expect_equal(a1$class, "Date") 
  expect_equal(a2$class, "Date") 
  expect_equal(a1$format,  "%b %m %y") 
  expect_equal(a2$format, "%b %m %Y") 
  expect_equal(a3$format, "%.1f")
  
})

test_that("ds30: datastep() keep parameter with one variable works.", {
  
  libname(dat, base_path, "csv")
  
  lib_load(dat)
  
  d1 <- datastep(dat.demo_studya, keep = c("study"), {})
  
  d1
  
  expect_equal("data.frame" %in% class(d1), TRUE)
  expect_equal(names(d1), c("study"))
  
  
})

# test_that("output variable  on datastep works as expected.", {
#   
#   
#   dat <- datastep(mtcars, {if (cyl == 8) output = TRUE})
#   
#   
#   expect_equal("output" %in% names(dat), FALSE)
#   expect_equal(nrow(dat), 14)
# })


test_that("ds31: Single value NSE works on datastep().", {
  
  
  d1 <- datastep(mtcars, 
                 drop = am, 
                 keep = v(mpg, cyl, disp, cylgrp),
                 by = cyl, 
                 sort_check = FALSE, {
    
    if (first.)
      cylgrp <- "begin"
    else
      cylgrp <- "-"
    
  })
  
  d1
  
  expect_equal(ncol(d1), 4)
  
  
  d2 <- datastep(d1, keep = cylgrp, {})
  
  expect_equal(ncol(d2), 1)
  
})

test_that("ds32: Delete function works on datastep().", {
  
  
  d1 <- datastep(mtcars, 
                 keep = v(mpg, cyl, disp, cylgrp),
                 by = cyl, 
                 sort_check = FALSE, {
                   
                   if (first.)
                     cylgrp <- "begin"
                   else
                     delete() 
                   
                 })
  
  d1
  
  expect_equal(ncol(d1), 4)
  expect_equal(nrow(d1), 16)
  
  # Should get no errors
  d2 <- datastep(mtcars, 
                 {delete()})
  
  d2
  
  expect_equal(nrow(d2), 0)
  expect_equal(ncol(d2), 11)
  
})


test_that("ds33: Output function works as expected.", {
  
  
  d1 <- datastep(mtcars, 
                 {
                   
                   if (cyl == 4)
                     output()
                   
                 })
  
  d1
  
  expect_equal(nrow(d1), 11)
  expect_equal(ncol(d1), 11)
  
})

test_that("ds34: has_output() function works.", {
  
  str1 <- "if (cyl == 4) output()"
  
  res1 <- has_output(str1)
  
  res1
  
  expect_equal(res1, TRUE)
  
  
  str2 <- "if (cyl == 4) delete()"
  
  
  res2 <- has_output(str2)
  
  res2
  
  expect_equal(res2, FALSE)
  
})


test_that("ds35: Output function can output multiple rows per obs.", {
  
  
  d1 <- datastep(mtcars, 
                 {
                   
                   fork <- "hello"
                   bork <- "sammy"
                   
                   if (cyl == 4) {
                     seq <- 1
                     output()
                     seq <- 2
                     output()
                     
                   }
                   
                   # Never executed
                   andalso <- "here"
                   
                 })
  
  d1
  
  expect_equal(nrow(d1), 22)
  expect_equal(ncol(d1), 14)
  
})



test_that("ds35: delete and output can be used together.", {
  
  
  d1 <- datastep(mtcars, 
                 {
                   
                   
                   if (cyl == 4) {
                     delete()
                     
                   }
                   
                   output()
                   
                 })
  
  d1
  
  expect_equal(nrow(d1), 21)
  expect_equal(ncol(d1), 11)
  
})


test_that("ds36: output works with empty dataset.", {
  
  d1 <- datastep(data.frame(), {
    
    bork <- 1
    fork <- "one"
    output()
    
    bork <- 2
    fork <- "two"
    output()
    
  })
  
  # print(d1)
  # print(attributes(d1))
  # print(attributes(d1$fork))
  d1
  
  expect_equal(nrow(d1), 2)
  expect_equal(ncol(d1), 2)
  expect_equal(names(d1), c("bork", "fork"))
  expect_equal(d1[[1, 1]], 1)
  # if ("factor" %in% class(d1[[2, 2]]))
  #   expect_equal(d1[[2, 2]], 2)
  # else
  #   expect_equal(d1[[2, 2]], "two")
    
})


test_that("ds37: datastep strips and restores extra classes.", {
  
  d2 <- mtcars
  
  class(d2) <- c("fork", class(d2))
  
  d1 <- datastep(d2, 
                 {
                   
                   
                   if (cyl == 4) {
                     delete()
                     
                   }

                   
                 })
  
  d1
  
  class(d1)
  
  expect_equal(class(d1), c("fork", "data.frame"))
  
})

test_that("ds38: no row data frame works with output.", {
  
  
  d1 <- subset(mtcars, mtcars$cyl == 10)
  
  d2 <- datastep(d1, {
    
    bork <- 1
    fork <- "one"
    output()
    
    bork <- 2
    fork <- "two"
    output()
    
  })
  
  d2
  
  expect_equal(nrow(d2), 2)
  expect_equal(ncol(d2), 13)
  
})

test_that("ds39: no row warning works.", {
  
  
  d1 <- subset(mtcars, mtcars$cyl == 10)
  
  expect_warning(datastep(d1, {
    
    bork <- 1

  }))
  

  
})

# Works interactively but not during test_that run
# Not sure what is going on.  Datastep can't find dslst.
# test_that("ds40: output works in loop", {
# 
# 
#   dslst <- list("mtcars" = mtcars, "beaver1" = beaver1, "iris" = iris)
# 
#   # Create metadata
#   res3 <- datastep(data.frame(), {
# 
# 
#     for (name in names(dslst)) {
#       rows <- nrow(dslst[[name]])
#       cols <- ncol(dslst[[name]])
#       output()
#     }
# 
# 
#   })
# 
# 
#   res3
# 
#   expect_equal(nrow(res3), 3)
#   expect_equal(ncol(res3), 3)
# 
# })

test_that("ds41: perform_set function works.", {
  
  dat1 <- mtcars[1:10, 1:10]
  dat2 <- mtcars[11:20, 2:11]
  
  res1 <- perform_set(dat1, dat2)
  
  
  res1
  
  expect_equal(nrow(res1), 20)
  expect_equal(ncol(res1), 11)

  dat1$char <- "top"
  dat2$char <- "middle"
  dat3 <- dat2
  dat3$char <- "bottom"
  
  res2 <- perform_set(dat1, list(dat2, dat3))
  
  
  res2
  
  expect_equal(nrow(res2), 30)
  expect_equal(ncol(res2), 12)
  
  dat1$char <- as.factor(dat1$char)
  dat2$char <- as.factor(dat2$char)
  dat3$char <- as.factor(dat3$char)
  
  res3 <- perform_set(dat1, list(dat2, dat3))
  
  
  res3
  
  expect_equal(nrow(res3), 30)
  expect_equal(ncol(res3), 12)
  expect_equal(levels(res3$char), c("top", "middle", "bottom"))

  
})


test_that("ds42: perform_merge function works.", {
  
  dat1 <- read.table(header = TRUE, text = '
    ID NAME
    A01 SUE
    A02 TOM
    A05 KAY
    A10 JIM
  ')
  
  dat2 <- read.table(header = TRUE, text = '
    ID AGE SEX
    A01 58 F
    A02 20 M
    A05 47 F
    A10 11 M
    A11 23 F
  ')
  
  dat1
  dat2
  
  res1 <- perform_merge(dat1, dat2, "ID", NULL)
  
  res1
  
  expect_equal(nrow(res1), 5)
  expect_equal(ncol(res1), 4)
  
  
  res2 <- perform_merge(dat1, dat2, "ID", c("INA", "INB"))
  
  res2
  
  expect_equal(nrow(res2), 5)
  expect_equal(ncol(res2), 6)
  
  dat3 <- read.table(header = TRUE, text = '
    ID STATUS
    A02 ACTIVE
  ')
  
  res3 <- perform_merge(dat1, list(dat2, dat3), "ID", c("INA", "INB", "INC"))
  
  res3
  
  expect_equal(nrow(res3), 5)
  expect_equal(ncol(res3), 8)
  
  dat4 <- read.table(header = TRUE, text = '
    ID WEIGHT
    A05 23
  ')
  
  res4 <- perform_merge(dat1, list(dat2, dat3, dat4), 
                        c("ID"), 
                        c("INA", "INB", "INC", "IND"))
  
  res4
  
  expect_equal(nrow(res4), 5)
  expect_equal(ncol(res4), 10)
  
  res5 <- perform_merge(dat1, list(dat2, dat3, dat4), 
                        c("ID"), 
                        c("INA", "INB", "INC"))
  
  res5
  
  expect_equal(nrow(res5), 5)
  expect_equal(ncol(res5), 9)
  
  

})

test_that("ds43: datastep with merge works.", {
  
  dat1 <- read.table(header = TRUE, text = '
    ID NAME
    A01 SUE
    A02 TOM
    A05 KAY
    A10 JIM
  ')
  
  dat2 <- read.table(header = TRUE, text = '
    ID AGE SEX
    A01 58 F
    A02 20 M
    A05 47 F
    A10 11 M
    A11 23 F
  ')
  
  dat1
  dat2
  
  res1 <- datastep(dat1, merge = dat2, merge_by = "ID", {})
  
  res1
  
  expect_equal(nrow(res1), 5)
  expect_equal(ncol(res1), 4)
  
  
  res2 <- datastep(dat1, merge = dat2, merge_by = "ID", 
                   merge_in = c("INA", "INB"),
                   where = expression(INA == 1), {})
  
  res2
  
  expect_equal(nrow(res2), 4)
  expect_equal(ncol(res2), 6)
  
  
  
  res3 <- datastep(dat1, merge = dat2, merge_by = "ID", 
                   merge_in = c("INA", "INB"),
                   where = expression(INA == 0 & INB == 1), {})
  
  res3
  
  expect_equal(nrow(res3), 1)
  expect_equal(ncol(res3), 6)
  

  
  dat4 <- read.table(header = TRUE, text = '
    NO AGE SEX
    A01 58 F
    A02 20 M
    A05 47 F
    A10 11 M
    A11 23 F
  ')
  
  dat1
  dat4
  
  res4 <- datastep(dat1, merge = dat4, merge_by = c("ID" = "NO"), {})
  
  res4
  
  expect_equal(nrow(res4), 5)
  expect_equal(ncol(res4), 4)
  
  
  expect_error( datastep(dat1, merge = dat4, merge_by = c("IDS" = "NO"), {}))
  expect_error( datastep(dat1, merge = dat4, merge_by = c("ID" = "NUM"), {}))
  
})

test_that("ds44: datastep with set works.", {
  
  dat1 <- mtcars[1:10, 1:10]
  dat2 <- mtcars[11:20, 2:11]
  
  res1 <- datastep(dat1, set = dat2, {})
  
  
  res1
  
  expect_equal(nrow(res1), 20)
  expect_equal(ncol(res1), 11)
  
  
})
  

test_that("ds45: keep and drop checks work.", {
  
  res1 <- datastep(mtcars, keep = c("mpg", "cyl", "fork"), {})
  
  expect_equal(ncol(res1), 2)
  
  res2 <- datastep(mtcars, drop = c("mpg", "cyl", "fork"), {})
  
  expect_equal(ncol(res2), 9)
  
})

test_that("ds44: Make sure cols not dropped.", {

  dat1 <- read.table(header = TRUE, text = '
      NAME ID SEX
      SUE A01   O
      TOM A02   O
      KAY A05   O
      JIM A10   O
    ')
  
  dat2 <- read.table(header = TRUE, text = '
      ID AGE SEX
      A01 58 F
      A02 20 M
      A05 47 F
      A10 11 M
      A11 23 F
    ')
  
  dat1
  dat2
  
  
  res1 <- datastep(dat1, merge = dat2, merge_by = "ID", {})
  
  
  res1
  
  expect_equal(nrow(res1), 5)
  expect_equal(ncol(res1), 5)

})

test_that("ds46: fix_names works as expected.", {
  
  v1 <- c("A", "B", "C", "D")
  v2 <- c("A", "E", "B", "F")
  ky <- "A"
  sfx <- c(".1", ".2")
  
  res <- fix_names(v1, v2, ky, sfx)
  
  res  
  
  expect_equal(res, c("A", "B.1", "C", "D", "E", "B.2", "F"))
  
})

test_that("ds47: column append with no merge_by works equal rows.", {
  
  dat1 <- read.table(header = TRUE, text = '
      NAME ID SEX
      SUE A01   O
      TOM A02   O
      KAY A05   O
      JIM A10   O
    ')
  
  dat2 <- read.table(header = TRUE, text = '
      ID AGE SEX
      A01 58 F
      A02 20 M
      A05 47 F
      A10 11 M
    ')
  
  dat1
  dat2
  
  
  res1 <- datastep(dat1, merge = dat2, {})
  
  
  res1
  
  expect_equal(nrow(res1), 4)
  expect_equal(ncol(res1), 6)
  expect_equal(names(res1), c("NAME", "ID.1", "SEX.1", "ID.2", "AGE", "SEX.2"))
  
})

test_that("ds48: column append with no merge_by works unequal rows.", {
  
  dat1 <- read.table(header = TRUE, text = '
      NAME ID SEX
      SUE A01   O
      TOM A02   O
      KAY A05   O
      JIM A10   O
    ')
  
  dat2 <- read.table(header = TRUE, text = '
      ID AGE SEX
      A01 58 F
      A02 20 M
      A05 47 F
    ')
  
  dat1
  dat2
  
  
  res1 <- datastep(dat1, merge = dat2, {})
  
  
  res1
  
  expect_equal(nrow(res1), 4)
  expect_equal(ncol(res1), 6)
  expect_equal(names(res1), c("NAME", "ID.1", "SEX.1", "ID.2", "AGE", "SEX.2"))
  
})

test_that("ds49: fill_missing() works as expected.", {
  
  dat2 <- read.table(header = TRUE, text = '
      ID AGE SEX
      A01 58 F
      A02 20 M
      A05 47 F
    ')
  
  res1 <- fill_missing(dat2, 4)
  
  res1  
  
  expect_equal(nrow(res1), 4)
  
  res2 <- fill_missing(dat2, 10)
  
  res2  
  
  expect_equal(nrow(res2), 10)
  
  res3 <- fill_missing(dat2, 2)
  
  res3  
  
  expect_equal(nrow(res3), 3)
  
})


test_that("ds50: copy_df_attributes works as expected.", {
  
  library(tibble)
  
  dat1 <- as_tibble(mtcars[1:10, c("mpg", "cyl", "disp")])
  labels(dat1) <- list(mpg = "Miles per gallon",
                       cyl = "Cylinders",
                       disp = "Displacement")
  
  dat2 <- mtcars[11:25, c("mpg", "cyl", "disp")]
  
  res1 <- copy_df_attributes(dat1, dat2)
  
  res1
  
  expect_equal(rownames(res1)[1], "Merc 280C")
  expect_equal("tbl_df" %in% class(res1), TRUE)
  
})

test_that("ds51: Set keeps dataset attributes.", {
  
  library(tibble)
  
  dat1 <- as_tibble(mtcars[1:10, c("mpg", "cyl", "disp")])
  labels(dat1) <- list(mpg = "Miles per gallon",
                       cyl = "Cylinders",
                       disp = "Displacement")
  
  dat2 <- as_tibble(mtcars[11:20, c("mpg", "cyl", "disp")])

  
  res1 <- datastep(dat1, set = dat2, {})
  
  res1
  
  expect_equal("tbl_df" %in% class(res1), TRUE)
  expect_equal(rownames(res1)[1], "1")
  
  lbls <- labels(res1)

  expect_equal(lbls[[1]], "Miles per gallon")
  expect_equal(lbls[[2]], "Cylinders")
  expect_equal(lbls[[3]], "Displacement")
  
})

test_that("ds52: Merge append keeps dataset attributes.", {
  
  library(tibble)
  
  dat1 <- as_tibble(mtcars[1:10, c("mpg", "cyl", "disp")])
  labels(dat1) <- list(mpg = "Miles per gallon",
                       cyl = "Cylinders",
                       disp = "Displacement")
  
  dat2 <- as_tibble(mtcars[11:20, c("hp", "drat", "wt")])
  labels(dat2) <- list(hp = "Horsepower",
                       wt = "Weight")
  
  res1 <- datastep(dat1, merge = dat2, {})
  
  res1
  
  expect_equal("tbl_df" %in% class(res1), TRUE)
  expect_equal(rownames(res1)[1], "1")
  
  lbls <- labels(res1)
  
  lbls
  
  expect_equal(lbls[[1]], "Miles per gallon")
  expect_equal(lbls[[2]], "Cylinders")
  expect_equal(lbls[[3]], "Displacement")
  
})


test_that("ds53: Merge by keeps dataset attributes.", {
  
  library(tibble)
  
  dat1 <- as_tibble(mtcars[1:10, c("mpg", "cyl", "disp")])
  labels(dat1) <- list(mpg = "Miles per gallon",
                       cyl = "Cylinders",
                       disp = "Displacement")
  
  dat2 <- tibble(cyl = c(4, 6, 8), 
                 lbl = c("4 Cylinders", 
                         "6 Cylinders",
                         "8 Cylinders"),
                 disp = rep(1, 2, 3))
  
  labels(dat2) <- list(cyl = "Cylinders",
                       lbl = "Label",
                       disp = "Displacement 2")
  
  res1 <- datastep(dat1, merge = dat2, merge_by = cyl,{})
  
  res1
  
  expect_equal("tbl_df" %in% class(res1), TRUE)
  expect_equal(rownames(res1)[1], "1")
  
  lbls <- labels(res1)
  
  lbls
  
  expect_equal(lbls[[1]], "Miles per gallon")
  expect_equal(lbls[[2]], "Cylinders")
  expect_equal(lbls[[3]], "Displacement")
  expect_equal(lbls[[4]], "Label")
  expect_equal(lbls[[5]], "Displacement 2")
  
})


test_that("ds43: datastep join works.", {
  
  dat1 <- read.table(header = TRUE, text = '
    ID NAME
    A01 SUE
    A02 TOM
  ')
  
  dat2 <- read.table(header = TRUE, text = '
    ID AGE SEX CODE
    A01 58 F    A01
    A02 20 M    A02
    A01 47 F    A01
    A02 11 M    A02
    A01 23 F    A01
  ')
  
  dat1
  dat2
  
  res1 <- datastep(dat1, merge = dat2, merge_by = "ID", {})
  
  res1
  
  expect_equal(nrow(res1), 5)
  expect_equal(ncol(res1), 5)
  
  res2 <- datastep(dat1, merge = dat2, merge_by = c(ID = "CODE"), {})
  
  
  expect_equal(nrow(res2), 5)
  expect_equal(ncol(res2), 5)
  
})

test_that("ds44: datastep multiple renames works.", {
  
  
  dat2 <- read.table(header = TRUE, text = '
    ID AGE SEX CODE
    A01 58 F    A01
    A02 20 M    A02
    A01 47 F    A01
    A02 11 M    A02
    A01 23 F    A01
  ')

  dat2
  
  res2 <- datastep(dat2,  
                   rename = c(AGE = "TITLE", SEX = "FORK"),{})
  
  res2
  expect_equal(nrow(res2), 5)
  expect_equal(ncol(res2), 4)
  expect_equal(names(res2), c("ID", "TITLE", "FORK", "CODE"))
  
})


test_that("ds43: Merge dataset names are not 'fixed' when using tibbles.", {
  
  library(tibble)
  
  dat1 <- read.table(header = TRUE, text = '
    ID2 NAME
    A01 SUE
    A02 TOM
  ')
  
  dat2 <- read.table(header = TRUE, text = '
    ID "AGE 1" "SEX 2" "CODE 4"
    A01 58 F    A01
    A02 20 M    A02
    A01 47 F    A01
    A02 11 M    A02
    A01 23 F    A01
  ', check.names = FALSE)
  
  dat1 <- as_tibble(dat1)
  dat2 <- as_tibble(dat2)
  
  
  res1 <- datastep(dat2, merge = dat1, merge_by = c("ID" = "ID2"), {}, 
                   log = FALSE)
  
  res1
  
  expect_equal(nrow(res1), 5)
  expect_equal(ncol(res1), 5)
  expect_equal(names(res1), c("ID", "AGE 1", "SEX 2", "CODE 4", "NAME"))
  
})


test_that("ds44: Merge works with factors.", {
  

  # Create sample data
  grp1 <- read.table(header = TRUE, text = '
    GROUP  NAME
    G01  Group1
    G02  Group2
  ', stringsAsFactors = TRUE)
  
  grp2 <- read.table(header = TRUE, text = '
    GROUP  NAME
    G03  Group3
    G04  Group4
  ', stringsAsFactors = TRUE)
    
  dat <- read.table(header = TRUE, text = '
    ID AGE SEX GROUP
    A01 58 F    G01
    A02 20 M    G02
    A03 47 F    G05
    A04 11 M    G03
    A05 23 F    G01
  ', stringsAsFactors = TRUE)
  
  # Set operation
  grps <- datastep(grp1, set = grp2, {})
  grps
  
  
  # Merge operation - Outer Join
  res <- datastep(dat, merge = grps, 
                  merge_by = "GROUP", 
                  merge_in = c("inA", "inB"), {})
                
                
  expect_equal(nrow(res), 6)
  expect_equal(ncol(res), 7)
  
})

