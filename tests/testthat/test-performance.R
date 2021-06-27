context("Datastep Performance Tests")

base_path <- "c:\\packages\\libr\\tests\\testthat\\data"

base_path <- "./data"

DEV <- FALSE


test_that("add_autos() function works as expected", {
  
  libname(dat, file.path(base_path, "SDTM"), "sas7bdat")

  tm <- Sys.time()
  
  dat3 <- add_autos(dat$lb, c("USUBJID", "LBCAT", "LBTESTCD"))
  
  
  tmdiff <- Sys.time() - tm
  tmdiff
  
  expect_equal("first." %in% names(dat3), TRUE)
  expect_equal("last." %in% names(dat3), TRUE)

})

test_that("sort check works as expected", {

  dat3 <- add_autos(mtcars, c("am"), sort_check = FALSE)
  

  expect_equal("first." %in% names(dat3), TRUE)
  expect_equal("last." %in% names(dat3), TRUE)
  expect_error( add_autos(mtcars, c("am"), sort_check = TRUE))
})


# Baseline of 10 sec on condition
test_that("datastep() performance is good", {
  
  if (DEV) {
    
    libname(dat, file.path(base_path, "SDTM"), "sas7bdat")

    
    tm <- Sys.time()
    
    res <- datastep(dat$lb, 
                    {
                      if (is.na(LBBLFL))
                        blisna <- TRUE
                      else
                        blisna <- FALSE
                      
                      
                    })
    
    tmdiff <- Sys.time() - tm
    tmdiff
    
    expect_equal(tmdiff < 10, TRUE)
    
  } else
    expect_equal(TRUE, TRUE)
  
})

# Jumps to 14 seconds when retain added
test_that("datastep() performance with retain is good", {
  
  
  libname(dat, file.path(base_path, "SDTM"), "sas7bdat")
  
  
  if (DEV) {
    
    libname(dat, file.path(base_path, "SDTM"), "sas7bdat")
    
    
    tm <- Sys.time()
    
    res <- datastep(dat$lb, retain = list(rnum = 0),
                    {
                      if (is.na(LBBLFL))
                        subjstart <- TRUE
                      else
                        subjstart <- FALSE
                      
                      rnum <- 2 + 1
                      
                    })
    
    tmdiff <- Sys.time() - tm
    tmdiff
    
    expect_equal(tmdiff < 14, TRUE)
    
  } else
    expect_equal(TRUE, TRUE)
  
})

# Still less than < 11 seconds when group by added
test_that("datastep() performance with by group is good", {
  
  if (DEV) {
    
    libname(dat, file.path(base_path, "SDTM"), "sas7bdat")
    
    
    tm <- Sys.time()
    
    res <- datastep(dat$lb, 
                    by = c("USUBJID"),
                    {
                      if (first.)
                        subjstart <- TRUE
                      else
                        subjstart <- FALSE
                      
                    #  rnum <- rnum + 1
                      
                    })
    
    tmdiff <- Sys.time() - tm
    tmdiff
    
    expect_equal(tmdiff < 11, TRUE)
    
  } else
    expect_equal(TRUE, TRUE)
  
})

# Jumps to 14 seconds when retain and group by added
test_that("datastep() performance with retain is good", {
  
  if (DEV) {
    
    libname(dat, file.path(base_path, "SDTM"), "sas7bdat")
    
    tm <- Sys.time()
  
    
    res <- datastep(dat$lb, retain = list(rnum = 0),
                    by = c("USUBJID", "LBCAT", "LBTESTCD"),
                    {
                      if (first.)
                        subjstart <- TRUE
                      else
                        subjstart <- FALSE
                      
                      rnum <- rnum + 1
                      
                      rnum2 <- n.
                      
                    }, sort_check = TRUE)
    
    tmdiff <- Sys.time() - tm
    tmdiff
    
    expect_equal(tmdiff < 16, TRUE)
    
  } else
    expect_equal(TRUE, TRUE)
  
})


test_that("datastep() with group_by performance is good", {
  
  if (DEV) {
    
    library(dplyr)
    
    scs <- specs(PE = import_spec(PESTAT = "character"))
    
    libname(dat, file.path(base_path, "SDTM"), "csv", import_specs = scs)
    
    tm <- Sys.time()
    
    prep <- dat$DM %>% 
      left_join(dat$VS, by = c("USUBJID" = "USUBJID")) %>% 
      select(USUBJID, VSTESTCD, VISIT, VISITNUM, VSSTRESN, ARM, VSBLFL) %>% 
      filter(VSTESTCD %in% c("PULSE", "RESP", "TEMP", "DIABP", "SYSBP"), 
             !(VISIT == "SCREENING" & VSBLFL != "Y")) %>% 
      arrange(USUBJID, VSTESTCD, VISITNUM) %>% 
      group_by(USUBJID, VSTESTCD) %>%
      #datastep(by = c("USUBJID", "VSTESTCD"), retain = list(BSTRESN = 0), {
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
    
    tmdiff <- Sys.time() - tm 
    tmdiff
    
    expect_equal(tmdiff < 3, TRUE)
    
  } else 
    expect_equal(TRUE, TRUE)
  
})


test_that("100,000 row datastep on data.frame is good.", {
  
  if (DEV) {
    
    l <- 100000
    
    df <- data.frame(C1 = seq_len(l), C2 = runif(l), 
                     C3 = runif(l), C4 = runif(l))
    
    tm <- Sys.time()
    
    res <- datastep(df, attrib = list(C5 = 0, C6 = 0),
                    {
                      C5 <- C2 + C3 + C4
                      C6 <- max(C2, C3, C4)
                      
                    })
    
    
    tmdiff <- Sys.time() - tm
    tmdiff
    
    res[1:10, ]
    
    expect_equal(tmdiff < 30, TRUE)
    
    # 100,000 rows is 20 seconds
    # 1,000,000 rows is 3.6 minutes
    
  } else
    expect_equal(TRUE, TRUE)
  
})


test_that("100,000 row datastep on tibble is good.", {
  
  if (DEV) {
    
    library(tibble)
    
    l <- 100000
    
    df <- tibble(C1 = seq_len(l), C2 = runif(l), 
                     C3 = runif(l), C4 = runif(l))
    
    tm <- Sys.time()
    
    res <- datastep(df, attrib = list(C5 = 0, C6 = 0),
                    {
                      C5 <- C2 + C3 + C4
                      C6 <- max(C2, C3, C4)
                      
                    })
    
    
    tmdiff <- Sys.time() - tm
    tmdiff
    
    res[1:10, ]
    
    expect_equal(tmdiff < 30, TRUE)
    
    # 100,000 rows is 21.2 seconds
    # 1,000,000 rows is 3.8 minutes
    
  } else
    expect_equal(TRUE, TRUE)
  
})



test_that("100,000 row datastep on data.table is good.", {
  
  if (DEV) {
    
    library(data.table)
    
    l <- 100000
    
    df <- data.table(C1 = seq_len(l), C2 = runif(l), 
                 C3 = runif(l), C4 = runif(l))
    
    tm <- Sys.time()
    
    res <- datastep(df, attrib = list(C5 = 0, C6 = 0),
                    {
                      C5 <- C2 + C3 + C4
                      C6 <- max(C2, C3, C4)
                      
                    })
    
    
    tmdiff <- Sys.time() - tm
    tmdiff
    
    res[1:10, ]
    
    expect_equal(tmdiff < 30, TRUE)
    
    # 100,000 rows is 1.3 minutes with out modification to datastep
    # 100,000 rows is 22.3 seconds after modification 
    # 1,000,000 rows is not going to try
    
  } else
    expect_equal(TRUE, TRUE)
  
})



