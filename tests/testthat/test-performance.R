context("Datastep Performance Tests")

base_path <- "c:\\packages\\libr\\tests\\testthat\\data"

base_path <- "./data"

DEV <- FALSE


# Baseline of 12.6 sec on condition
test_that("datastep() performance is good", {
  
  if (DEV) {
    
    libname(dat, file.path(base_path, "SDTM"), "sas7bdat")
    
    dat2 <- data.frame(dat$lb, n. = seq(1, nrow(dat$lb)))
    
    tm <- Sys.time()
   res <-  split(dat2, dat2$n.)
    
   tmdiff <- Sys.time() - tm
   tmdiff
   
   res[[2]]
  
   
   t(res[[2]])
    
    fanny <- Vectorize(function() {
      if (is.na(LBBLFL))
        blisna <- TRUE
      else
        blisna <- FALSE
      
    })
    
    res <- within(dat$lb, fanny)
    
    names(res)
    
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
    
    expect_equal(tmdiff < 1, TRUE)
    
  } else
    expect_equal(TRUE, TRUE)
  
})

# Jumps to 13.1 seconds when retain added
test_that("datastep() performance with retain is good", {
  
  
  libname(dat, file.path(base_path, "SDTM"), "sas7bdat")
  
  data <- as.data.frame(dat$lb)
  
  ret <- list()
  
  tm <- Sys.time()
  
  for (i in seq_len(nrow(data)))
  {

   data[i, "fork"] <- i

   ret[[i]] <-  data[i, ]

  }
  
  
  ret2 <- list()
  
  for (i in seq_len(nrow(data)))
  {
    
    
    ret2[[i]] <-  ret[[i]][]
    
  }
  

  forkfunc <- Vectorize(function(dat) {
    
    
    dat[["sammy"]] <- dat[["LBBLFL"]] 
    
    return(dat)
    
  })
  
  res2 <- forkfunc(ret)
  
  res2[1]
  
  ret[2]
  
  #d2 <- bind_rows(ret, .id = "column_label")
  
  tmdiff <- Sys.time() - tm
  tmdiff
  
  
  
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
    
    expect_equal(tmdiff < 16, TRUE)
    
  } else
    expect_equal(TRUE, TRUE)
  
})

# Still less than < 1 second when group by added
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
    
    expect_equal(tmdiff < 16, TRUE)
    
  } else
    expect_equal(TRUE, TRUE)
  
})

# Jumps to 14 seconds when retain and group by added
test_that("datastep() performance with retain is good", {
  
  if (DEV) {
    
    libname(dat, file.path(base_path, "SDTM"), "sas7bdat")
    
    tm <- Sys.time()
    
    res <- datastep(dat$lb, retain = list(rnum = 0),
                    by = c("USUBJID"),
                    {
                      if (first.)
                        subjstart <- TRUE
                      else
                        subjstart <- FALSE
                      
                      rnum <- rnum + 1
                      
                    })
    
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
    
    expect_equal(tmdiff < 20, TRUE)
    
  } else 
    expect_equal(TRUE, TRUE)
  
})


