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
  
  expect_equal(names(d1), c("study", "inv", "patient", "race", 
                            "dob", "treatment", "sexc", "usubjid"))
  
})


test_that("datastep() by parameter works as expected.", {
  
  libname(dat, base_path, "rds")
  
  lib_load(dat)
  
  
  d1 <- datastep(dat.demo_studya, by = c("treatment"), {
    
    f1 <- first.
    l1 <- last.
    
  })
  
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
