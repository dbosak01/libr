context("Datastep Tests")

base_path <- "c:\\packages\\libr\\tests\\testthat\\data"

base_path <- "./data"

test_that("datastep() function works as expected with mtcars.", {
  
  d1 <- datasets::iris
  

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
