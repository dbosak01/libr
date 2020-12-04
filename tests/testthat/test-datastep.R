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

test_that("datastep() auto-group-by works as expected.", {
  
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


test_that("datastep() by parameter works as expected.", {
  
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



