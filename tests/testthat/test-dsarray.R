context("Datastep Array Tests")

base_path <- "c:\\packages\\libr\\tests\\testthat\\data"

base_path <- "./data"

DEV <- FALSE


test_that("dsarray() function works", {
  
  
  dsa <- dsarray("one", "two", "three", "four")
  
  expect_equal(length(dsa), 4)
  expect_equal(length(names(dsa)), 4)
  expect_equal("dsarray" %in% class(dsa), TRUE)
  expect_equal(as.character(dsa), c("one", "two", "three", "four"))
  expect_equal(names(dsa), c("one", "two", "three", "four"))
  
})




test_that("dsarray function works with character index.", {
  
  
  
  d1 <- datastep(mtcars, arrays = list(dsa = dsarray("vs", "am", "gear")), {
    
    temp <- dsa["am"]
    
  })
  
  expect_equal("temp" %in% names(d1), TRUE)
  expect_equal(d1[["temp"]], mtcars[["am"]])
  
})


test_that("dsarray function works with numeric index.", {
  
  
  
  d1 <- datastep(mtcars, arrays = list(dsa = dsarray("vs", "am", "gear")), {
    
    temp <- dsa[2]
    
  })
  
  expect_equal("temp" %in% names(d1), TRUE)
  expect_equal(d1[["temp"]], mtcars[["am"]])
  
})


test_that("dsarray function works with no index.", {
  
 
  
  d1 <- datastep(mtcars, {
    
    temp <- sum(dsa[])
    temp2 <- mean(dsa[])
    
  }, arrays = list(dsa = dsarray("vs", "am", "gear")))
  
  expect_equal("temp" %in% names(d1), TRUE)
  expect_equal("temp2" %in% names(d1), TRUE)
  expect_equal(d1[["temp"]], mtcars[["vs"]] + mtcars[["am"]] + mtcars[["gear"]])
  
})


test_that("dsarray dynamic assignment works as expected.", {
  
  
  
  d1 <- datastep(mtcars, {
    
    temp <- !dsa[1]
    
    for (nm in dsa) {
      
     assign(nm, dsa[nm] + 2)
    }

    
  }, arrays = list(dsa = dsarray("vs", "am", "gear")))
  
  d1
  
  expect_equal("temp" %in% names(d1), TRUE)
  expect_equal(d1[["vs"]], mtcars[["vs"]] + 2)
  expect_equal(d1[["am"]], mtcars[["am"]] + 2)
  expect_equal(d1[["gear"]], mtcars[["gear"]] + 2)
  
})


test_that("length.dsarray works as expected.", {
  
  dsa <- dsarray("vs", "am", "gear")
  
  expect_equal(length(dsa), 3)
  

  
})

test_that("names.dsarray works as expected.", {
  
  dsa <- dsarray("vs", "am", "gear")
  
  as.character(dsa)
  
  expect_equal(names(dsa), c("vs", "am", "gear"))
  
  
  
})

test_that("dsarray dynamic assignment to new variables works as expected.", {
  
  
  
  
  d1 <- datastep(mtcars, arrays = list(dsa = dsarray("vs", "am", "gear"),
                                       dsa1 = c("vs1", "am1", "gear1")), 
                 steps = {
  
    
    for (i in seq_along(dsa)) {
      
      assign(dsa1[i], dsa[i] * 2)
    }
    
    
  })
  
  d1
  
  expect_equal("vs1" %in% names(d1), TRUE)
  expect_equal(d1[["vs1"]], mtcars[["vs"]] * 2)
  expect_equal(d1[["am1"]], mtcars[["am"]] * 2)
  expect_equal(d1[["gear1"]], mtcars[["gear"]] * 2)

})



test_that("for loop and data type check works as expected.", {
  
  d1 <- datastep(mtcars, 
                 arrays = list(dsa = dsarray("vs", "am", "gear", "fork")),
                 calculate = {
    
    fork <- "my value"
    
    
  }, drop = "nm",
    steps =             
                 
    {
    rownum <- n.
    
    for (nm in dsa) {
      if (class(dsa[nm]) == "character")
          assign(nm, paste(dsa[nm], n.))
      else            
          assign(nm, dsa[nm] + n.)
    }
    
    
  })
  
  d1
  
  expect_equal("fork" %in% names(d1), TRUE)
  expect_equal("rownum" %in% names(d1), TRUE)

  
})




