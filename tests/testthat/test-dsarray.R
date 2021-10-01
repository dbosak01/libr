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


test_that("dsarray function works with factors.", {

  dat <- mtcars
  dat$cat <- factor("A")

  d1 <- datastep(dat, arrays = list(dsa = dsarray("vs", "am", "gear", "cat")), {

    temp <- dsa[4]
    temp2 <- dsa["cat"]

  })

  expect_equal("temp" %in% names(d1), TRUE)
  expect_equal(d1[["temp"]], as.character(dat[["cat"]]))

  expect_equal("temp2" %in% names(d1), TRUE)
  expect_equal(d1[["temp2"]], as.character(dat[["cat"]]))

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

  if (DEV) {

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
  
  } else 
    expect_equal(TRUE, TRUE)

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


  if (DEV) {

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
    
  } else {
    expect_equal(TRUE, TRUE) 
  }
})



test_that("for loop and data type check works as expected.", {

  if (DEV) {
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

  } else {
    expect_equal(TRUE, TRUE) 
  }
})


test_that("System test of datastep array.", {

  dfin <- read.table(header = TRUE, text = '
   C1 C2 C3 C4 C5 C6 C7
   12 R11 D21 201901 09 D89 Real
   21 R23 D77 201901 21 D77 Fetched
   33 R43 D87 201901 31 D87 Real
   33 R43 D87 201901 31 D87 Fetched
   57 R12 D87 201901 12 D87 Fetched')


  dfout <- datastep(dfin,
                    arrays = list(dsa = dsarray(names(dfin[1:6]))),
                    drop = c("nm"),
                    {


                      # After the first row
                      if (n. > 1) {

                        # Loop through column array
                        for (nm in dsa) {

                          # If any of the first 6 columns don't match
                          # or C7 is equal to Real, keep the row
                          #print(paste0("DSA", n., ":", dsa[nm]))
                          #print(paste0("data", n., ":", data[[n. - 1, nm]]))
                          if (as.character(data[[n., nm]]) != as.character(data[[n. - 1, nm]]) ||
                              C7 == "Real") {
                            delete <- FALSE
                            break
                          } else {

                            delete <- TRUE

                          }

                        }

                      } else {

                        # Keep first row by default
                        delete <- FALSE
                      }

                    })

  # See results of datastep
  dfout
  #   C1  C2  C3     C4 C5  C6      C7 delete
  # 1 12 R11 D21 201901  9 D89    Real  FALSE
  # 2 21 R23 D77 201901 21 D77 Fetched  FALSE
  # 3 33 R43 D87 201901 31 D87    Real  FALSE
  # 4 33 R43 D87 201901 31 D87 Fetched   TRUE
  # 5 57 R12 D87 201901 12 D87 Fetched  FALSE

  #print(dfout)

  expect_equal(nrow(dfout), 5)

  # Filter out rows flagged for deletion
  res <- dfout[dfout$delete == FALSE, names(dfout)[1:7]]
  #print(res)
  res
  #   C1  C2  C3     C4 C5  C6      C7
  # 1 12 R11 D21 201901  9 D89    Real
  # 2 21 R23 D77 201901 21 D77 Fetched
  # 3 33 R43 D87 201901 31 D87    Real
  # 5 57 R12 D87 201901 12 D87 Fetched

  expect_equal(nrow(res), 4)
})


test_that("System test of datastep array.", {

  df <- read.table(header = TRUE, text = '
      C1    C2
       3    A1
       2    A2
       1    A3
  ')


  dt <- datastep(df,
                 arrays = list(arr1 = dsarray("C1", "C2")),
                 {

                    D1 <- arr1["C1"]
                    D2 <- arr1["C2"]

                 })
  #print(dt)
  expect_equal(dt$D1, c(3, 2, 1))
  #print(dt$D2)
  expect_equal(dt$D2, c("A1", "A2", "A3"))

})
