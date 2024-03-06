library(fmtr)

myfmt <- value(condition(x >= 20, "High"),
               condition(x < 20, "Low"))


d1 <- datastep(mtcars, {
  
  mpgcat <- fapply(mpg, myfmt)
  
})


d1


if (!"mpgcat" %in% names(d1))
  stop("Mpgcat missing")


if (d1$mpgcat[1] != "High")
  stop("Mpgcat value not High")
