



#' @noRd
ds_experiment <- function() {
  
  
  dat <- mtcars
  
  cnt <- 0
  
  code <- expression(fork <- mpg / cyl,
                     cnt <- cnt + 1)
  
  
  within(dat, eval(code), keepAttrs = TRUE)
  
  
  
  
  
  
  
  l <- 100000
  
  df <- tibble(C1 = seq_len(l), C2 = runif(l), 
               C3 = runif(l), C4 = runif(l))
  
  tm <- Sys.time()
  
  env <- new.env()
  
  env$n. <- 1
  # 
  # res <- datastep(df, attrib = list(C5 = 0, C6 = 0),
  #                 {
  #                   C5 <- C2 + C3 + C4
  #                   C6 <- max(C2, C3, C4)
  #                   n. <- n. + 1
  #                 })
  
  n. <- 0
  
  exp <- expression(C5 <- C2 + C3 + C4, 
                    C6 <- max(C2, C3, C4),

                    cnt <- n.)
  
  
  d2 <- within(df, eval(exp))
  
  d2
  
  d3 <- within(df, {C5 <- C2 + C3 + C4 
                    C6 <- max(C2, C3, C4)
                    assign("n.", get("n.", envir = parent.frame(3)) + 1, envir = parent.frame(3))
                    cnt <- get("n.", envir = parent.frame(3))})
  
  d3
  

  
  tmdiff <- Sys.time() - tm
  tmdiff
  
  
  mydat <- data.frame(A = c(1, 2, 3, 4), 
                      B = c(5, 6, 7, 8))
  
  
  myfunc <- function(dat) {
    
    retainedvar <- 1
    
    innerfunc <- Vectorize(function(dat){
      
      retainedvar <<- retainedvar + 1
    }
    )
    
  }
  
  # Base R
  lapply()
  mapply()
  sapply()
  vapply()
  
  # fmtr package
  fapply()
  
  subset(mtcars, mpg > 25)
  
  mtcars[mtcars$mpg > 25, ] 
  
}
