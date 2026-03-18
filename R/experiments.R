#' 
#' 
#' 
#' 
#' #' @noRd
#' ds_experiment <- function() {
#'   
#'   
#'   lbdat <- dat$lb
#'   
#'   rws2 <- split(lbdat, seq(nrow(lbdat)))
#'   
#'   
#'   
#'   dt <- mtcars
#'   dt$color <- "Red"
#'   
#'   library(common)
#'   
#'   labels(dt) <- list(mpg = "Mile Per Gallon")
#'   
#'   labels(dt)
#'   
#'   cnt <- 0
#'   
#'   code <- expression(fork <- mpg / cyl,
#'                      cnt <- cnt + 1)
#'   
#'   
#'   code2 <- expression(cnt <- cnt + 1)
#'   
#'   
#'   rws <- split(dt, seq(nrow(dt)))
#'   
#'   labels(rws[[2]])
#'   class(rws[[1]][["mpg"]])
#'   
#'   prrow <- NULL
#'   n. <- 1
#'   
#'   e <- new.env()
#'   e$data <- rws
#'   
#'   labels(dt)
#'   
#'   att <- attributes(dt$mpg)
#'   
#' 
#'   process_dstep <- function(rw, cd, retlst) {
#' 
#'     # Get current row
#'    # rw <- data[[n.]]
#' 
#'     # Deal with retained values
#'     if (length(retlst) > 0) {
#' 
#'       if (n. == 1) {
#'         prw <- retlst
#'       } else {
#'         prw <- prrow
#'       }
#' 
#' 
#'       for (nm in names(retlst)) {
#'         rw[[nm]] <- prw[[nm]]
#'       }
#'     }
#' 
#'     ret <- within(rw, eval(cd))
#' 
#'     prrow <<- ret
#'     n. <<- n. + 1
#' 
#'     return(ret)
#'   }
#'   
#'   retain <- list(cnt = 0)
#' 
#'   tm <- Sys.time()
#'   res <- mapply(rws, FUN = process_dstep, 
#'                 MoreArgs = list(cd = code, retlst = retain), 
#'                 SIMPLIFY = FALSE)
#'   
#'   print(Sys.time() - tm)
#'   e$data
#'   
#'   
#'   myres <- as.data.frame(t(res))
#' 
#'   class(myres)
#'   
#'   myfunc <- function(dt, cd) {
#'     
#'     rwnm <- 1
#'     
#'     innerfunc <- function(dt) {
#'       
#'       rwnm <<- rwnm + 1
#'       print(rwnm)
#'       dt[rwnm, "rwnm"] <- rwnm
#'     }
#'     
#'     mapply(dat, FUN = innerfunc)
#'     
#'     
#'   }
#'   
#'   myfunc(dat, code)
#'   
#'   
#'   
#'   l <- 100000
#'   
#'   df <- tibble(C1 = seq_len(l), C2 = runif(l), 
#'                C3 = runif(l), C4 = runif(l))
#'   
#'   tm <- Sys.time()
#'   
#'   env <- new.env()
#'   
#'   env$n. <- 1
#'   # 
#'   # res <- datastep(df, attrib = list(C5 = 0, C6 = 0),
#'   #                 {
#'   #                   C5 <- C2 + C3 + C4
#'   #                   C6 <- max(C2, C3, C4)
#'   #                   n. <- n. + 1
#'   #                 })
#'   
#'   n. <- 0
#'   
#'   exp <- expression(C5 <- C2 + C3 + C4, 
#'                     C6 <- max(C2, C3, C4),
#' 
#'                     cnt <- n.)
#'   
#'   
#'   d2 <- within(df, eval(exp))
#'   
#'   d2
#'   
#'   d3 <- within(df, {C5 <- C2 + C3 + C4 
#'                     C6 <- max(C2, C3, C4)
#'                     assign("n.", get("n.", envir = parent.frame(3)) + 1, envir = parent.frame(3))
#'                     cnt <- get("n.", envir = parent.frame(3))})
#'   
#'   d3
#'   
#' 
#'   
#'   tmdiff <- Sys.time() - tm
#'   tmdiff
#'   
#'   
#'   mydat <- data.frame(A = c(1, 2, 3, 4), 
#'                       B = c(5, 6, 7, 8))
#'   
#'   
#'   myfunc <- function(dat) {
#'     
#'     retainedvar <- 1
#'     
#'     innerfunc <- Vectorize(function(dat){
#'       
#'       retainedvar <<- retainedvar + 1
#'     }
#'     )
#'     
#'   }
#'   
#'   # Base R
#'   lapply()
#'   mapply()
#'   sapply()
#'   vapply()
#'   
#'   # fmtr package
#'   fapply()
#'   
#'   subset(mtcars, mpg > 25)
#'   
#'   mtcars[mtcars$mpg > 25, ] 
#'   
#' }
