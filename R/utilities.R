
# Utility Functions -------------------------------------------------------


#' @title Print a data library
#' @description A class-specific instance of the \code{print} function for 
#' data libraries.  The function prints the library in a summary manner.  
#' Use \code{verbose = TRUE} to print the library as a list.
#' @param x The library to print.
#' @param ... Any follow-on parameters.
#' @param verbose Whether or not to print the library in verbose style.
#' By default, the parameter is FALSE, meaning to print in summary style.
#' @return The object, invisibly.
#' @family lib
#' @examples 
#' # Create temp directory
#' tmp <- tempdir()
#' 
#' # Save some data to temp directory
#' saveRDS(iris, file.path(tmp, "iris.rds"))
#' saveRDS(ToothGrowth, file.path(tmp, "ToothGrowth.rds"))
#' saveRDS(PlantGrowth, file.path(tmp, "PlantGrowth.rds"))
#' 
#' # Create data library
#' libname(dat, tmp)
#' 
#' # Print library summary 
#' print(dat)
#' # library 'dat': 3 items
#' # - attributes: not loaded
#' # - path: C:\Users\User\AppData\Local\Temp\RtmpCSJ6Gc
#' # - items:
#' #          Name Extension Rows Cols   Size        LastModified
#' # 1        iris       rds  150    5 7.8 Kb 2020-11-05 22:26:59
#' # 2 PlantGrowth       rds   30    2 2.5 Kb 2020-11-05 22:26:59
#' # 3 ToothGrowth       rds   60    3 3.4 Kb 2020-11-05 22:26:59
#' 
#' # Clean up
#' lib_delete(dat)
#' @import crayon
#' @export
print.lib <- function(x, ..., verbose = FALSE) {
  
  if (verbose == TRUE) {
    
    print(unclass(x))  
    
  } else {
    
    grey60 <- make_style(grey60 = "#999999")
    
    cat(grey60(paste0("# library '",  attr(x, "name"), "': ", length(x), 
                      " items\n")))
    
    at <- paste("- attributes:")
    if (!is.null(attr(x, "type")))
      at <- paste(at, attr(x, "type"))
    if (attr(x, "read_only"))
      at <- paste(at, "read_only")
    if (attr(x, "loaded"))
      at <- paste(at, "loaded\n")
    else 
      at <- paste(at, "not loaded\n")
    
    cat(at)
    cat(paste0("- path: ", attr(x, "path"), "\n"))
    
    if (length(x) > 0)
      cat("- items:\n")
    
    dat <- lib_info(x)
    
    print(dat)
    
  }
  
  invisible(x)
}


#' @noRd
is.loaded.lib <- function(name) {
  
  lb <- get(name, envir = e$env)
  
  ret <- attr(lb, "loaded")
  
  return(ret)
}


#' @title Class test for a data library
#' @description This function tests whether an object is a data library.  The
#' data library has a class of "lib".  
#' @param x The object to test.
#' @return TRUE or FALSE, depending on whether or not the object is a 
#' data library.
#' @family lib
#' @examples 
#' # Create format catalog
#' libname(dat, tempdir()) 
#'            
#' # Test for "lib" class
#' is.lib(dat) 
#' # [1] TRUE
#' 
#' is.lib(list())
#' # [1] FALSE
#' @export
is.lib <- function(x) {
  
  ret <- FALSE
  if (any(class(x) == "lib"))
    ret <-  TRUE
  
  return(ret)
}


#' @noRd
getExtension <- function(file){ 
  ex <- strsplit(basename(file), split="\\.")[[1]]
  return(ex[-1])
} 

#' @noRd
getFileName <- function(file){ 
  ex <- strsplit(basename(file), split="\\.")[[1]]
  return(ex[1])
}

#' @noRd
getUniqueName <- function(nm, nms) {
  
  ret <- nm
  pos <- match(nm, nms)
  if (!is.na(pos)) {
    
    ex <- strsplit(basename(nm), split="\\_")[[1]]
    #print(ex)
    num <- as.numeric(ex[-1])
    #print(num)
    if (length(num) == 0 )
      ret <- paste0(nm, "_1")
    else if (is.na(num))
      ret <- paste0(nm, "_1")
    else {
      tmp <- sub(paste0("_", num), "", nm)
      ret <- paste0(tmp, "_", num + 1)
    }
    
  }
  
  return(ret)
}


# if (ext == "csv") {
#   
#   dat <- read_csv(fp, ...)
#   
# } else if (ext == "rds") {
#   
#   dat <- read_rds(fp, ...)
#   
# } else if (ext == "sas7bdat") {
#   
#   dat <- read_sas(fp, ...)
#   
# } else if (ext == "xlsx") {
#   
#   dat <- read_xlsx(fp, ...)
#   
# } else if (ext == "xls") {
#   
#   dat <- read_xls(fp, ...)
#   
# } 

#' @noRd
writeData <- function(x, ext, file_path) {
  
  
  if (ext == "csv") {
    
    if (file.exists(file_path)) {
      dat <- read_csv(file_path)
      if (!comp(x, dat)) {
        file.remove(file_path)
        write_csv(x, file_path)
      }
      
    } else
      write_csv(x, file_path)
    
  } else if (ext == "rds") {
    
    if (file.exists(file_path)) {
      dat <- read_rds(file_path)
      if (!comp(x, dat)) {
        file.remove(file_path)
        write_rds(x, file_path)
      }
    } else
      write_rds(x, file_path)
    
  } else if (ext == "sas7bdat") {
    
    if (file.exists(file_path)) {
      dat <- read_sas(file_path)
      if (!comp(x, dat)) {
        file.remove(file_path)
        write_sas(x, file_path)
      }
    } else
      write_sas(x, file_path)
    
  } else if (ext == "xlsx") {
    
    if (file.exists(file_path)) {
      dat <- read_xlsx(file_path, )
      if (!comp(x, dat)) {
        file.remove(file_path)
        openxlsx::write.xlsx(x, file_path)
      }
    } else
      openxlsx::write.xlsx(x, file_path)
    
  } else if (ext == "xls") {
    
    message(paste("NOTE: Libr cannot write xls files. Writing xlsx instead."))
    
    if (file.exists(file_path)) {
      dat <- read_xlsx(file_path)
      if (!comp(x, dat)) {
        file.remove(file_path)
        openxlsx::write.xlsx(x, file_path)
      }
    } else
      openxlsx::write.xlsx(x, file_path)
  }  
  
  
}

#' @noRd
comp <- function(x1, x2) {
  
 ret <- TRUE  
 if (is.null(x1) | is.null(x2)) {
   ret <- FALSE
 } else if (all(class(x1) != class(x2))) {
   ret <- FALSE 
 } else if ("data.frame" %in% class(x1)) {
   
   if (nrow(x1) != nrow(x2)) {
     ret <- FALSE
   } else if (ncol(x1) != ncol(x2)) {
     ret <- FALSE
   } else if (all(names(x1) != names(x2))) {
     ret <- FALSE 
   } else {
     
     for (i in seq_along(x1)) {
       if (any(!strong_eq(x1[[i]], x2[[i]]))) {
         ret <- FALSE
         break
       }
     }
   }
 } else {
  
   if (length(x1) != length(x2)) {
     ret <- FALSE
   } else {
     
     if (any(!strong_eq(x1, x2))) {
       ret <- FALSE
     }
   }
 }
  
 return(ret)
}

#' @noRd
strong_eq <- Vectorize(function(x1, x2) {
  
  ret <- TRUE
  if (is.null(x1) & is.null(x2))
    ret <- TRUE
  else if (is.null(x1) & !is.null(x2))
    ret <- FALSE
  else if (!is.null(x1) & is.null(x2))
    ret <- FALSE
  else if (is.na(x1) & is.na(x2))
    ret <- TRUE
  else if (is.na(x1) & !is.na(x2))
    ret <- FALSE
  else if (!is.na(x1) & is.na(x2))
    ret <- FALSE
  else {
    ret <- x1 == x2
    
  }
  
  return(ret)
  
})
