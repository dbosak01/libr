
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


#' @import tools
#' @noRd
writeData <- function(x, ext, file_path, force = FALSE) {
  
  # Get checksum on file
  if (file.exists(file_path))
    cs1 <- md5sum(file_path)
  else 
    cs1 <- "a"
  
  # Get checksum on library data
  cs2 <- attr(x, "checksum")
  if (is.null(cs2))
    cs2 <- "b"
  
  # Compare checksums
  cs_comp <- cs1 == cs2
  
  if (ext == "csv") {
  
    if (!cs_comp | force) {
      if (file.exists(file_path))
        file.remove(file_path)
      write_csv(x, file_path)
    }
    
  } else if (ext == "rds") {
    
    if (!cs_comp | force) {
      if (file.exists(file_path))
        file.remove(file_path)
      write_rds(x, file_path)
    }

  } else if (ext == "sas7bdat") {
    
    if (!cs_comp | force) {
      if (file.exists(file_path))
        file.remove(file_path)
      write_sas(x, file_path)
    }
    
  } else if (ext == "xpt") {
    
    if (!cs_comp | force) {
      if (file.exists(file_path))
        file.remove(file_path)
      write_xpt(x, file_path)
    }
    
  }else if (ext == "xlsx") {
  
    if (!cs_comp | force) {
      if (file.exists(file_path))
        file.remove(file_path)
      openxlsx::write.xlsx(x, file_path)
    }

  } else if (ext == "xls") {
    
    message(paste("NOTE: Libr cannot write xls files. Writing xlsx instead."))
    
    if (!cs_comp | force) {
      if (file.exists(file_path))
        file.remove(file_path)
      openxlsx::write.xlsx(x, file_path)
    }

  }  
  
  
}

#' @noRd
spec_trans <- c("guess" = "guess",
                "logical" = "logical",
                "character" = "text",
                "numeric" = "numeric", 
                "integer" = "numeric")
              
#' @noRd
get_colspec_xlsx <- function(type_string, num_cols, col_names) {
  
 ret <- NULL  

   
 if (length(col_names) != num_cols)
   stop("Column names and length are not equal")
 
 ret <- rep("guess", num_cols)
 names(ret) <- col_names
 
 for (nm in col_names) {
   if (nm %in% names(type_string)) {
     
     ct <- type_string[[nm]]
     
     if (ct %in% names(spec_trans))
       ret[[nm]] <- spec_trans[[ct]]
     else {
       ret[[nm]] <- "date"
     }
   }
 }
 
  
 return(ret)
  
}


#' @import readr                
#' @noRd
get_colspec_csv <- function(type_string) {
  

  ret <- cols()
  for (nm in names(type_string)) {
    if (type_string[[nm]] == "logical")
      ret$cols[[nm]] <- col_logical()
    else if (type_string[[nm]] == "character")
      ret$cols[[nm]] <- col_character()
    else if (type_string[[nm]] == "integer")
      ret$cols[[nm]] <- col_integer()
    else if (type_string[[nm]] == "numeric")
      ret$cols[[nm]] <- col_double()
    else if (type_string[[nm]] == "guess")
      ret$cols[[nm]] <- col_guess()
    else {
      fp <- trimws(unlist(strsplit(type_string[[nm]], "=", fixed = TRUE)))
      if (fp[[1]] == "date")
        ret$cols[[nm]] <- col_date(fp[[2]])
      else if (fp[[1]] == "time")
        ret$cols[[nm]] <- col_time(fp[[2]])
      else if (fp[[1]] == "datetime")
        ret$cols[[nm]] <- col_datetime(fp[[2]])
      else 
        stop(paste0("Column type not valid: ", fp[[1]]))
      
    }
    
  }
    

  return(ret)
  
}


#' @title Check identity of two objects
#' @description The \code{\%eq\%} function provides a more human-comprehensible
#' style of equality check.  The function allows comparing of nulls, NA values,
#' and atomic type values without error. The function also allows comparing
#' of data frames.  The function will return TRUE if all values in the 
#' data frames are equal, and ignores differences in attributes.
#' @param x1 The first object to compare
#' @param x2 The second object to compare
#' @return A TRUE or FALSE value depending on whether the objects are equal.
#' @export
`%eq%` <- function(x1, x2) {

 ret <- TRUE
 if (is.null(x1) & is.null(x2))
   ret <- TRUE
 else if (is.null(x1) | is.null(x2)) {
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

# get_id <- function(n = 1, seed_no = 1, id_len = 5){
#   set.seed(seed_no)
#   pool <- c(letters, LETTERS, 0:9)
#   
#   ret <- character(n) 
#   for(i in seq(n)){
#     this_res <- paste0(sample(pool, id_len, replace = TRUE), collapse = "")
#     while(this_res %in% ret){ 
#       this_res <- paste0(sample(pool, id_len, replace = TRUE), collapse = "")
#     }
#     ret[i] <- this_res
#   }
#   return(ret)
# }
# 
# get_id()
