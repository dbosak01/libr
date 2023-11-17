
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
#' # Create data library
#' libname(dat, tmp)
#' 
#' # Add data to library
#' lib_add(dat, iris, ToothGrowth, PlantGrowth)
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
    
    # Print list form
    print(unclass(x))  
    
  } else {
    
    # Prepare color
    grey60 <- make_style(grey60 = "#999999")
    
    # Print a nice header
    cat(grey60(paste0("# library '",  attr(x, "name"), "': ", length(x), 
                      " items\n")))
    
    at <- paste("- attributes:")
    if (!is.null(attr(x, "engine")))
      at <- paste(at, attr(x, "engine"))
    if (attr(x, "read_only"))
      at <- paste(at, "read_only")
    if (attr(x, "loaded"))
      at <- paste(at, "loaded\n")
    else 
      at <- paste(at, "not loaded\n")
    
    cat(at)
    cat(paste0("- path: ", attr(x, "path"), "\n"))
    
    if (!is.null(attr(x, "where"))) {
      
       cat(paste0("- where: ",  attr(x, "where"), "\n"))
    }
    
    if (length(x) > 0)
      cat("- items:\n")
    
    # Reuse the info function
    # Already has everything we need
    dat <- lib_info(x)
    
    print(dat)
    
  }
  
  invisible(x)
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
#' 
#' # Clean up
#' lib_delete(dat)
#' @export
is.lib <- function(x) {
  
  ret <- FALSE
  if (any(class(x) == "lib"))
    ret <-  TRUE
  
  return(ret)
}



# Internal Utilities ------------------------------------------------------


#' @description Internal function to see if a lib is loaded
#' @noRd
is.loaded.lib <- function(name) {
  
  lb <- get(name, envir = e$env)
  
  ret <- attr(lb, "loaded")
  
  return(ret)
}

#' @description Get the extension of a file
#' @noRd
getExtension <- function(file){ 
  ex <- strsplit(basename(file), split="\\.")[[1]]
  return(ex[-1])
} 

#' @description Get the name of a file without extension
#' @noRd
getFileName <- function(file){ 
  ex <- strsplit(basename(file), split="\\.")[[1]]
  return(ex[1])
}

# @description Had an idea to add a unique name if user tries to put a 
# duplicate name.  Idea never came to fruition.  Keeping code here anyway.
# @noRd
# getUniqueName <- function(nm, nms) {
#   
#   ret <- nm
#   pos <- match(nm, nms)
#   if (!is.na(pos)) {
#     
#     ex <- strsplit(basename(nm), split="\\_")[[1]]
#     #print(ex)
#     num <- as.numeric(ex[-1])
#     #print(num)
#     if (length(num) == 0 )
#       ret <- paste0(nm, "_1")
#     else if (is.na(num))
#       ret <- paste0(nm, "_1")
#     else {
#       tmp <- sub(paste0("_", num), "", nm)
#       ret <- paste0(tmp, "_", num + 1)
#     }
#     
#   }
#   
#   return(ret)
# }

#' @description Write data to the file system.  This will compare the stored
#' checksums with the current checksum and only update if the file has
#' changed.  That way any timestamps on the files are preserved. 
#' @import tools
#' @import haven
#' @import readr
#' @import openxlsx
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
      write_csv(x, file_path, na = "")
      attr(x, "checksum") <- md5sum(file_path)
    }
    
  } else if (ext == "rds") {
    
    if (!cs_comp | force) {
      if (file.exists(file_path))
        file.remove(file_path)
      write_rds(x, file_path)
      attr(x, "checksum") <- md5sum(file_path)
    }
    
  } else if (tolower(ext) %in% c("rdata", "rda")) {
    
    if (!cs_comp | force) {
      if (file.exists(file_path))
        file.remove(file_path)
      save(x, file = file_path)
      attr(x, "checksum") <- md5sum(file_path)
    }

  } else if (ext == "sas7bdat") {
    
    # if (!cs_comp | force) {
    #   if (file.exists(file_path))
    #     file.remove(file_path)
    #   write_sas(x, file_path)
    #   attr(x, "checksum") <- md5sum(file_path)
    # }
    
  } else if (ext == "dbf") {
    
    if (!cs_comp | force) {
      if (file.exists(file_path))
        file.remove(file_path)
      foreign::write.dbf(as.data.frame(x, stringsAsFactors = FALSE), file_path)
      attr(x, "checksum") <- md5sum(file_path)
    }
    
  } else if (ext == "xpt") {
    
    if (!cs_comp | force) {
      if (file.exists(file_path))
        file.remove(file_path)
      write_xpt(x, file_path)
      attr(x, "checksum") <- md5sum(file_path)
    }
    
  } else if (ext == "xlsx") {
  
    if (!cs_comp | force) {
      if (file.exists(file_path))
        file.remove(file_path)
      openxlsx::write.xlsx(x, file_path)
      attr(x, "checksum") <- md5sum(file_path)
    }

  } else if (ext == "xls") {
    
    message(paste("NOTE: Libr cannot write xls files. Writing xlsx instead."))
    
    if (!cs_comp | force) {
      fp <- sub(".xls", ".xlsx", file_path, fixed = TRUE)
      if (file.exists(fp))
        file.remove(fp)
      openxlsx::write.xlsx(x, fp)
      
      attr(x, "extension") <- "xlsx"
      attr(x, "path") <- fp
      attr(x, "checksum") <- md5sum(fp)
    }

  }  
  
  
  return(x)
}

#' @description Mapping of column types
#' @noRd
spec_trans <- c("guess" = "guess",
                "logical" = "logical",
                "character" = "text",
                "numeric" = "numeric", 
                "integer" = "numeric")

#' @description This function maps the colspec interface to the colspecs
#' needed for the excel import function.              
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

#' @description This function maps the colspec interface to the colspecs
#' needed for the csv import function. 
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

#' @description Apply import specs to any data frame
#' @noRd
exec_spec <- function(x, spcs, nm) {

 ret <- x
 
 if (!is.null(spcs)) {  
   
   colspcs <- list()
   naval <- spcs$na
   tws <- spcs$trim_ws
   
   if (!is.null(spcs$specs[[nm]])) {
     tspec <- spcs$specs[[nm]]
     if (!is.null(tspec$na))
       naval <- tspec$na
     if (!is.null(tspec$trim_ws))
       tws <- tspec$trim_ws
     if (!is.null(tspec$col_types))
       colspcs <- tspec$col_types
     
   }
   
   for (nm in names(ret)) {
     
     # Apply trimws if requested on character columns
     if ("character" %in% class(ret[[nm]]))
       ret[[nm]] <- trimws(ret[[nm]])
     
     # Apply na conversion on requested values
     if (length(colspcs) == 0 | is.null(colspcs[[nm]]))
       ret[[nm]] <- ifelse(ret[[nm]] %in% naval, NA, ret[[nm]])
     
     if (length(colspcs) > 0) {
       if (!is.null(colspcs[[nm]])) {
        
         if (colspcs[[nm]] != "guess") {
           if (colspcs[[nm]] == "integer") {
             ret[[nm]] <- ifelse(ret[[nm]] %in% naval, as.integer(NA), ret[[nm]])
             ret[[nm]] <- as.integer(ret[[nm]])
           } else if (colspcs[[nm]] == "numeric") {
             ret[[nm]] <- ifelse(ret[[nm]] %in% naval, as.numeric(NA), ret[[nm]])
             ret[[nm]] <- as.numeric(ret[[nm]])
           } else if (colspcs[[nm]] == "character") {
             ret[[nm]] <- ifelse(ret[[nm]] %in% naval, as.character(NA), ret[[nm]])
             ret[[nm]] <- as.character(ret[[nm]])
           } else if (colspcs[[nm]] == "logical") {
             ret[[nm]] <- ifelse(ret[[nm]] %in% naval, as.logical(NA), ret[[nm]])
             ret[[nm]] <- as.logical(ret[[nm]])
           } else {

             spl <- trimws(unlist(strsplit(colspcs[[nm]], "=", fixed = TRUE)))

             if (length(spl) > 1) {
               if (spl[1] == "date") {
                 ret[[nm]] <- ifelse(ret[[nm]] %in% naval, as.Date(NA), ret[[nm]])
                 ret[[nm]] <- as.Date(ret[[nm]], spl[2])
               } else if (spl[1] == "datetime") {
                 ret[[nm]] <- ifelse(ret[[nm]] %in% naval, as.POSIXct(NA), ret[[nm]])
                 ret[[nm]] <- as.POSIXct(ret[[nm]], format = spl[2])
               } else if (spl[1] == "time") { 
                 ret[[nm]] <- ifelse(ret[[nm]] %in% naval, as.POSIXct(NA), ret[[nm]])
                 ret[[nm]] <- as.POSIXct(ret[[nm]], format = spl[2])
               }
             }
           }
         }
       }
     }
     
     for (atnm in names(attributes(x[[nm]]))) {
       if (atnm != "class")
        attr(ret[[nm]], atnm) <- attr(x[[nm]], atnm)
      
     }
   }
 }
 
 return(ret)
}



dofilter <- function(str, vect, extension = NULL) {
  
  # Remove base path and file extension
  if (is.null(extension))
    lst <- vect
  else 
    lst <- gsub(paste0(".", extension), "", basename(vect))
  
  # Generate regular expressions
  flt <-  paste0("^", gsub("*", ".*", str, fixed = TRUE), "$")
  
  # Create return list
  ret <- c()
  
  # Perform filter 
  for (f in flt) {
    tmp <- vect[grepl(f, lst, ignore.case = TRUE, useBytes = TRUE)]
    #tmp <- vect[grepl(f, lst)]
    
    # Add results to return vector
    if (length(tmp) > 0) {
      for (t in tmp)
        ret[length(ret) + 1] <- t
    }
  }
  
  # Dedupe
  ret <- unique(ret)
  
  return(ret)
  
}


#' @noRd
log_logr <- function(x) {
  
  if (length(find.package('logr', quiet=TRUE)) > 0) {
    if (utils::packageVersion("logr") >= "1.2.0") {
      logr::log_hook(x)
    }
  }
}



# Check if logr.output option is set or not
log_output <- function() {
  
  ret <- TRUE
  tmp <- options("logr.output")
  if (!is.null(tmp$logr.output)) {
    
    ret <- tmp$logr.output
    
  }
  
  return(ret)
}



# @noRd
# standard_eval <- function() {
#   
#   ret <- FALSE
#   
#   if (is.null(options("libr.standard_eval")[[1]]) == FALSE) {
#     if (options("libr.standard_eval")[[1]] == TRUE)
#       ret <- TRUE
#     
#   }
#   
#   if (is.null(options("sassy.standard_eval")[[1]]) == FALSE) {
#     if (options("sassy.standard_eval")[[1]] == TRUE)
#       ret <- TRUE
#     
#   }
#   
#   return(ret)
# }


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
