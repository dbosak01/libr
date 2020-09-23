
# Libname Definition -------------------------------------------


#' @title Create a data library
#' @description A data library is a collection of data frames  A data 
#' library allows you to manage and store data frames as a unit.  The 
#' \code{libname} function defines the library.
#' @details A data library is an S3 object of class "lib".  The purpose of 
#' the library is to combine related data frames, and allow you to manipulate all
#' of them as a single object.  
#' @param directory_path A directory path in which the data resides.  The incoming 
#' data can be in any file format: rds, csv, sas7bdat, etc.  The 
#' \code{libname} function will read any type of data into the library, 
#' and render as an R data frame.  
#' @param filter One or more file extensions to filter the incoming data.  The
#' default value is NULL, meaning all recognized data files will be input.
#' Multiple filters can be passed as a vector of file extensions. 
#' Valid values are "rds", "sas7bdat", "xls", "xlsx", "csv", and "dat". 
#' @return The library object.
#' @family lib
#' @examples 
#' # Create temp directory
#' tmp <- tempdir()
#' 
#' # Save some data to temp directory
#' saveRDS(trees, file.path(tmp, "trees.rds"))
#' saveRDS(lynx, file.path(tmp, "lynx.rds"))
#' saveRDS(beaver1, file.path(tmp, "beaver1.rds"))
#' 
#' # Create format catalog
#' lib <- libname(tmp)
#' 
#' # Print library summary 
#' print(lib)
#' @import readxl
#' @import haven
#' @export
libname <- function(directory_path, filter = NULL, ...) {
  
  # Create new structure of class "lib"
  l <- structure(list(), class = c("lib", "list"))
  
  attr(l, "path") <- directory_path
  attr(l, "filter") <- filter
  
  lst <- list.files(directory_path)
  for (fl in lst) {
    fp <- file.path(directory_path, fl)
    ext <-  getExtension(fl)
    nm <- getFileName(fl)
    #nm <- getUniqueName(nm, names(l))
    
    #print(ext)
    if (length(ext) > 0) { 
      if (ext == "csv") {
        
        dat <- read.csv(fp, ...)
        
      } else if (ext == "rds") {
        
        dat <- readRDS(fp, ...)
        
      } else if (ext == "sas7bdat") {
        
        dat <- read_sas(fp, ...)
        
      } else if (ext == "xlsx") {
        
        dat <- read_xlsx(fp, ...)
        
      } else if (ext == "xls") {
        
        dat <- read_xls(fp, ...)
        
      }  else if (ext == "dat"){
        
        dat <- read.Table(fp, ...)
      }
      
      attr(dat, "name") <- nm
      attr(dat, "extension") <- ext
      attr(dat, "path") <- directory_path
      l[[nm]] <- dat
    }

  }
  
  return(l)
  
}




# Conversion Functions -----------------------------------------------------

#' @title Generic Casting Method for Data Libraries
#' @description A generic method for casting objects to
#' a data library.  Individual objects will inherit from this function.
#' @param x The object to cast.
#' @return A library object, created using the information in the 
#' input object.
#' @seealso For class-specific methods, see 
#' \code{\link{as.lib.list}}.
#' @family lib
#' @export
as.lib <- function (x) {
  UseMethod("as.lib", x)
}

#' @title Convert a List of Data Frames to a Data Library
#' @family lib
#' @export
as.lib.list <- function(x, path) {
  
  
  class(x) <- list("lib", class(x))
  
  return(x)
  
}


# Manipulation Functions --------------------------------------------------


#' @title Write a data library to the file system
#' @description The \code{lib.write} function writes the data library
#' to the file system.  By default, the library will be written to the 
#' directory for which it was defined, and each data frame will be written
#' in the format from which it was read.  Data frames that were not read
#' from a file will be saved in RDS format, unless otherwise specified.
#' @param x The format catalog to write.
#' @param dir_path The directory path to write the catalog to. Default is the 
#' current working directory.
#' @param file_name The name of the file to save the catalog as.  Default is
#' the name of the variable that contains the catalog.  The ".fcat" file
#' extension will be added automatically.
#' @param type The type of data library. Default is NULL, meaning the library
#' is not typed.  Valid values are "rds", "sas7bdat", "xls", "xlsx", and "csv". 
#' The data files will be saved as the type specified on this parameter, 
#' no matter from which type of file they were input.
#' @return The full path of the saved format catalog.
#' @family lib
#' @export
lib.write <- function(x, dir_path = getwd(), file_name = deparse(substitute(x, 
                                                     env = environment())),
                      type = NULL) {
  pth <- file.path(dir_path, paste0(file_name, ".fcat"))
  
  
  if (file.exists(pth))
    file.remove(pth)
  
  saveRDS(x, pth)
  
  return(pth)
}


#' @title Copy a Data Library
#' @family lib
#' @export
lib.copy <- function(x) {
  
  
}

#' @title Remove Data from a Data Library
#' @family lib
#' @export
lib.remove <- function(x) {
  
}

#' @title Create a New Data Library Directory
#' @family lib
#' @export
lib.create <- function(x) {
  
  
}

#' @title Get Information on a Data Library
#' @family lib
#' @export
lib.info <- function(x) {
  
}

#' @title Append Data to a Data Library
#' @family lib
#' @export
lib.append <- function(x) {
  
  
}

#' @title Delete a Data Library
#' @family lib
#' @export
lib.delete <- function(x) {
  
  
  
}

#' @title Get the Path for a Data Library
#' @family lib
#' @export
lib.path <- function(x) {
  
}

#' @title Get the Size of a Data Library
#' @family lib
#' @export
lib.size <- function(x) {
  
  
}


# Utility Functions -------------------------------------------------------


#' @title Print a data library
#' @description A class-specific instance of the \code{print} function for 
#' data libraries.  The function prints the library in a summary manner.  
#' Use \code{verbose = TRUE} to print the catalog as a list.
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
#' # Create format catalog
#' lib <- libname(tmp)
#' 
#' # Print library summary 
#' print(lib)
#' @export
print.lib <- function(x, ..., verbose = FALSE) {
  
  if (verbose == TRUE) {
    
    print(unclass(x))  
    
  } else {
    
    #dat <- summary(x)
    
    #print(dat)
    
    
    #For now
    print(unclass(x))
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
#' lb1 <- libname(tempdir()) 
#'            
#' # Test for "lib" class
#' is.lib(lb1)  
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


getUniqueName <- function(nm, nms) {
  
  ret <- nm
  pos <- match(nm, nms)
  if (!is.na(pos)) {
    
    ex <- strsplit(basename(nm), split="\\_")[[1]]
    print(ex)
    num <- as.numeric(ex[-1])
    print(num)
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


x <- c("text", "text", "text", "text")


