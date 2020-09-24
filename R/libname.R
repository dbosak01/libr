
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
#' @param read_only Whether the library should be created as read only.
#' Default is FALSE.  If TRUE, the user will be restricted from
#' appending, removing, or writing data from the library to the file system.
#' @param ... Follow-on parameters to the data import functions.  Which
#' parameters exist depend on which types of files are being imported.
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
#' data <- libname(tmp)
#' 
#' # Print library summary 
#' print(data)
#' @import readxl
#' @import haven
#' @import utils
#' @export
libname <- function(directory_path, filter = NULL, read_only = FALSE, ...) {
  
  if (!dir.exists(directory_path))
    stop(paste("Directory path does not exist. Use lib_create() to",
               "create a new data directory."))
  
  
  # Create new structure of class "lib"
  l <- structure(list(), class = c("lib", "list"))
  
  attr(l, "path") <- directory_path
  attr(l, "filter") <- filter
  attr(l, "read_only") <- read_only
  
  if (is.null(filter))
    lst <- list.files(directory_path)
  else
    lst <- list.files(directory_path, pattern = filter)
  
  for (fl in lst) {
    fp <- file.path(directory_path, fl)
    ext <-  getExtension(fl)
    nm <- getFileName(fl)
    #nm <- getUniqueName(nm, names(l))
    
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
        
        dat <- read.table(fp, ...)
      }
      
      if (nm %in% names(l))
        warning(paste("The name", nm, "already exists in the library.",
                      "Data will be replaced."))
      
      attr(dat, "name") <- nm
      attr(dat, "extension") <- ext
      attr(dat, "path") <- fp
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
#' @param path The path for the specified library.
#' @return A library object, created using the information in the 
#' input object.
#' @seealso For class-specific methods, see 
#' \code{\link{as.lib.list}}.
#' @family lib
#' @export
as.lib <- function (x, path) {
  UseMethod("as.lib", x)
}

#' @title Convert a List of Data Frames to a Data Library
#' @param x The list to convert
#' @param path The path to associate with the library.
#' @family lib
#' @export
as.lib.list <- function(x, path) {
  
  
  class(x) <- list("lib", class(x))
  
  attr(x, "path") <- path
  
  return(x)
  
}


# Manipulation Functions --------------------------------------------------


#' @title Load a Library into the Global Environment
#' @description The \code{lib_load} function loads a data library into
#' the global environment.  The data frames will be loaded with 
#' <library>.<data frame> syntax.
#' @param x The data library to load.
#' @param pos The environment to load data into.  This parameter is used
#' internally and is normally not modified by the user.
#' @return The loaded data library. 
#' @seealso \code{\link{lib_unload}} to unload the library.
#' @family lib
#' @export
lib_load <- function(x, pos = 1) {
  
  if (all(class(x) != "lib"))
    stop("Object must be a data library.")
  
  # Get library name
  libname <- deparse1(substitute(x, env = environment())) 
  
  # For each name in library, assign to global environment
  for (nm in names(x)) {
    n <-  paste0(libname, ".", nm)
    assign(n, x[[nm]], envir = as.environment(pos))
  }
  
  return(x)
}


#' @title Unload a Library from the Global Environment
#' @description The \code{lib_unload} function unloads a data library from
#' the global environment.  The unload function does not delete the data 
#' or the remove the library.  It simply removes the data frames from global 
#' memory.
#' @param x The data library to unload.
#' @return The unloaded data library.
#' @seealso \code{\link{lib_load}} to load the library.
#' @family lib
#' @export
lib_unload <- function(x) {

  if (all(class(x) != "lib"))
    stop("Object must be a data library.")
  
  # Get name of library
  libname <- deparse1(substitute(x, env = environment())) 
  
  # Get data frame names
  ls <-  paste0(libname, ".", names(x))
  
  # Intersect names with what is actually in the environment
  ls <- intersect(ls, ls(envir = .GlobalEnv))
  
  # Remove intersection
  rm(list = ls, envir = .GlobalEnv)
  
  return(x)
}

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
#' is not typed.  Valid values are "rds", "sas7bdat", "xls", "xlsx", "csv",
#' and "dat".
#' The data files will be saved as the type specified on this parameter, 
#' no matter from which type of file they were input.
#' @return The full path of the saved format catalog.
#' @family lib
#' @export
lib_write <- function(x, dir_path = getwd(), file_name = deparse(substitute(x, 
                                                     env = environment())),
                      type = NULL) {
  pth <- file.path(dir_path, paste0(file_name, ".fcat"))
  
  
  if (file.exists(pth))
    file.remove(pth)
  
  saveRDS(x, pth)
  
  return(pth)
}


#' @title Copy a Data Library
#' @description The \code{lib_copy} function copies a data library.  The 
#' function accepts a library and a destination path.  If the destination 
#' path does not exist, the function will attempt to create it.  Note that
#' the copy will result in the current data in memory written to the new
#' destination directory.  
#' @param x The library to copy.
#' @param directory_path The path to copy the library to.
#' @family lib
#' @export
lib_copy <- function(x, directory_path) {
  
  if (all(class(x) != "lib"))
    stop("Object must be a data library.")
  
  if (!dir.exists(directory_path))
    dir.create(directory_path)
  
  attr(x, "path") <- directory_path
  lib_write(x)
  
  return(x)
}

#' @title Remove Data from a Data Library
#' @description The \code{lib_remove} function removes an item from the 
#' data library, and deletes the source file for that data.
#' @param x The data library.
#' @param nm The name of the item to remove from the data library.  
#' @return The library with the requested item removed.
#' @family lib
#' @export
lib_remove <- function(x, nm) {
  
  if (all(class(x) != "lib"))
    stop("Object must be a data library.")
  
  
  pth <- attr(x[[nm]], "path")
  
  if (file.exists(pth))
    file.remove(pth)
  
  x[[nm]] <- NULL
  
  return(x)
}

#' @title Create a New Data Directory and Library
#' @description The \code{\link{lib_create}} function creates a new 
#' data directory if it does not exist, and creates a new library
#' assigned to that directory.
#' @param directory_path The path of the directory to create.
#' @return The new data library.
#' @family lib
#' @export
lib_create <- function(directory_path) {
  
  
  if (!file.exists(directory_path))
    dir.create(directory_path)
  
  if (dir.exists(directory_path)) {
    ret <- libname(directory_path)
  } else
    stop("Directory could not be created.")
  
  return(ret)
}



#' @title Append Data to a Data Library
#' @description The \code{\link{lib_append}} function adds a data frame
#' to an existing data library.
#' @param x The library to append data to.
#' @param df The data frame to append to the library.
#' @family lib
#' @export
lib_append <- function(x, df) {
  
  if (all(class(x) != "lib"))
    stop("Object must be a data library.")
  
  nm <- deparse1(substitute(df, env = environment()))
  lbnm  <- deparse1(substitute(x, env = environment()))
  
  if (nm %in% names(x))
    warning(paste("The name", nm, "already exists in the library",
                  lbnm, ". Data will be replaced."))
  
  x[[nm]] <- df
  
}

#' @title Delete a Data Library
#' @description The \code{lib_delete} function deletes a data library from
#' the file system.  All data files associated with the library will be deleted.
#' If other files exist in the library directory, they will not be affected
#' by the delete operation.  The directory that contains the data will also
#' not be affected by the delete operation.  To delete the data directory, 
#' use the \code{\link{unlink}} function.
#' @param x The data library to delete.
#' @family lib
#' @export
lib_delete <- function(x) {
  
  if (all(class(x) != "lib"))
    stop("Object must be a data library.")
  
  for (dat in x) {
    pth <- attr(dat, "path")
    file.remove(pth)
  }
  
  return(x)
}

#' @title Get the Path for a Data Library
#' @description The \code{lib_path} function returns the current path of the 
#' the library as a string.
#' @param x The data library.
#' @return The path of the data library as a single string.
#' @family lib
#' @export
lib_path <- function(x) {
  
  if (all(class(x) != "lib"))
    stop("Object must be a data library.")
  
  ret <- attr(x, "path")
  
  return(ret)
}

#' @title Get the Size of a Data Library
#' @description The \code{lib_size} function returns the number of bytes used
#' by the data library, as stored on disc.  
#' @param x The data library.
#' @return The size of the data library in bytes as stored on the file system.
#' @family lib
#' @export
lib_size <- function(x) {
  
  if (all(class(x) != "lib"))
    stop("Object must be a data library.")
  
  path <- attr(x, "path")
  filter <- attr(x, "filter")
  
  if (is.null(filter)) 
    info <- file.info(list.files(path, full.names = TRUE))
  else 
    info <- file.info(list.files(path, full.names = TRUE, pattern = filter))
  
  if (nrow(info) == 0)
    ret <- 0
  else
    ret <- sum(info[["size"]])
  
  
  return(ret)
}


#' @title Get Information on a Data Library
#' @param x The data library.
#' @family lib
#' @export
lib_info <- function(x) {
  
  if (all(class(x) != "lib"))
    stop("Object must be a data library.")
  
  path <- attr(x, "path")
  filter <- attr(x, "filter")
  
  if (is.null(filter)) 
    info <- file.info(list.files(path, full.names = TRUE))
  else 
    info <- file.info(list.files(path, full.names = TRUE, pattern = filter))
  
  
  return(info)
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


