# Globals -----------------------------------------------------------------

#' @noRd
e <- new.env(parent = emptyenv())
e$env <- as.environment(1)
e$libs <- list()

# Libname Definition ------------------------------------------------------


#' @title Create a data library
#' @description A data library is a collection of data frames  A data 
#' library allows you to manage and store data frames as a unit.  The 
#' \code{libname} function defines the library.
#' @details A data library is an S3 object of class "lib".  The purpose of 
#' the library is to combine related data frames, and allow you to manipulate all
#' of them as a single object.  
#' 
#' The function uses the \code{\link[readr]{readr}}, \code{\link[readxl]{readxl}},
#' and \code{\link[haven:read_sas]{haven}} packages to import data.  
#' These packages have
#' sensible defaults, and most of the time your data will import in an
#' acceptable manner.  In some cases, however, you may want to control how
#' the data is imported.  For those cases, you can use to the
#' \code{...} parameter on the \code{libname} function to pass parameters to
#' the import functions.  
#' @param name The unquoted name of the library to create.  This name will 
#' be created as a variable in the global environment.
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
#' # for illustration purposes
#' saveRDS(trees, file.path(tmp, "trees.rds"))
#' saveRDS(rock, file.path(tmp, "rocks.rds"))
#' saveRDS(beaver1, file.path(tmp, "beaver1.rds"))
#' 
#' # Create data library
#' libname(dat, tmp)
#' 
#' # Print dictionary for library
#' dictionary(dat)
#' 
#' # Load library into workspace 
#' lib_load(dat)
#' 
#' # Print summaries for each data frame
#' summary(dat.rocks)
#' summary(dat.trees)
#' summary(dat.beaver1)
#' 
#' #Unload from workspace
#' lib_unload(dat)
#' 
#' @import readr
#' @import readxl
#' @import haven
#' @import utils
#' @export
libname <- function(name, directory_path, filter = NULL, 
                    read_only = FALSE, ...) {
  
  name_c <- deparse1(substitute(name, env = environment()))
  
  if (!dir.exists(directory_path))
    stop(paste("Directory path does not exist. Use lib_create() to",
               "create a new data directory."))

  # Create new structure of class "lib"
  l <- structure(list(), class = c("lib", "list"))
  
  attr(l, "name") <- name_c
  attr(l, "path") <- directory_path
  attr(l, "filter") <- filter
  attr(l, "read_only") <- read_only
  attr(l, "loaded") <- FALSE
  
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
      
      dat <- NULL
      
      if (ext == "csv") {
        
        dat <- read_csv(fp, ...)
        
      } else if (ext == "rds") {
        
        dat <- read_rds(fp, ...)
        
      } else if (ext == "sas7bdat") {
        
        dat <- read_sas(fp, ...)
        
      } else if (ext == "xlsx") {
        
        dat <- read_xlsx(fp, ...)
        
      } else if (ext == "xls") {
        
        dat <- read_xls(fp, ...)
        
      }  else if (ext == "dat"){
        
        dat <- read_table(fp, ...)
      }
      
      if (any(class(dat) == "data.frame")) {
      
        if (nm %in% names(l))
          warning(paste("The name", nm, "already exists in the library.",
                        "Data will be replaced."))
        
        attr(dat, "name") <- nm
        attr(dat, "extension") <- ext
        attr(dat, "path") <- fp
        l[[nm]] <- dat
      }
    }

  }
  
  assign(name_c, l, envir = as.environment(e$env))
  e$libs[[name_c]] <- attributes(l)
  
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
as.lib <- function(x, path) {
  UseMethod("as.lib", x)
}

#' @title Convert a List of Data Frames to a Data Library
#' @param x The list to convert
#' @param path The path to associate with the library.
#' @family lib
#' @export
as.lib.list <- function(x, path) {
  
  name_c <- deparse1(substitute(x, env = environment()))
  
  class(x) <- list("lib", class(x))
  
  attr(x, "name") <- name_c
  attr(x, "path") <- path
  attr(x, "loaded") <- FALSE
  e$libs[[name_c]] <- attributes(x)
  
  return(x)
  
}


# Manipulation Functions --------------------------------------------------


#' @title Load a Library into the Global Environment
#' @description The \code{lib_load} function loads a data library into
#' the global environment.  The data frames will be loaded with 
#' <library>.<data frame> syntax.
#' @param x The data library to load.
#' @return The loaded data library. 
#' @seealso \code{\link{lib_unload}} to unload the library.
#' @family lib
#' @export
lib_load <- function(x) {
  
  if (all(class(x) != "lib"))
    stop("Object must be a data library.")
  
  # Get library name
  libnm <- deparse1(substitute(x, env = environment())) 
  
  # For each name in library, assign to global environment
  for (nm in names(x)) {
    n <-  paste0(libnm, ".", nm)
    assign(n, x[[nm]], envir = as.environment(e$env))
  }
  
  attr(x, "loaded") <- TRUE
  assign(libnm, x, envir = as.environment(e$env))
  e$libs[[libnm]] <- attributes(x)
  
  return(x)
}


#' @title Unload a Library from the Workspace
#' @description The \code{lib_unload} function unloads a data library from
#' the workspace environment.  The unload function does not delete the data 
#' or the remove the library.  It simply removes the data frames from working 
#' memory.
#' @param x The data library to unload.
#' @param sync Whether to sync the workspace with the library list before
#' it is unloaded.  If you want to unload the workspace without saving the 
#' workspace data, set this parameter to false.
#' @return The unloaded data library.
#' @seealso \code{\link{lib_load}} to load the library.
#' @family lib
#' @export
lib_unload <- function(x, sync = TRUE) {

  if (all(class(x) != "lib"))
    stop("Object must be a data library.")
  
  # Get name of library
  libnm <- deparse1(substitute(x, env = environment())) 
  
  if (sync)
    x <- lib_sync(x, libnm)
  
  # Get data frame names
  ls <-  paste0(libnm, ".", names(x))
  
  # Intersect names with what is actually in the environment
  ls <- intersect(ls, ls(envir = e$env))
  
  # Remove intersection
  rm(list = ls, envir = e$env)
  
  # Mark as unloaded
  attr(x, "loaded") <- FALSE
  assign(libnm, x, envir = as.environment(e$env))
  e$libs[[libnm]] <- attributes(x)
  
  return(x)
}

#' @title Write a data library to the file system
#' @description The \code{lib_write} function writes the data library
#' to the file system.  By default, the library will be written to the 
#' directory for which it was defined, and each data frame will be written
#' in the format from which it was read.  Data frames that were not read
#' from a file will be saved in RDS format, unless otherwise specified.  To
#' specify a different format, set the \code{type} parameter on 
#' \code{lib_write}.  Setting this parameter will override any source
#' file types and save the data in a consistent file format.
#' @param x The format catalog to write.
#' @param type The type of data library. Default is NULL, meaning the library
#' is not typed.  Valid values are "rds", "sas7bdat", "xls", "xlsx", "csv",
#' and "dat".
#' The data files will be saved as the type specified on this parameter, 
#' no matter from which type of file they were input.  If the type is
#' passed as a single string, all data files will be saved as that single type.
#' The type can also be passed as a named vector of types, with each name 
#' corresponding to the name of the data frame in the library.
#' @return The full path of the saved data library.
#' @family lib
#' @export
lib_write <- function(x, type = NULL) {
  
  if (all(class(x) != "lib"))
    stop("Object must be a data library.")
  
  lbnm <- deparse1(substitute(x, env = environment()))
  
  if (e$libs[[lbnm]]$loaded) {
    x <- lib_sync(x)
  }
    
  nms <- names(x)
  
  # Get path
  pth <- attr(x, "path")
  
  for (nm in nms) {
    
    styp <- attr(x[[nm]], "extension")
    if (!is.null(type))
      typ <- type
    else if (!is.null(styp))
      typ <- styp
    else
      typ <- "rds"
    
    if (file.exists(pth))
      file.remove(pth)
    
    saveRDS(x, pth)
  }
  
  return(pth)
}


#' @title Synchronize Loaded Library
#' @description The \code{lib_sync} function synchronizes the data
#' loaded into the working environment with the data frames stored 
#' in the data library object.  This synchronization is necessary only
#' for libraries that have been loaded into the working environment.
#' The function is used internally to the \strong{libr} package, but 
#' may be useful to package users in some situations. 
#' @param x The data library to synchronize.
#' @param name The name of the library to sync if not the variable
#' name of the incoming library. Used internally.
#' @return The synchronized data library.
#' @export
lib_sync <- function(x, name = NULL) {
  
  if (all(class(x) != "lib"))
    stop("Object must be a data library.")
  
  if (!is.null(name))
    libnm <- name
  else 
    libnm <- deparse1(substitute(x, env = environment()))
  
  if (e$libs[[libnm]]$loaded == TRUE) {
    
    # Get names from what is actually in the environment.
    # Any names removed from environment will not be removed from list.
    # Any names added to environment will be added to list.
    # Idea is to always preserve data unless the user specifically 
    # asks to kill it.
    en <- ls(envir = e$env)
    fen <- grep(paste0("^", libnm, "\\."), en, value = TRUE)

    for (gnm in fen) {
      #print(gnm)
      nm <- sub(paste0(libnm, "."), "", gnm, fixed = TRUE)
      #print(nm)
      x[[nm]] <- get(gnm, envir = e$env)
      
    }
    
    assign(libnm, x, envir = as.environment(e$env))
    
  } else {
    
    message("NOTE: library is not loaded") 
    
  }
  
  return(x)
}


#' @title Copy a Data Library
#' @description The \code{lib_copy} function copies a data library.  The 
#' function accepts a library and a destination path.  If the destination 
#' path does not exist, the function will attempt to create it.  Note that
#' the copy will result in the current data in memory written to the new
#' destination directory.  
#' @param x The library to copy.
#' @param nm The unquoted variable name to hold the new library.
#' @param directory_path The path to copy the library to.
#' @family lib
#' @export
lib_copy <- function(x, nm, directory_path) {
  
  if (all(class(x) != "lib"))
    stop("Object must be a data library.")
  
  if (!dir.exists(directory_path))
    dir.create(directory_path)
  
  libnm <- deparse1(substitute(x, env = environment))
  
  if (e$libs[[libnm]]$loaded) {
    
    x <- lib_sync(x) 
  }
  
  attr(x, "path") <- directory_path
  x <- lib_write(x)
  
  e$libs[[nm]] <- x
  
  return(x)
}

#' @title Remove Data from a Data Library
#' @description The \code{lib_remove} function removes an item from the 
#' data library, and deletes the source file for that data.
#' @param x The data library.
#' @param name The quoted name of the item to remove from the data library. 
#' For more than one name, pass a vector of quoted names.
#' @return The library with the requested item removed.
#' @family lib
#' @export
lib_remove <- function(x, name) {
  
  if (all(class(x) != "lib"))
    stop("Object must be a data library.")
  
  libnm <- deparse1(substitute(x, env = environment()))
  
  
  for (nm in name) {
    pth <- attr(x[[nm]], "path")
    
    if (!is.null(pth)) {
      if (file.exists(pth))
        file.remove(pth)
    }
    
    x[[nm]] <- NULL
    
    if (e$libs[[libnm]]$loaded) {
      gnm <- paste0(libnm, ".", nm)
      rm(list = gnm, envir = as.environment(e$env))
    }
  }
  
  assign(libnm, x, envir = as.environment(e$env))
  
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
#' @param ... The data frame(s) to append to the library.
#' @param .name The reference name to use for the data.  By default,
#' the name will be the variable name.  To assign a name different
#' from the variable name, assign a quoted name to this parameter.  If more
#' than one data set is being appended, assign a vector of quoted names.
#' @family lib
#' @export
lib_append <- function(x, ..., .name = NULL) {
  
  if (all(class(x) != "lib"))
    stop("Object must be a data library.")
  
  lbnm  <- deparse1(substitute(x, env = environment()))
  
  nms <- as.character(substitute(c(...), env = environment()))
  nms <- nms[2:length(nms)]  
  
  lst <- list(...)
  
  if (!is.null(.name)) {
    nms <- .name
  } 
    
  i <- 1
  for (nm in nms) {
    if (nm %in% names(x))
      warning(paste("The name", nm, "already exists in the library",
                    lbnm, ". Data will be replaced."))
    
    x[[nm]] <- lst[[i]]
    
    if (e$libs[[lbnm]]$loaded) {
      
      assign(paste0(lbnm, ".", nm), lst[[i]], envir = as.environment(e$env))
    }
    i <- i + 1
  }
  
  assign(lbnm, x, envir = as.environment(e$env))
  
  return(x)
  
}


#' @title Delete a Data Library
#' @description The \code{lib_delete} function deletes a data library from
#' the file system.  All data files associated with the library will be deleted.
#' If other files exist in the library directory, they will not be affected
#' by the delete operation.  The directory that contains the data will also
#' not be affected by the delete operation.  To delete the data directory, 
#' use the \code{\link[base]{unlink}} function.
#' @param x The data library to delete.
#' @family lib
#' @export
lib_delete <- function(x) {
  
  if (all(class(x) != "lib"))
    stop("Object must be a data library.")
  
  lnm <- deparse1(substitute(x, env = environment()))
  
  lib_unload(x)
  
  for (dat in x) {
    pth <- attr(dat, "path")
    file.remove(pth)
  }
  
  e$libs[[lnm]] <- NULL
  
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
#' @description The \code{lib_info} function returns a data frame of information
#' about each item in the data library.  That information includes the item
#' name, file extension, number of rows, number of columns, size in bytes, 
#' the last modified date.
#' @param x The data library.
#' @return A data frame of information about the library.
#' @family lib
#' @export
lib_info <- function(x) {
  
  if (all(class(x) != "lib"))
    stop("Object must be a data library.")

  ret <- NULL

  for (nm in names(x)) {
    
    itm <- x[[nm]]
    
    pth <- attr(itm, "path")
    if (!is.null(pth)) { 
      info <- file.info(pth)
      lm <- info[1, "mtime"]
    } else {
      lm <- NA
    }
    
    if (is.null(attr(itm, "extension"))) {
      ex <- NA 
    } else {
      ex <- attr(itm, "extension") 
    }
      
    
    rw <- data.frame(Name = nm, 
                     Extension = ex,
                     Rows = nrow(itm),
                     Cols = ncol(itm),
                     Size = format(object.size(itm), units = "auto"),
                     LastModified = lm)
    
    if (is.null(ret))
      ret <- rw
    else 
      ret <- rbind(ret, rw)
    
  }
  
  return(ret)
}


#' @title Set the Environment for the Library Functions
#' @description The \code{lib_environment} function sets the working environment
#' for the \strong{libr} package.  By default, the environment is set to the 
#' current global environment.  If an alternative environment is desired, a 
#' user may set it with this function. 
#' @param env The environment to use.
#' @return The new environment.
#' @family lib
#' @export
lib_environment <- function(env = NULL) {
  
  if (is.null(env)) {
   if (is.null(options()[["libr.env"]])) {
      e$env = as.environment(1)
   } else {
     e$env = options()[["libr.env"]]
   }
    
  } else {
    e$env = env
  }
  
  
  return(env)
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
#' # Create data library
#' libname(dat, tmp)
#' 
#' # Print library summary 
#' print(dat)
#' @export
print.lib <- function(x, ..., verbose = FALSE) {
  
  if (verbose == TRUE) {
    
    print(unclass(x))  
    
  } else {
    
    cat(paste0("# library '",  attr(x, "name"), "': ", length(x), " items\n"))
    
    at <- paste("- attributes:")
    if (!is.null(attr(x, "filter")))
      at <- paste(at, attr(x, "filter"))
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

