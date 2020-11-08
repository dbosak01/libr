# Globals -----------------------------------------------------------------

#' @noRd
e <- new.env(parent = emptyenv())
e$env <- as.environment(1)


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
#' @param directory_path A directory path in which the data resides.  The 
#' incoming 
#' data can be in any file format: rds, csv, sas7bdat, etc.  The 
#' \code{libname} function will read any type of data into the library, 
#' and render as an R data frame.  
#' @param type One or more file extensions to filter the incoming data.  The
#' default value is NULL, meaning all recognized data files will be input.
#' Valid values are "rds", "sas7bdat", "xls", "xlsx", "csv". 
#' When saved with \code{lib_write}, each file will be saved in its original
#' file format, unless otherwise specified on the \code{type} parameter of 
#' \code{lib_write}.
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
#' # Clean up
#' lib_delete(dat)
#' @import readr
#' @import readxl
#' @import haven
#' @import utils
#' @export
libname <- function(name, directory_path, type = NULL, 
                    read_only = FALSE, ...) {
  
  if (!file.exists(directory_path))
    dir.create(directory_path)
  
  name_c <- deparse1(substitute(name, env = environment()))
  

  # Create new structure of class "lib"
  l <- structure(list(), class = c("lib", "list"))
  
  attr(l, "name") <- name_c
  attr(l, "path") <- directory_path
  attr(l, "read_only") <- read_only
  attr(l, "loaded") <- FALSE
  attr(l, "type") <- type
    
  
  if (is.null(type))
    lst <- list.files(directory_path)
  else
    lst <- list.files(directory_path, pattern = type)
  
  for (fl in lst) {
    fp <- file.path(directory_path, fl)
    ext <-  getExtension(fl)
    nm <- getFileName(fl)
    
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
  
  assign(name_c, l, envir = e$env)

  
  return(l)
  
}


# Manipulation Functions --------------------------------------------------


#' @title Load a Library into the Workspace
#' @description The \code{lib_load} function loads a data library into
#' an environment. By default this is the global environment.  The data frames 
#' will be loaded with <library>.<data frame> syntax.  Loading the data frames
#' into the environment makes them easy to access and use in your program.
#' @param x The data library to load.
#' @return The loaded data library. 
#' @seealso \code{\link{lib_unload}} to unload the library.
#' @family lib
#' @examples 
#' # Create temp directory
#' tmp <- tempdir()
#' 
#' # Save some data to temp directory for illustration purposes
#' saveRDS(iris, file.path(tmp, "iris.rds"))
#' saveRDS(ToothGrowth, file.path(tmp, "ToothGrowth.rds"))
#' saveRDS(PlantGrowth, file.path(tmp, "PlantGrowth.rds"))
#' 
#' # Create library
#' libname(dat, tmp)
#' 
#' # Load library into workspace
#' lib_load(dat)
#' 
#' # Examine workspace
#' ls()
#' # [1] "dat" "dat.iris" "dat.PlantGrowth" "dat.ToothGrowth" "tmp"
#' 
#' # Use some data
#' summary(dat.PlantGrowth)
#' summary(dat.ToothGrowth)
#' 
#' # Unload library
#' lib_unload(dat)
#' 
#' # Examine workspace again
#' ls()
#' # [1] "dat" "tmp"
#' 
#' # Clean up
#' lib_delete(dat)
#' @export
lib_load <- function(x) {
  
  if (all(class(x) != "lib"))
    stop("Object must be a data library.")
  
  # Get library name
  libnm <- deparse1(substitute(x, env = environment())) 
  
  # For each name in library, assign to global environment
  for (nm in names(x)) {
    n <-  paste0(libnm, ".", nm)
    assign(n, x[[nm]], envir = e$env)
  }
  
  attr(x, "loaded") <- TRUE
  assign(libnm, x, envir = e$env)
  
  return(x)
}


#' @title Unload a Library from the Workspace
#' @description The \code{lib_unload} function unloads a data library from
#' the workspace environment.  The unload function does not delete the data 
#' or the remove the library.  It simply removes the data frames from working 
#' memory.  By defautl, the \code{unload} function will also synchronize the 
#' data in working memory with the data stored in the library list, which can
#' become out of sync if you change the data in working memory.
#' @param x The data library to unload.
#' @param sync Whether to sync the workspace with the library list before
#' it is unloaded.  If you want to unload the workspace without saving the 
#' workspace data, set this parameter to FALSE.
#' @param name The name of the library to unload, if the name is different
#' than the variable name.  Used internally.
#' @return The unloaded data library.
#' @seealso \code{\link{lib_load}} to load the library.
#' @family lib
#' @examples 
#' # Create temp directory
#' tmp <- tempdir()
#' 
#' # Save some data to temp directory for illustration purposes
#' saveRDS(iris, file.path(tmp, "iris.rds"))
#' saveRDS(ToothGrowth, file.path(tmp, "ToothGrowth.rds"))
#' saveRDS(PlantGrowth, file.path(tmp, "PlantGrowth.rds"))
#' 
#' # Create library
#' libname(dat, tmp)
#' 
#' # Load library into workspace
#' lib_load(dat)
#' 
#' # Examine workspace
#' ls()
#' # [1] "dat" "dat.iris" "dat.PlantGrowth" "dat.ToothGrowth" "tmp"
#' 
#' # Use some data
#' summary(dat.PlantGrowth)
#' summary(dat.ToothGrowth)
#' 
#' # Unload library
#' lib_unload(dat)
#' 
#' # Examine workspace again
#' ls()
#' # [1] "dat" "tmp"
#' 
#' # Clean up
#' lib_delete(dat)
#' @export
lib_unload <- function(x, sync = TRUE, name = NULL) {

  if (all(class(x) != "lib"))
    stop("Object must be a data library.")
  
  # Get name of library
  libnm <- deparse1(substitute(x, env = environment())) 
  
  if (!is.null(name))
    libnm <- name
  
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
  assign(libnm, x, envir = e$env)
  
  return(x)
}


#' @title Add Data to a Data Library
#' @description The \code{\link{lib_add}} function adds a data frame
#' to an existing data library.  The function will both add the data
#' to the library list, and immediately write the data to the library
#' directory location.  The data will be written in the file format
#' associated with the library.  If no file format is associated with 
#' the library, it will be written as an RDS file.
#' @param x The library to add data to.
#' @param ... The data frame(s) to add to the library.
#' @param type The type of file format to assign to this data. By default,
#' the function will assign the type associated with the library.  If no
#' type is associated with the library, the type will be assigned "rds".
#' @param name The reference name to use for the data.  By default,
#' the name will be the variable name.  To assign a name different
#' from the variable name, assign a quoted name to this parameter.  If more
#' than one data set is being appended, assign a vector of quoted names.
#' @family lib
#' @examples 
#' #' # Create temp directory
#' tmp <- tempdir()
#' 
#' # Create library
#' libname(dat, tmp)
#' 
#' # Add data to the library
#' lib_add(dat, mtcars)
#' lib_add(dat, beaver1)
#' lib_add(dat, iris)
#' 
#' # Examine the library
#' dat
#' # library 'dat': 3 items
#' # - attributes: not loaded
#' # - path: C:\Users\User\AppData\Local\Temp\RtmpCSJ6Gc
#' # - items:
#' #      Name Extension Rows Cols   Size        LastModified
#' # 1  mtcars       rds   32   11 7.5 Kb 2020-11-05 19:32:00
#' # 2 beaver1       rds  114    4 5.1 Kb 2020-11-05 19:32:04
#' # 3    iris       rds  150    5 7.5 Kb 2020-11-05 19:32:08
#' 
#' # Clean up
#' lib_delete(dat)
#' @export
lib_add <- function(x, ..., type = NULL, name = NULL) {
  
  if (all(class(x) != "lib"))
    stop("Object must be a data library.")
  
  lbnm  <- deparse1(substitute(x, env = environment()))
  
  if (attr(x, "read_only") == FALSE) {
    
    nms <- as.character(substitute(c(...), env = environment()))
    nms <- nms[2:length(nms)]  
    
    lst <- list(...)
    
    if (!is.null(name)) {
      nms <- name
    } 
    
    if (!is.null(type)) {
      typ <- type
    } else {
      if (!is.null(attr(x, "type")))
        typ <- attr(x, "type")
      else 
        typ <- "rds"
    }
    
    i <- 1
    for (nm in nms) {
      if (nm %in% names(x))
        warning(paste0("The name '", nm, "' already exists in the library '",
                      lbnm, "'. Data will be replaced."))
      
      x[[nm]] <- lst[[i]]
      attr(x[[nm]], "name") <- nm
      attr(x[[nm]], "extension") <- typ
      
      if (is.loaded.lib(lbnm)) {
        
        assign(paste0(lbnm, ".", nm), x[[nm]], envir = e$env)
      }
      
      pth <- file.path(attr(x, "path"), paste0(nm, ".", typ))
      
      writeData(x[[nm]], typ, pth)
      
      i <- i + 1
    }
    
    assign(lbnm, x, envir = e$env)
    
  } else {
    
   stop(paste0("Cannot add to library '", lbnm, "' because it is read-only.")) 
  }
    
  
  return(x)
  
}


#' @title Replace Data in a Data Library
#' @description The \code{\link{lib_replace}} function replaces a data frame
#' in an existing data library.  The function will both replaced the data
#' in the library list, and immediately write the data to the library
#' directory location.  The data will be written in the file format
#' associated with the library.  If no file format is associated with 
#' the library, it will be written as an RDS file.  
#' @param x The library to replace data in.
#' @param ... The data frame(s) to replace.
#' @param type The type of file format to assign to this data. By default,
#' the function will assign the type associated with the library.  If no
#' type is associated with the library, the type will be assigned "rds".
#' @param name The reference name to use for the data.  By default,
#' the name will be the variable name.  To assign a name different
#' from the variable name, assign a quoted name to this parameter.  If more
#' than one data set is being appended, assign a vector of quoted names.
#' @family lib
#' @examples 
#' #' # Create temp directory
#' tmp <- tempdir()
#' 
#' # Create library
#' libname(dat, tmp)
#' 
#' # Add data to the library
#' lib_add(dat, mtcars)
#' 
#' # Examine the library
#' dat
#' # library 'dat': 3 items
#' # - attributes: not loaded
#' # - path: C:\Users\User\AppData\Local\Temp\RtmpCSJ6Gc
#' # - items:
#' #      Name Extension Rows Cols   Size        LastModified
#' # 1  mtcars       rds   32   11 7.5 Kb 2020-11-05 19:32:00
#' 
#' # Replace data with subsets
#' lib_replace(dat, mtcars[1:10, 1:5], name = "mtcars")
#' 
#' # Examine the library again
#' dat
#' # library 'dat': 3 items
#' # - attributes: not loaded
#' # - path: C:\Users\User\AppData\Local\Temp\RtmpCSJ6Gc
#' # - items:
#' #      Name Extension Rows Cols   Size        LastModified
#' # 1  mtcars       rds   10    5 7.5 Kb 2020-11-05 19:33:00
#' 
#' # Clean up
#' lib_delete(dat)
#' @export
lib_replace <- function(x, ..., type = NULL, name = NULL) {
  
  if (all(class(x) != "lib"))
    stop("Object must be a data library.")
  
  lbnm  <- deparse1(substitute(x, env = environment()))
  
  if (attr(x, "read_only") == FALSE) {
    
    nms <- as.character(substitute(c(...), env = environment()))
    nms <- nms[2:length(nms)]  
    
    lst <- list(...)
    
    if (!is.null(name)) {
      nms <- name
    } 
    
    if (!is.null(type)) {
      typ <- type
    } else {
      if (!is.null(attr(x, "type")))
        typ <- attr(x, "type")
      else 
        typ <- "rds"
    }
    
    i <- 1
    for (nm in nms) {
      if (!nm %in% names(x))
        warning(paste0("The name '", nm, "' does not exist in the library '",
                       lbnm, "'. Data will be added."))
      
      x[[nm]] <- lst[[i]]
      attr(x[[nm]], "name") <- nm
      attr(x[[nm]], "extension") <- typ
      
      if (is.loaded.lib(lbnm)) {
        
        assign(paste0(lbnm, ".", nm), x[[nm]], envir = e$env)
      }
      
      pth <- file.path(attr(x, "path"), paste0(nm, ".", typ))
      
      writeData(x[[nm]], typ, pth)
      
      i <- i + 1
    }
    
    assign(lbnm, x, envir = e$env)
    
  } else {
    
    stop(paste0("Cannot replace in library '", lbnm, "' because it is read-only.")) 
  }
  
  
  return(x)
  
}

#' @title Remove Data from a Data Library
#' @description The \code{lib_remove} function removes an item from the 
#' data library, and deletes the source file for that data.  If the library
#' is loaded, it will also remove that item from the workspace.
#' @param x The data library.
#' @param name The quoted name of the item to remove from the data library. 
#' For more than one name, pass a vector of quoted names.
#' @return The library with the requested item removed.
#' @family lib
#' @examples 
#' # Create temp directory
#' tmp <- tempdir()
#' 
#' # Create library
#' libname(dat, tmp)
#' 
#' # Add data to the library
#' lib_add(dat, mtcars)
#' lib_add(dat, beaver1)
#' lib_add(dat, iris)
#' 
#' # Examine the library
#' dat
#' 
#' # library 'dat': 3 items
#' # - attributes: not loaded
#' # - path: C:\Users\User\AppData\Local\Temp\RtmpCSJ6Gc
#' # - items:
#' #      Name Extension Rows Cols   Size        LastModified
#' # 1  mtcars       rds   32   11 7.5 Kb 2020-11-05 19:32:00
#' # 2 beaver1       rds  114    4 5.1 Kb 2020-11-05 19:32:04
#' # 3    iris       rds  150    5 7.5 Kb 2020-11-05 19:32:08
#' 
#' # Remove items from the library
#' lib_remove(dat, c("beaver1", "iris"))
#' 
#' # Examine the library again
#' dat
#' # library 'dat': 1 items
#' # - attributes: not loaded
#' # - path: C:\Users\User\AppData\Local\Temp\RtmpCSJ6Gc
#' # - items:
#' #     Name Extension Rows Cols   Size        LastModified
#' # 1 mtcars       rds   32   11 7.5 Kb 2020-11-05 19:32:40
#'
#' # Clean up
#' lib_delete(dat)
#' @export
lib_remove <- function(x, name) {
  
  if (all(class(x) != "lib"))
    stop("Object must be a data library.")
  
  libnm <- deparse1(substitute(x, env = environment()))
  libpth <- attr(x, "path")
  
  if (attr(x, "read_only") == FALSE) {
    for (nm in name) {
      
      pth <- file.path(libpth, paste0(nm, ".", attr(x[[nm]], "extension")))
      
      if (!is.null(pth)) {
        if (file.exists(pth))
          file.remove(pth)
      }
      
      x[[nm]] <- NULL
      
      if (is.loaded.lib(libnm)) {
        gnm <- paste0(libnm, ".", nm)
        rm(list = gnm, envir = e$env)
      }
    }
    
    assign(libnm, x, envir = e$env)
  
  } else {
    
    stop(paste0("Cannot remove from library '", libnm, "' because it is read-only.")) 
  }
  
  return(x)
}


#' @title Write a Data Library to the File System
#' @description The \code{lib_write} function writes the data library
#' to the file system.  By default, the library will be written to the 
#' directory for which it was defined, and each data frame will be written
#' in the format from which it was read.  Data frames that were not read
#' from a file will be saved in RDS format, unless otherwise specified.  To
#' specify a different format, set the \code{type} parameter on 
#' \code{lib_write}.  Setting this parameter will override any source
#' file types and save the data in the indicated file format.
#' 
#' The \strong{libr} package can write data files in csv, rds, xlsx, sas7bdat,
#' and a plain text file format.  Data read in xls file format will be saved 
#' as an xlsx.
#' 
#' @param x The format catalog to write.
#' @param type The type of data library. Default is NULL, meaning the library
#' is not typed.  Valid values are "rds", "sas7bdat", "xls", "xlsx", "csv",
#' and "dat".
#' The data files will be saved as the type specified on this parameter, 
#' no matter from which type of file they were input.  If the type is
#' passed as a single string, all data files will be saved as that single type.
#' The type can also be passed as a named vector of types, with each name 
#' corresponding to the name of the data frame in the library.
#' @return The saved data library.
#' @family lib
#' @import haven
#' @import readr
#' @import readxl
#' @examples 
#' # Create temp directory
#' tmp <- tempdir()
#' 
#' # Create library
#' libname(dat, tmp)
#' 
#' # Load the empty library 
#' lib_load(dat)
#' 
#' # Add data to the library
#' dat.mtcars <- mtcars
#' dat.beaver1 <- beaver1
#' dat.iris <- iris
#' 
#' # Unload the library
#' lib_unload(dat)
#' 
#' # Write the library to the file system
#' lib_write(dat)
#' 
#' # Examine the library
#' dat
#' # library 'dat': 3 items
#' #- attributes: not loaded
#' #- path: C:\Users\User\AppData\Local\Temp\RtmpCSJ6Gc
#' #- items:
#' #     Name Extension Rows Cols   Size        LastModified
#' #1 beaver1       rds  114    4 4.8 Kb 2020-11-05 20:47:16
#' #2    iris       rds  150    5 7.3 Kb 2020-11-05 20:47:16
#' #3  mtcars       rds   32   11 7.3 Kb 2020-11-05 20:47:16
#' 
#' # Clean up
#' lib_delete(dat)
#' @export
lib_write <- function(x, type = NULL) {
  
  if (all(class(x) != "lib"))
    stop("Object must be a data library.")
  
  lbnm <- deparse1(substitute(x, env = environment()))
  
  if (is.loaded.lib(lbnm)) {
    x <- lib_sync(x, lbnm)
  }
    
  nms <- names(x)
  
  # Get path
  libpth <- attr(x, "path")
  
  for (nm in nms) {
    
    styp <- attr(x[[nm]], "extension")
    if (!is.null(type))
      ext <- type
    else if (!is.null(attr(x, "type")))
      ext <- attr(x, "type")
    else if (!is.null(styp) && !is.na(styp))
      ext <- styp
    else
      ext <- "rds"
    
    attr(x[[nm]], "extension") <- ext
    
    fp <- file.path(libpth, paste0(nm, ".", ext))

    writeData(x[[nm]], ext, fp)  
  }
  
  assign(lbnm, x, envir = e$env)
  
  return(x)
}


#' @title Synchronize Loaded Library
#' @description The \code{lib_sync} function synchronizes the data
#' loaded into the working environment with the data frames stored 
#' in the data library object.  This synchronization is necessary only
#' for libraries that have been loaded into the working environment.
#' The function
#' copies data frames from the working environment to the library
#' list, overwriting any data in the list. The function is useful when 
#' you want to update the library list, but 
#' not unload the data from working memory.
#' @param x The data library to synchronize.
#' @param name The name of the library to sync if not the variable
#' name of the incoming library. Used internally.
#' @return The synchronized data library.
#' @examples 
#' # Create temp directory
#' tmp <- tempdir()
#' 
#' # Create library
#' libname(dat, tmp)
#' 
#' # Examine empty library
#' dat
#' # library 'dat': 0 items
#' # - attributes: not loaded
#' # - path: C:\Users\User\AppData\Local\Temp\RtmpCSJ6Gc
#' # NULL
#' 
#' # Load the library 
#' lib_load(dat)
#' 
#' # Add data to the workspace
#' dat.mtcars <- mtcars
#' dat.beaver1 <- beaver1
#' dat.iris <- iris
#' 
#' # Sync the library
#' lib_sync(dat)
#' 
#' # Examine the library again
#' dat
#' # library 'dat': 3 items
#' # - attributes: loaded
#' # - path: C:\Users\User\AppData\Local\Temp\RtmpCSJ6Gc
#' # - items:
#' #      Name Extension Rows Cols   Size LastModified
#' # 1 beaver1        NA  114    4 4.6 Kb         <NA>
#' # 2    iris        NA  150    5 7.1 Kb         <NA>
#' # 3  mtcars        NA   32   11   7 Kb         <NA>
#' 
#' # Clean up
#' lib_delete(dat)
#' @export
lib_sync <- function(x, name = NULL) {
  
  if (all(class(x) != "lib"))
    stop("Object must be a data library.")
  
  if (!is.null(name))
    libnm <- name
  else 
    libnm <- deparse1(substitute(x, env = environment()))
  
  if (is.loaded.lib(libnm) == TRUE) {
    
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
    
    assign(libnm, x, envir = e$env)
    
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
#' @return The new copy of the library.
#' @family lib
#' @examples
#' # Create temp directory
#' tmp <- tempdir()
#' 
#' # Create library
#' libname(dat1, tmp)
#' 
#' # Add dat to library
#' lib_add(dat1, mtcars)
#' lib_add(dat1, iris)
#' 
#' # Copy dat1 to dat2
#' lib_copy(dat1, dat2, file.path(tmp, "copy"))
#' 
#' # Examine dat2
#' dat2
#' # library 'dat2': 2 items
#' # - attributes: not loaded
#' # - path: C:\Users\User\AppData\Local\Temp\RtmpCSJ6Gc/copy
#' # - items:
#' #     Name Extension Rows Cols   Size        LastModified
#' # 1 mtcars       rds   32   11 7.5 Kb 2020-11-05 21:14:54
#' # 2   iris       rds  150    5 7.5 Kb 2020-11-05 21:14:54
#' 
#' # Clean up
#' lib_delete(dat1)
#' lib_delete(dat2)
#' @export
lib_copy <- function(x, nm, directory_path) {
  
  if (all(class(x) != "lib"))
    stop("Object must be a data library.")
  
  if (!dir.exists(directory_path))
    dir.create(directory_path)
  
  libnm <- deparse1(substitute(x, env = environment()))

  
  if (attr(x, "loaded") == TRUE) {
    
    x <- lib_sync(x, name = libnm) 
  }
  
  newlib <- deparse1(substitute(nm, env = environment()))
  
  cpy <- x
  
  attr(cpy, "name") <- newlib
  attr(cpy, "path") <- directory_path
  attr(cpy, "loaded") <- FALSE

  nms <- names(cpy)

  # Write out data
  for (nm in nms) {
    
    styp <- attr(cpy[[nm]], "extension")
    if (!is.null(attr(cpy, "type")))
      ext <- attr(cpy, "type")
    else if (!is.null(styp) & !is.na(styp))
      ext <- styp
    else
      ext <- "rds"
    
    attr(cpy[[nm]], "extension") <- ext
    
    fp <- file.path(directory_path, paste0(nm, ".", ext))
    
    writeData(cpy[[nm]], ext, fp)  
  }
  
  assign(newlib, cpy, envir = e$env)
  
  return(cpy)
}



#' @title Delete a Data Library
#' @description The \code{lib_delete} function deletes a data library from
#' the file system.  All data files associated with the library will be deleted.
#' If other files exist in the library directory, they will not be affected
#' by the delete operation.  The directory that contains the data will also
#' not be affected by the delete operation.  To delete the data directory, 
#' use the \code{\link[base]{unlink}} function or other packaged functions.
#' @param x The data library to delete.
#' @family lib
#' @examples 
#' # Create temp directory
#' tmp <- tempdir()
#' 
#' # Create library
#' libname(dat, tmp)
#' 
#' # Add data to library
#' lib_add(dat, mtcars)
#' lib_add(dat, iris)
#' 
#' # Load library
#' lib_load(dat)
#' 
#' # Examine workspace
#' ls()
#' # [1] "dat" "dat.iris" "dat.mtcars" "tmp"
#' 
#' # Examine library
#' dat
#' # library 'dat': 2 items
#' # - attributes: not loaded
#' # - path: C:\Users\User\AppData\Local\Temp\RtmpCSJ6Gc
#' # - items:
#' #     Name Extension Rows Cols   Size        LastModified
#' # 1 mtcars       rds   32   11 7.5 Kb 2020-11-05 21:18:17
#' # 2   iris       rds  150    5 7.5 Kb 2020-11-05 21:18:17
#' 
#' # Delete library
#' lib_delete(dat)
#' 
#' #' # Examine workspace again
#' ls()
#' # [1] "tmp"
#' @export
lib_delete <- function(x) {
  
  if (all(class(x) != "lib"))
    stop("Object must be a data library.")
  
  lnm <- deparse1(substitute(x, env = environment()))
  
  if (attr(x, "read_only") == FALSE) {
  
    if (is.loaded.lib(lnm))
      lib_unload(x, TRUE, lnm)
    
    lbpth <- attr(x, "path")
    
    for (nm in names(x)) {
      pth <- file.path(lbpth, paste0(nm, ".", attr(x[[nm]], "extension")))
      
      if (file.exists(pth))
        file.remove(pth)
      x[[nm]] <- NULL
    }
    
    rm(list = lnm, envir = e$env)
  
  } else {
    
   stop(paste0("Cannot delete library '", lnm, "' because it is read-only."))
    
  }
  
}

#' @title Get the Path for a Data Library
#' @description The \code{lib_path} function returns the current path of the 
#' the library as a string.
#' @param x The data library.
#' @return The path of the data library as a single string.
#' @family lib
#' @examples 
#' # Create temp directory
#' tmp <- tempdir()
#' 
#' # Create library
#' libname(dat, tmp)
#' 
#' # Examine library path
#' lib_path(dat)
#' # [1] "C:\\Users\\User\\AppData\\Local\\Temp\\RtmpCSJ6Gc"
#' 
#' # Clean up
#' lib_delete(dat)
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
#' @examples 
#' # Create temp directory
#' tmp <- tempdir()
#' 
#' # Create library
#' libname(dat, tmp)
#' 
#' # Add some data to library
#' lib_add(dat, mtcars)
#' lib_add(dat, iris)
#' 
#' # Check size of library
#' lib_size(dat)
#' # [1] 9757
#' 
#' # Clean up
#' lib_delete(dat)
#' @export
lib_size <- function(x) {
  
  if (all(class(x) != "lib"))
    stop("Object must be a data library.")
  
  path <- attr(x, "path")
  type <- attr(x, "type")
  
  if (is.null(type)) 
    info <- file.info(list.files(path, full.names = TRUE))
  else 
    info <- file.info(list.files(path, full.names = TRUE, pattern = type))
  
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
#' # Get library information
#' info <- lib_info(dat)
#' 
#' # Examine info
#' info
#' #      Name Extension Rows Cols   Size        LastModified
#' # 1 beaver1       rds  114    4 5.3 Kb 2020-11-05 21:27:57
#' # 2   rocks       rds   48    4 3.1 Kb 2020-11-05 21:27:56
#' # 3   trees       rds   31    3 2.4 Kb 2020-11-05 21:27:56
#' 
#' # Clean up
#' lib_delete(dat)
#' @export
lib_info <- function(x) {
  
  if (all(class(x) != "lib"))
    stop("Object must be a data library.")

  ret <- NULL
  
  libpth <- attr(x, "path")

  for (nm in names(x)) {
    
    itm <- x[[nm]]
    
    pth <- file.path(libpth, paste0(nm, ".", attr(itm, "extension")))
    #print(pth)
    
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
    
    if (any(class(itm) %in% "data.frame")) {  
      rws <- nrow(itm)
      cls <- ncol(itm)
    } else if (is.vector(itm) | is.atomic(itm) | is.list(itm)) {
      rws <- length(itm)
      cls <- 1
    } else {
      rws <- 0
      cls <- 0 
    }
    
    rw <- data.frame(Name = nm, 
                     Extension = ex,
                     Rows = rws,
                     Cols = cls,
                     Size = format(object.size(itm), units = "auto"),
                     LastModified = lm)
    
    if (is.null(ret))
      ret <- rw
    else 
      ret <- rbind(ret, rw)
    
  }
  
  return(ret)
}


# Setting a new environment causes a lot of trouble, and would take
# significant changes to make it work properly.  Will eliminate for now
# and consider for a future version.  That means the global environment is 
# the only environment available for now.

# @title Set the Environment for the Library Functions
# @description The \code{lib_env} function sets the working environment
# for the \strong{libr} package.  By default, the environment is set to the
# current global environment.  If an alternative environment is desired, a
# user may set it with this function. You may also set the environment
# with the option 'libr.env'.
# @param env The environment to use.
# @return The new environment.
# @family lib
# @examples
# # Create new environment
# env1 <- new.env()
# 
# # Assign new environment
# lib_env(env1)
# 
# # Create temp directory
# tmp <- tempdir()
# 
# # Create library
# libname(dat, tmp)
# 
# # Add data to library
# lib_add(dat, mtcars)
# lib_add_dat, iris)
# 
# # Load library
# lib_load(dat)
# 
# # Examine global environment
# ls()
# # [1] "env1" "tmp"
# 
# # Examine new environment
# ls(envir = env1)
# 
# # Clean up
# lib_delete(dat)
# @export
# lib_env <- function(env = NULL) {
#   
#   if (is.null(env)) {
#    if (is.null(options()[["libr.env"]])) {
#      if (is.null(e$env))
#        e$env = as.environment(1)
#    } else {
#      e$env = options()[["libr.env"]]
#    }
#     
#   } else {
#     e$env = env
#   }
#   
#   return(e$env)
# }
