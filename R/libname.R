# Globals -----------------------------------------------------------------

#' @noRd
e <- new.env(parent = emptyenv())
e$env <- parent.frame()


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
#' @param engine The engine to use to create the library.  The engine typically 
#' corresponds to the file extension of the input files. The
#' default value is 'rds'.
#' Valid values are 'rds', 'sas7bdat', 'xpt', 'xls', 'xlsx', 'dbf', and 'csv'. 
#' When saved with \code{lib_write}, each file will be saved in its original
#' file format, unless otherwise specified on the \code{engine} parameter of 
#' \code{lib_write}.
#' @param read_only Whether the library should be created as read only.
#' Default is FALSE.  If TRUE, the user will be restricted from
#' appending, removing, or writing data from the library to the file system.
#' @param ... Follow-on parameters to the data import functions.  Which
#' parameters exist depend on which types of files are being imported.
#' @param env The environment to use for the libname if it is loaded. 
#' Default is parent.frame().
#' @param import_specs A collection of import specifications to be used for import.
#' The items on the list should be named according to the file names in 
#' the library directory. This parameter is available for files of type
#' 'csv', 'xlsx', and 'xls'.  See the \code{\link{specs}} function for addtional
#' information.
#' @return The library object.
#' @family lib
#' @seealso \code{\link{spec}} to define import specifications for a library, 
#' and \code{\link{dictionary}} to view the data dictionary.
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
#' @import tibble
#' @export
libname <- function(name, directory_path, engine = "rds", 
                    read_only = FALSE, ..., env = parent.frame(), 
                    import_specs = NULL) {
  if (is.null(engine))
    stop("engine parameter cannot be null")
  
  if (length(engine) > 1)
    stop("engine parameter does not accept more than one value.")
  
  if (!engine %in% c("rds", "csv", "sas7bdat", "xlsx", "xls", "xpt", "dbf"))
    stop(paste0("Invalid engine parameter value: ", engine))
  
  if (!is.null(import_specs)) {
    if (!"specs" %in% class(import_specs))
      stop("import_specs parameter value must be of class 'specs'.")
    
  }
  
  # Set the environment to be used 
  e$env <- env
  
  # If directory doesn't exist, create it
  if (!file.exists(directory_path))
    dir.create(directory_path)
  
  # Get safe library name 
  name_c <- deparse1(substitute(name, env = environment()))
  

  # Create new structure of class "lib"
  l <- structure(list(), class = c("lib", "list"))
  
  # Set attributes for this library
  attr(l, "name") <- name_c
  attr(l, "path") <- directory_path
  attr(l, "read_only") <- read_only
  attr(l, "loaded") <- FALSE
  attr(l, "engine") <- engine
  attr(l, "import_specs") <- import_specs  
  

  # Get the file list according to the engine type
  lst <- list.files(directory_path, pattern = paste0("\\.", engine, "$"))
  
  for (fl in lst) {
    fp <- file.path(directory_path, fl)
    ext <-  getExtension(fl)
    nm <- getFileName(fl)
    
    if (length(ext) > 0) { 
      
      dat <- NULL
      
      if (ext == "csv") {
        message(paste0("$", nm))
        
        if (is.null(import_specs))
          dat <- read_csv(fp, ...)
        else {

          if (is.null(import_specs$specs[[nm]]))
            dat <- read_csv(fp, ...)
          else {
            spcs <- get_colspec_csv(import_specs$specs[[nm]]$col_types)
           # print(spcs)
            na <- import_specs$specs[[nm]]$na
            if (is.null(na))
              na = c("", "NA")
            tws <- import_specs$specs[[nm]]$trim_ws
            if (is.null(tws))
              tws <- TRUE
            
            dat <- read_csv(fp, ..., 
                            col_types = spcs,
                            na = na,
                            trim_ws = tws)
          }
        }
        
      } else if (ext == "rds") {
        
        dat <- read_rds(fp, ...)
        
      } else if (ext == "sas7bdat") {
        
        dat <- read_sas(fp, ...)
        
      } else if (ext == "dbf") {
        
        dat <- foreign::read.dbf(fp, ...)
        if (!is_tibble(dat))
          dat <- as_tibble(dat)
        
      } else if (ext == "xpt") {
        
        dat <- read_xpt(fp, ...)
        
      } else if (ext == "xlsx") {
        
        message(paste0("$", nm))
        
        if (is.null(import_specs))
          dat <- read_xlsx(fp, ...)
        else {
          if (is.null(import_specs$specs[[nm]]))
            dat <- read_xlsx(fp, ...)
          else {
            
            typs <- import_specs$specs[[nm]]$col_types
            tmp <- read_xlsx(fp, ...,
                             col_types = c("text"))
            nms <- names(tmp)
            spcs <- get_colspec_xlsx(typs, length(nms), nms)
            na <- import_specs$specs[[nm]]$na
            if (is.null(na))
              na = c("", "NA")
            tws <- import_specs$specs[[nm]]$trim_ws
            if (is.null(tws))
              tws <- TRUE
            
            dat <- read_xlsx(fp, ..., 
                             col_types = spcs, 
                             na = na, 
                             trim_ws = tws)
          }
          
        }
        
      } else if (ext == "xls") {
        
        message(paste0("$", nm))
        
        if (is.null(import_specs))
          dat <- read_xls(fp, ...)
        else {
          if (is.null(import_specs$specs[[nm]]))
            dat <- read_xls(fp, ...)
          else {
            
            typs <- import_specs$specs[[nm]]$col_types
            tmp <- read_xls(fp, ...,
                             col_types = c("text"))
            nms <- names(tmp)
            spcs <- get_colspec_xlsx(typs, length(nms), nms)
            na <- import_specs$specs[[nm]]$na
            if (is.null(na))
              na = c("", "NA")
            tws <- import_specs$specs[[nm]]$trim_ws
            if (is.null(tws))
              tws <- TRUE
            
            dat <- read_xls(fp, ..., 
                             col_types = spcs, 
                             na = na, 
                             trim_ws = tws)
          }
          
        }
      } 
      
      if (any(class(dat) == "data.frame")) {
      
        if (nm %in% names(l))
          warning(paste("The name", nm, "already exists in the library.",
                        "Data will be replaced."))
        
        # Set attributes on data frame
        attr(dat, "name") <- nm
        attr(dat, "extension") <- ext
        attr(dat, "path") <- fp
        attr(dat, "checksum") <- md5sum(fp)

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
  
  # Set loaded attribute
  attr(x, "loaded") <- TRUE
  
  # Reassign with current attributes
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
  
  if (!is.null(name)) {
   if (!"character" %in% class(name))
     stop("Name must be of class character")
  }
  
  # Get name of library
  libnm <- deparse1(substitute(x, env = environment())) 
  
  # Use override name if requested
  if (!is.null(name))
    libnm <- name
  
  # Sync if requested
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
  
  # Reassign with updated attributes
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
#' @import tools
#' @export
lib_add <- function(x, ..., name = NULL) {
  
  if (all(class(x) != "lib"))
    stop("Object must be a data library.")
  
  lbnm  <- deparse1(substitute(x, env = environment()))
  
  if (attr(x, "read_only") == FALSE) {
    
    # Get safe data names
    nms <- as.character(substitute(c(...), env = environment()))
    nms <- nms[2:length(nms)]  
    
    # Get straight parameter list
    lst <- list(...)
    
    # Use override name if requested
    if (!is.null(name)) {
      nms <- name
    } 
    
    # Get engine type
    if (!is.null(attr(x, "engine")))
      typ <- attr(x, "engine")
    else 
      typ <- "rds"
    
    
    i <- 1
    for (nm in nms) {
      
      # Check for duplicate names
      if (nm %in% names(x))
        warning(paste0("The name '", nm, "' already exists in the library '",
                      lbnm, "'. Data will be replaced."))
      
      # Assign parameter value
      x[[nm]] <- lst[[i]]
      
      # Assign attributes
      attr(x[[nm]], "name") <- nm
      attr(x[[nm]], "extension") <- typ
      
      # If lib is loaded, add to environment
      if (is.loaded.lib(lbnm)) {
        
        assign(paste0(lbnm, ".", nm), x[[nm]], envir = e$env)
      }
      
      # Construct path
      pth <- file.path(attr(x, "path"), paste0(nm, ".", typ))
      
      # Write df to file system
      x[[nm]] <- writeData(x[[nm]], typ, pth)
    
      i <- i + 1
    }
    
    # Reassign updated attributes
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
lib_replace <- function(x, ...,  name = NULL) {
  
  if (all(class(x) != "lib"))
    stop("Object must be a data library.")
  
  # Get safe variable name
  lbnm  <- deparse1(substitute(x, env = environment()))
  
  if (attr(x, "read_only") == FALSE) {
    
    # Get safe parameters
    nms <- as.character(substitute(c(...), env = environment()))
    nms <- nms[2:length(nms)]  
    
    # Get actual parameters
    lst <- list(...)
    
    # Use override name if requested
    if (!is.null(name)) {
      nms <- name
    } 
    
    # Get engine type
    if (!is.null(attr(x, "engine")))
      typ <- attr(x, "engine")
    else 
      typ <- "rds"
    
    
    i <- 1
    for (nm in nms) {
      
      # Trap duplicate names
      if (!nm %in% names(x))
        warning(paste0("The name '", nm, "' does not exist in the library '",
                       lbnm, "'. Data will be added."))
      
      # Assign dataset 
      x[[nm]] <- lst[[i]]
      
      # Update attributes
      attr(x[[nm]], "name") <- nm
      attr(x[[nm]], "extension") <- typ
      
      # If lib is loaded, assign data set to environment
      if (is.loaded.lib(lbnm)) {
        
        assign(paste0(lbnm, ".", nm), x[[nm]], envir = e$env)
      }
      
      # Construct path
      pth <- file.path(attr(x, "path"), paste0(nm, ".", typ))
      
      # Write data to file system
      x[[nm]] <- writeData(x[[nm]], typ, pth)
      
      i <- i + 1
    }
    
    # Update variable in environment
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
  
  # Get safe names
  libnm <- deparse1(substitute(x, env = environment()))
  
  # Get file path
  libpth <- attr(x, "path")
  
  if (attr(x, "read_only") == FALSE) {
    for (nm in name) {
      
      # Construct file path
      pth <- file.path(libpth, paste0(nm, ".", attr(x[[nm]], "extension")))
      
      # Remove file
      if (!is.null(pth)) {
        if (file.exists(pth))
          file.remove(pth)
      }
      
      # Clear out item from list
      x[[nm]] <- NULL
      
      # Remove variable from environment
      if (is.loaded.lib(libnm)) {
        gnm <- paste0(libnm, ".", nm)
        rm(list = gnm, envir = e$env)
      }
    }
    
    # Update library
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
#' from a file will be saved in RDS format, unless otherwise specified.  
#' 
#' The \strong{libr} package can write data files in csv, rds, xlsx, sas7bdat,
#' and a plain text file format.  Data read in xls file format will be saved 
#' as an xlsx.
#' 
#' By default, the \code{lib_write} function will not write data that has 
#' not changed.  Prior to writing a file, \code{lib_write} will compare the 
#' data in memory to the data on disk.  If there are differences in the data,
#' the function will overwrite the version on disk.  To override the default
#' behavior, use the \code{force} option to force \code{lib_write} to write
#' every data file to disk.
#' 
#' @param x The format catalog to write.
#' @param force Force writing each data file to disk, even if it has not 
#' changed.
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
lib_write <- function(x, force = FALSE) {
  
  if (all(class(x) != "lib"))
    stop("Object must be a data library.")
  
  # Get safe name
  lbnm <- deparse1(substitute(x, env = environment()))
  
  if (attr(x, "read_only") != TRUE) { 
  
    # Sync with list if needed
    if (is.loaded.lib(lbnm)) {
      x <- lib_sync(x, lbnm)
    }
      
    # Get data names
    nms <- names(x)
    
    # Get path
    libpth <- attr(x, "path")
    
    for (nm in nms) {
      
      # Figure out the extension
      styp <- attr(x[[nm]], "extension")
      if (!is.null(attr(x, "engine")))
        ext <- attr(x, "engine")
      else if (!is.null(styp) && !is.na(styp))
        ext <- styp
      else
        ext <- "rds"
      
      # Assign extension attribute
      attr(x[[nm]], "extension") <- ext
      
      # Construct the path
      fp <- file.path(libpth, paste0(nm, ".", ext))
  
      # Write data to the file system
      x[[nm]] <- writeData(x[[nm]], ext, fp, force)  
      
    }
    
    # Update the library variable
    assign(lbnm, x, envir = e$env)
  
  } else {
    
    stop(paste0("Cannot write to library '", lbnm, "' because it is read-only."))
  }
  
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
  
  # Use name if requested
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
    
    # Get list of variables from environment
    en <- ls(envir = e$env)
    
    # Filter by what belongs to library
    fen <- grep(paste0("^", libnm, "\\."), en, value = TRUE)

    for (gnm in fen) {
      
      # Remove lib prefix to get clean name
      nm <- sub(paste0(libnm, "."), "", gnm, fixed = TRUE)
      
      # Pull from environment and add to list
      x[[nm]] <- get(gnm, envir = e$env)
      
    }
    
    # Update lib variable
    assign(libnm, x, envir = e$env)
    
  } else {
    
    warning(paste0("Library '", libnm, "' is not loaded")) 
    
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
  
  # Create target directory if it doesn't exist
  if (!dir.exists(directory_path))
    dir.create(directory_path)
  
  # Get safe lib name
  libnm <- deparse1(substitute(x, env = environment()))

  # Sync with list if needed
  if (attr(x, "loaded") == TRUE) {
    
    x <- lib_sync(x, name = libnm) 
  }
  
  # Get safe name of new library
  newlib <- deparse1(substitute(nm, env = environment()))
  
  # Copy lib
  cpy <- x
  
  # Set attributes
  attr(cpy, "name") <- newlib
  attr(cpy, "path") <- directory_path
  attr(cpy, "loaded") <- FALSE

  # Get list of dataset names
  nms <- names(cpy)

  # Write out data
  for (nm in nms) {
    
    # Figure out the extension
    styp <- attr(cpy[[nm]], "extension")
    if (!is.null(attr(cpy, "engine")))
      ext <- attr(cpy, "engine")
    else if (!is.null(styp) & !is.na(styp))
      ext <- styp
    else
      ext <- "rds"
    
    # Update attributes
    attr(cpy[[nm]], "extension") <- ext
    
    # Construct path
    fp <- file.path(directory_path, paste0(nm, ".", ext))
    
    # Write copied data to file system
    cpy[[nm]] <- writeData(cpy[[nm]], ext, fp)  
    

  }
  
  # Update environment
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
  
  # Get safe name
  lnm <- deparse1(substitute(x, env = environment()))
  
  if (attr(x, "read_only") == FALSE) {
  
    # Unload if needed
    if (is.loaded.lib(lnm))
      lib_unload(x, TRUE, lnm)
    
    # Get path of lib
    lbpth <- attr(x, "path")
    
    # Remove each file in lib
    for (nm in names(x)) {
      pth <- file.path(lbpth, paste0(nm, ".", attr(x[[nm]], "extension")))
      
      if (file.exists(pth))
        file.remove(pth)
      x[[nm]] <- NULL
    }
    
    # Remove variables from environment
    if (lnm %in% ls(envir = e$env))
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
  
  # Return saved path
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
  
  # Get the lib type and path
  path <- attr(x, "path")
  type <- attr(x, "engine")

  # Get info for all files in lib
  info <- file.info(list.files(path, full.names = TRUE, 
                               pattern =  paste0("\\.", type, "$")))
  # Calculate size
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
  
  # Get lib path
  libpth <- attr(x, "path")

  for (nm in names(x)) {
    
    # Put each data set into own variable for convenience
    itm <- x[[nm]]
    
    # Construct path for each file
    pth <- file.path(libpth, paste0(nm, ".", attr(itm, "extension")))
    
    # Get file info
    if (!is.null(pth)) { 
      info <- file.info(pth)
      lm <- info[1, "mtime"]
    } else {
      lm <- NA
    }
    
    # Get saved extension
    if (is.null(attr(itm, "extension"))) {
      ex <- NA 
    } else {
      ex <- attr(itm, "extension") 
    }
    
    # Usually the data is a data frame.  Sometimes not.
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
    
    # Construct row of info to present to user
    rw <- data.frame(Name = nm, 
                     Extension = ex,
                     Rows = rws,
                     Cols = cls,
                     Size = format(utils::object.size(itm), units = "auto"),
                     LastModified = lm)
    
    # Bind the info rows for each file
    if (is.null(ret))
      ret <- rw
    else 
      ret <- rbind(ret, rw)
    
  }
  
  return(ret)
}

