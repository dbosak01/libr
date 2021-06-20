# Globals -----------------------------------------------------------------

#' @noRd
e <- new.env(parent = emptyenv())
e$env <- parent.frame()


# Libname Definition ------------------------------------------------------


#' @title Create a data library
#' @encoding UTF-8
#' @description A data library is a collection of data sets. The purpose of 
#' the data library is to combine related data sets, and provides the opportunity
#' to manipulate all of them as a single object. A data library is created using 
#' the \code{libname} function. The \code{libname} function allows you to 
#' load an entire directory of data into memory in one step.  The \strong{libr}
#' package contains additional functions to add and remove data from the library, 
#' copy the library, and write any changed data to the file system.
#' @details  
#' For most projects, a data file does not exist in isolation.  There are sets of 
#' related files of the same file type.  The aim of the \code{libname} function
#' is to take advantage of this fact, and give you an easy way to manage
#' the entire set.
#' 
#' The \code{libname} function points to a directory of data files, and associates
#' a name with that set of data.  The name refers to an object of class 'lib', 
#' which at its heart is a named list.  When the \code{libname} function 
#' executes, it will load all the data in the directory into the list, and assign
#' the file name (without extension) as the list item name. Data can be accessed
#' using list syntax, or loaded directly into the local environment using the 
#' \code{\link{lib_load}} function.
#' 
#' The \code{libname} function provides several data engines to read
#' data of different types.  For example, there is an engine for Excel 
#' files, and another engine for SAS® datasets.  The engines are identified
#' by the extension of the file type they handle.  The available engines are 
#' 'rds', 'csv', 'xlsx', 'xls', 'sas7bdat', 'xpt', and 'dbf'.
#' Once an engine has been assigned to a library, all other read/write 
#' operations will be performed by that engine.  
#' 
#' The data engines largely hide file import details from you.  
#' The purpose of the \code{libname} function is to make it easy to 
#' import a set of related data files that follow standard conventions.  
#' The function assumes that the 
#' data has file extensions that match the file type, and then makes further
#' assumptions based on each type of file.  As a result, there are very few 
#' import options on the \code{libname} function.  If your data does not
#' follow standard conventions, it is recommended that you import your 
#' data using a package that gives you more control over import options. 
#' 
#' @section Import Specifications:
#' In most cases, it is not necessary to specify the data types for incoming
#' columns in your data.  Either the file format will preserve the appropriate
#' data type information, or the assigned engine will guess correctly.  
#' 
#' However, in some cases it will be necessary to control the column data types.
#' For these cases, use the  
#' \code{import_specs} parameter.  The \code{import_specs} parameter allows you
#' to specify the data types by data set and column name. All the data type
#' specifications are contained within a \code{specs} collection, and the 
#' specifications for a particular data set are defined by an 
#' \code{import_spec} function. See the \code{\link{specs}} and 
#' \code{\link{import_spec}} documentation for further information
#' and examples of defining an import spec.
#' 
#' @section Data Engines:
#' The \code{libname} function currently provides seven different engines for 
#' seven different types of data files.
#' Here is a complete list of available engines and some commentary about each:
#' \itemize{
#' \item{\strong{rds}: For R data sets.  This engine is the default.  Because
#' detailed data type and attribute information can be stored inside the rds
#' file, the rds engine is the most reliable and easiest to use.}
#' \item{\strong{csv}: For comma separated value files.  This engine assumes
#' that the first row has column names, and that strings containing commas are 
#' quoted.  Blank values and the string 'NA' will be interpreted as NA.
#' Because data type information is not stored in csv files, the 
#' csv engine will attempt to guess the data types based on the available data.
#' For most columns, the csv engine is able to guess accurately. Where
#' it fails most commonly is with date and time columns. For csv date and time
#' columns, it is therefore recommended to assign an import spec that tells
#' the engine how to read the date or time. See the \code{\link{specs}}
#' documentation for additional details.}
#' \item{\strong{xlsx}: For Excel files produced with the current version 
#' of Excel.  Excel provides more data type information than csv, but it 
#' is not as accurate as rds.  Therefore, you may also need to provide 
#' import specifications with Excel files. Also note that currently 
#' the xlsx import engine will only import the first sheet of an Excel workbook.  
#' If you need to import a sheet that is not the first sheet, use a 
#' different package to import the data.}
#' \item{\strong{xls}: An Excel file format used between 1997 and 2003, and 
#' still used in some organizations. As with xlsx, this file format provides more 
#' information than csv, but is not entirely reliable. Therefore, you may need 
#' to provide import specifications to the xls engine. Also note that the xls
#' engine can read, but not write xls files.  Any xls files read with the
#' xls engine will be written as an xlsx file. Like the xlsx engine, the xls
#' engine can only read the first sheet of a workbook.}
#' \item{\strong{sas7bdat}: Handles SAS® datasets. SAS® datasets provide better
#' type information than either csv or Excel.  In most cases, you will not 
#' need to define import specifications for SAS® datasets.  The sas7bdat engine
#' interprets empty strings, single blanks, and a single dot (".") as missing
#' values. While the import of SAS® datasets is fairly reliable, sas7bdat files 
#' exported with the sas7bdat engine sometimes cannot be read by SAS® software.
#' In these cases, it is recommended to export to another file format, such
#' as csv or dbf, and then import into SAS®.}
#' \item{\strong{xpt}: The SAS® transport file engine.  Transport format is 
#' a platform independent file format.  Similar to SAS® datasets, it 
#' provides data type information.  In most cases, you will not need to 
#' define import specifications.  The xpt engine also interprets empty strings, 
#' single blanks, and a single dot (".") as missing values.}
#' \item{\strong{dbf}: The DBASE file format engine.  The DBASE engine
#' was added to the \strong{libr} package because many types of software can
#' read and write in DBASE format reliably.  Therefore it is a useful 
#' file format for interchange between software systems.  The DBASE file 
#' format contains type information.}
#' }
#' @section File Filters:
#' If you wish to import only a portion of your data files into a library, 
#' you may accomplish it with the \code{filter} parameter.  The filter 
#' parameter allows you to pass a vector of strings corresponding to the 
#' names of the files you want to import.  The function allows a 
#' wild-card (\*) for partial matching.  For example, \code{"te\*"} means any
#' file name that that begins with a "te", and \code{"\*st"} means any file name
#' that ends with an "st". 
#' 
#' @param name The unquoted name of the library to create.  The library name will 
#' be created as a variable in the environment specified on the \code{env}
#' parameter.  The default environment is the parent frame.
#' @param directory_path A directory path to associate with the library.  If 
#' the directory contains data files of the type specified on the 
#' \code{engine} parameter, they will be imported into the library list.  If
#' the directory does not contains data sets of the appropriate type, 
#' it will be created as an empty library. If the directory does not exist, 
#' it will be created by the \code{libname} function.
#' @param engine The engine to associate with the library.  The specified 
#' engine will be used to import and export data.  The engine name 
#' corresponds to the standard file extension of the data file type. The
#' default engine is 'rds'.
#' Valid values are 'rds', 'sas7bdat', 'xpt', 'xls', 'xlsx', 'dbf', and 'csv'. 
#' @param read_only Whether the library should be created as read-only.
#' Default is FALSE.  If TRUE, the user will be restricted from
#' appending, removing, or writing any data from memory to the file system.
#' @param env The environment to use for the libname. 
#' Default is \code{parent.frame()}.  When working inside a function, the 
#' \code{parent.frame()} will refer to the local function scope.  When
#'  working outside a function, the \code{parent.frame()} will be the
#'  global environment. If the \code{env} parameter is set to a custom 
#'  environment, the custom environment will be used for all subsequent 
#'  operations with that libname.   
#' @param import_specs A collection of import specifications, 
#' defined using the \code{\link{specs}} function.
#' The import specs should be named according to the file names in 
#' the library directory. See the \code{\link{specs}} function for additional
#' information.
#' @param filter One or more quoted strings to use as filters for the incoming 
#' file names. For more than one filter string, pass them as a vector of
#' strings. The filter string can be a full or partial file name, without
#' extension.  If using a partial file name, use a wild-card character (*) 
#' to identify the missing portion. The match will be case-insensitive.
#' @return The library object, with all data files loaded into the library
#' list.  Items in the list will be named according the the file name,
#' minus the file extension.
#' @family lib
#' @seealso \code{\link{specs}} to define import specifications, 
#' and \code{\link{dictionary}} to view the data dictionary for a library.
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
#' # # library 'dat': 3 items
#' # - attributes: rds not loaded
#' # - path: C:\Users\User\AppData\Local\Temp\RtmpklJcfl
#' # - items:
#' #      Name Extension Rows Cols   Size        LastModified
#' # 1 beaver1       rds  114    4 5.9 Kb 2020-12-06 15:21:30
#' # 2   rocks       rds   48    4 3.6 Kb 2020-12-06 15:21:30
#' # 3   trees       rds   31    3 2.9 Kb 2020-12-06 15:21:30
#' 
#' # Print dictionary for library
#' dictionary(dat)
#' # A tibble: 11 x 10
#' #    Name    Column Class   Label Description Format Width Justify  Rows   NAs
#' #    <chr>   <chr>  <chr>   <chr> <chr>       <lgl>  <lgl> <chr>   <int> <int>
#' #  1 beaver1 day    numeric NA    NA          NA     NA    NA        114     0
#' #  2 beaver1 time   numeric NA    NA          NA     NA    NA        114     0
#' #  3 beaver1 temp   numeric NA    NA          NA     NA    NA        114     0
#' #  4 beaver1 activ  numeric NA    NA          NA     NA    NA        114     0
#' #  5 rocks   area   integer NA    NA          NA     NA    NA         48     0
#' #  6 rocks   peri   numeric NA    NA          NA     NA    NA         48     0
#' #  7 rocks   shape  numeric NA    NA          NA     NA    NA         48     0
#' #  8 rocks   perm   numeric NA    NA          NA     NA    NA         48     0
#' #  9 trees   Girth  numeric NA    NA          NA     NA    NA         31     0
#' # 10 trees   Height numeric NA    NA          NA     NA    NA         31     0
#' # 11 trees   Volume numeric NA    NA          NA     NA    NA         31     0
#' 
#' # Load library into workspace 
#' lib_load(dat)
#' 
#' # Print summaries for each data frame
#' # Note that once loaded into the workspace, 
#' # data can be accessed using two-level syntax.
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
                    read_only = FALSE, env = parent.frame(), 
                    import_specs = NULL, filter = NULL) {
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
  if (!dir.exists(directory_path))
    dir.create(directory_path, showWarnings = FALSE)
  
  # Get safe library name 
  name_c <- paste(deparse(substitute(name, env = environment())), collapse = "")
  

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
  
  if (!is.null(filter))
    lst <- dofilter(filter, lst, engine)
  
  for (fl in lst) {
    fp <- file.path(directory_path, fl)
    ext <-  getExtension(fl)
    nm <- getFileName(fl)
    
    if (length(ext) > 0) { 
      
      dat <- NULL
      
      if (ext == "csv") {
        message(paste0("$", nm))
        
        if (is.null(import_specs))
          dat <- read_csv(fp)
        else {

          if (is.null(import_specs$specs[[nm]]))
            dat <- read_csv(fp)
          else {
            spcs <- get_colspec_csv(import_specs$specs[[nm]]$col_types)
           # print(spcs)
            na <- import_specs$specs[[nm]]$na
            if (is.null(na))
              na = import_specs$na
            tws <- import_specs$specs[[nm]]$trim_ws
            if (is.null(tws))
              tws <- import_specs$trim_ws
            
            dat <- read_csv(fp, 
                            col_types = spcs,
                            na = na,
                            trim_ws = tws)
          }
        }
        
      } else if (ext == "rds") {
        
        dat <- read_rds(fp)
        if (!is.null(import_specs))
          dat <- exec_spec(dat, import_specs, nm)
        
      } else if (ext == "sas7bdat") {
        
        dat <- read_sas(fp)
        if (!is.null(import_specs))
          dat <- exec_spec(dat, import_specs, nm)
        else {
          spcs <- specs(na = c("", "."))
          dat <- exec_spec(dat, spcs, nm)
          
        }
        
      } else if (ext == "dbf") {
        
        dat <- foreign::read.dbf(fp)
        if (!is_tibble(dat))
          dat <- as_tibble(dat)
        
        if (!is.null(import_specs))
          dat <- exec_spec(dat, import_specs, nm)
        
      } else if (ext == "xpt") {
        
        dat <- read_xpt(fp)
        if (!is.null(import_specs))
          dat <- exec_spec(dat, import_specs, nm)
        else {
          spcs <- specs(na = c("", "."))
          dat <- exec_spec(dat, import_specs, nm)
          
        }
        
      } else if (ext == "xlsx") {
        
        message(paste0("$", nm))
        
        if (is.null(import_specs))
          dat <- read_xlsx(fp)
        else {
          if (is.null(import_specs$specs[[nm]]))
            dat <- read_xlsx(fp)
          else {
            
            typs <- import_specs$specs[[nm]]$col_types
            tmp <- read_xlsx(fp,
                             col_types = c("text"))
            nms <- names(tmp)
            spcs <- get_colspec_xlsx(typs, length(nms), nms)
            na <- import_specs$specs[[nm]]$na
            if (is.null(na))
              na = import_specs$na
            tws <- import_specs$specs[[nm]]$trim_ws
            if (is.null(tws))
              tws <- import_specs$trim_ws
            
            dat <- read_xlsx(fp, 
                             col_types = spcs, 
                             na = na, 
                             trim_ws = tws)
          }
          
        }
        
      } else if (ext == "xls") {
        
        message(paste0("$", nm))
        
        if (is.null(import_specs))
          dat <- read_xls(fp)
        else {
          if (is.null(import_specs$specs[[nm]]))
            dat <- read_xls(fp)
          else {
            
            typs <- import_specs$specs[[nm]]$col_types
            tmp <- read_xls(fp,
                             col_types = c("text"))
            nms <- names(tmp)
            spcs <- get_colspec_xlsx(typs, length(nms), nms)
            na <- import_specs$specs[[nm]]$na
            if (is.null(na))
              na = import_specs$na
            tws <- import_specs$specs[[nm]]$trim_ws
            if (is.null(tws))
              tws <- import_specs$trim_ws
            
            dat <- read_xls(fp, 
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
  
  log_logr(l)
  
  return(l)
  
}


# Manipulation Functions --------------------------------------------------


#' @title Load a Library into the Workspace
#' @description The \code{lib_load} function loads a data library into
#' an environment. The environment used is associated with the library at 
#' the time it is created with the \code{\link{libname}} function.  
#' When the \code{lib_load} function is called, the data frames/tibbles 
#' will be loaded with <library>.<data set> syntax.  Loading the data frames
#' into the environment makes them easy to access and use in your program.
#' @param x The data library to load.
#' @param filter One or more quoted strings to use as filters for the  
#' data names to load into the workspace. For more than one filter string, 
#' pass them as a vector of strings. The filter string can be a full or 
#' partial name.  If using a partial name, use a wild-card character (*) 
#' to identify the missing portion. The match will be case-insensitive.
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
lib_load <- function(x, filter = NULL) {
  
  if (all(class(x) != "lib"))
    stop("Object must be a data library.")
  
  # Get library name
  libnm <- paste(deparse(substitute(x, env = environment())), collapse = "")
  
  if (is.null(filter))
    nms <- names(x)
  else 
    nms <- dofilter(filter, names(x))
  
  # For each name in library, assign to global environment
  for (nm in nms) {
    n <-  paste0(libnm, ".", nm)
    assign(n, x[[nm]], envir = e$env)
  }
  
  # Set loaded attribute
  attr(x, "loaded") <- TRUE
  
  # Reassign with current attributes
  assign(libnm, x, envir = e$env)
  
  log_logr(paste0("lib_load: library '", libnm, "' loaded"))  
  
  return(x)
}


#' @title Unload a Library from the Workspace
#' @description The \code{lib_unload} function unloads a data library from
#' the workspace environment.  The unload function does not delete the data 
#' or remove the library.  It simply removes the data frames from working 
#' memory.  By default, the \code{lib_unload} function will also synchronize the 
#' data in working memory with the data stored in the library list, as these
#' two instances can become out of sync if you change the data in working memory.
#' @param x The data library to unload.
#' @param sync Whether to sync the workspace with the library list before
#' it is unloaded.  Default is TRUE. If you want to unload the workspace 
#' without saving the workspace data, set this parameter to FALSE.
#' @param name The name of the library to unload, if the name is different
#' than the variable name.  Used internally.
#' @return The unloaded data library.
#' @seealso \code{\link{lib_load}} to load the library.
#' @family lib
#' @examples 
#' # Create temp directory
#' tmp <- tempdir()
#' 
#' # Create library
#' libname(dat, tmp)
#' 
#' # Add data to library
#' lib_add(dat, iris, ToothGrowth, PlantGrowth)
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
  libnm <- paste(deparse(substitute(x, env = environment())), collapse = "") 
  
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
  
  log_logr(paste0("lib_unload: library '", libnm, "' unloaded"))  
  
  return(x)
}


#' @title Add Data to a Data Library
#' @description The \code{\link{lib_add}} function adds a data frame
#' or tibble to an existing data library.  The function will both add the data
#' to the library list, and immediately write the data to the library
#' directory location. The data will be written to disk
#' in the file format associated with the library engine. 
#' If the library is loaded, the function will also 
#' add the data to the workspace environment.   
#' @param x The library to add data to.
#' @param ... The data frame(s) to add to the library.  If more than one,
#' separate with commas.
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
#' # # library 'dat': 0 items
#' # - attributes: rds not loaded
#' # - path: C:\Users\User\AppData\Local\Temp\RtmpCSJ6Gc
#' # NULL
#' 
#' # Add data to the library
#' lib_add(dat, mtcars, beaver1, iris)
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
lib_add <- function(x, ..., name = NULL) {
  
  if (all(class(x) != "lib"))
    stop("Object must be a data library.")
  
  lbnm  <- paste(deparse(substitute(x, env = environment())), collapse = "")
  
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
    
    log_logr(paste0("lib_add: added data to library '", lbnm, "': ", 
                    paste(nms, collapse = " "))) 
    
  } else {
    
   stop(paste0("Cannot add to library '", lbnm, "' because it is read-only.")) 
  }
    
  
  return(x)
  
}


#' @title Replace Data in a Data Library
#' @description The \code{\link{lib_replace}} function replaces a data frame
#' in an existing data library.  The function will replace the data
#' in the library list, the data in the workspace (if loaded), 
#' and immediately write the new data to the library
#' directory location.  The data will be written in the file format
#' associated with the library engine. 
#' @param x The library to replace data in.
#' @param ... The data frame(s) to replace.  If you wish to replace more than
#' one data set, separate with commas.
#' @param name The reference name to use for the data.  By default,
#' the name will be the variable name.  To assign a name different
#' from the variable name, assign a quoted name to this parameter.  If more
#' than one data set is being replaced, assign a vector of quoted names.
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
#' # library 'dat': 3 items
#' # - attributes: not loaded
#' # - path: C:\Users\User\AppData\Local\Temp\RtmpCSJ6Gc
#' # - items:
#' #      Name Extension Rows Cols   Size        LastModified
#' # 1  mtcars       rds   32   11 7.5 Kb 2020-11-05 19:32:00
#' 
#' # Replace data with a subset
#' lib_replace(dat, mtcars[1:10, 1:5], name = "mtcars")
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
  lbnm  <- paste(deparse(substitute(x, env = environment())), collapse = "")
  
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
    
    log_logr(paste0("lib_replace: replaced data in library '", lbnm, "': ", 
                    paste(nms, collapse = " ")))
    
  } else {
    
    stop(paste0("Cannot replace in library '", lbnm, "' because it is read-only.")) 
  }
  
  
  return(x)
  
}

#' @title Remove Data from a Data Library
#' @description The \code{lib_remove} function removes an item from the 
#' data library, and deletes the source file for that data.  If the library
#' is loaded, it will also remove that item from the workspace environment.
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
#' lib_add(dat, mtcars, beaver1, iris)
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
  libnm <- paste(deparse(substitute(x, env = environment())), collapse = "")
  
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
    
    log_logr(paste0("lib_remove: removed data from library '", libnm, "': ", 
                    paste(name, collapse = " ")))
  
  } else {
    
    stop(paste0("Cannot remove from library '", libnm, "' because it is read-only.")) 
  }
  
  return(x)
}


#' @title Write a Data Library to the File System
#' @description The \code{lib_write} function writes the data library
#' to the file system.  The library will be written to the 
#' directory for which it was defined, and each data frame will be written
#' in the format associated with the library data engine.  See the 
#' \code{\link{libname}} function for further elaboration on the types
#' of engines available, and the assumptions/limitations of each.  
#' 
#' By default, the \code{lib_write} function will not write data that has 
#' not changed.  Prior to writing a file, \code{lib_write} will compare the 
#' data in memory to the data on disk.  If there are differences in the data,
#' the function will overwrite the version on disk.  To override the default
#' behavior, use the \code{force} option to force \code{lib_write} to write
#' every data file to disk.
#' 
#' @param x The data library to write.
#' @param force Force writing each data file to disk, even if it has not 
#' changed.
#' @return The saved data library.
#' @family lib
#' @examples 
#' # Create temp directory
#' tmp <- tempdir()
#' 
#' # Create library
#' libname(dat, tmp)
#' # # library 'dat': 0 items
#' # - attributes: rds not loaded
#' # - path: C:\Users\User\AppData\Local\Temp\RtmpCSJ6Gc
#' # NULL
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
#' # library 'dat': 3 items
#' # - attributes: rds not loaded
#' # - path: C:\Users\User\AppData\Local\Temp\RtmpCSJ6Gc
#' # - items:
#' #      Name Extension Rows Cols   Size LastModified
#' # 1 beaver1        NA  114    4 4.6 Kb         <NA>
#' # 2    iris        NA  150    5 7.1 Kb         <NA>
#' # 3  mtcars        NA   32   11   7 Kb         <NA>
#' 
#' # Write the library to the file system
#' lib_write(dat)
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
  lbnm <- paste(deparse(substitute(x, env = environment())), collapse = "")
  
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
    
    log_logr(paste0("lib_write: write data in library '", lbnm, "'"))
    log_logr(x)
  
  } else {
    
    stop(paste0("Cannot write to library '", lbnm, "' because it is read-only."))
  }
  
  return(x)
}


#' @title Synchronize Loaded Library
#' @description The \code{lib_sync} function synchronizes the data
#' loaded into the working environment with the data stored 
#' in the library list.  Synchronization is necessary only
#' for libraries that have been loaded into the working environment.
#' The function
#' copies data from the working environment to the library
#' list, overwriting any data in the list. The function is useful when 
#' you want to update the library list, but are not yet ready to 
#' unload the data from working memory.  
#' 
#' Note that the \code{lib_sync} function does not 
#' write any data to disk. Also note that the \code{lib_sync} function will
#' not automatically remove any variables from the library list that 
#' have been removed from the workspace.  To remove items from the library
#' list, use the \code{\link{lib_remove}} function.  To write data to 
#' disk, use the \code{\link{lib_write}} function.
#' @param x The data library to synchronize.
#' @param name The name of the library to sync if not the variable
#' name. Used internally.
#' @return The synchronized data library.
#' @family lib
#' @examples 
#' # Create temp directory
#' tmp <- tempdir()
#' 
#' # Create library
#' libname(dat, tmp)
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
    libnm <- paste(deparse(substitute(x, env = environment())), collapse = "")
  
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
    
    log_logr(paste0("lib_sync: synchronized data in library '", libnm, "'"))
    
  } else {
    
    warning(paste0("Library '", libnm, "' is not loaded")) 
    
  }
  
  return(x)
}


#' @title Copy a Data Library
#' @description The \code{lib_copy} function copies a data library.  The 
#' function accepts a library and a destination path.  If the destination 
#' path does not exist, the function will attempt to create it.  
#' 
#' Note that
#' the copy will result in the current data in memory written to the new
#' destination directory.  If the library is loaded into the workspace, 
#' the workspace version will be considered the most current version, and
#' that is the version that will be copied.  
#' @param x The library to copy.
#' @param nm The unquoted variable name to hold the new library.
#' @param directory_path The path to copy the library to.
#' @return The new library.
#' @family lib
#' @examples
#' # Create temp directory
#' tmp <- tempdir()
#' 
#' # Create library
#' libname(dat1, tmp)
#' 
#' # Add dat to library
#' lib_add(dat1, mtcars, iris)
#' 
#' # Copy dat1 to dat2
#' lib_copy(dat1, dat2, file.path(tmp, "copy"))
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
  libnm <- paste(deparse(substitute(x, env = environment())), collapse = "")

  # Sync with list if needed
  if (attr(x, "loaded") == TRUE) {
    
    x <- lib_sync(x, name = libnm) 
  }
  
  # Get safe name of new library
  newlib <- paste(deparse(substitute(nm, env = environment())), collapse = "")
  
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
  
  log_logr(paste0("lib_copy: copied data from library '", libnm, "' to '", newlib, "'"))
  log_logr(cpy)
  
  return(cpy)
}



#' @title Delete a Data Library
#' @description The \code{lib_delete} function deletes a data library from
#' the file system and from memory.  All data files associated with the library
#' and the specified engine will be deleted.
#' If other files exist in the library directory, they will not be affected
#' by the delete operation.  
#' 
#' The directory that contains the data will also
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
  lnm <- paste(deparse(substitute(x, env = environment())), collapse = "")
  
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
    
    log_logr(paste0("lib_delete: deleted library '", lnm, "'"))
  
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
#' by the data library, as stored on disk.  
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


#' @title Get Information about a Data Library
#' @description The \code{lib_info} function returns a data frame of information
#' about each item in the data library.  That information includes the item
#' name, file extension, number of rows, number of columns, size in bytes, 
#' and the last modified date.
#' @param x The data library.
#' @return A data frame of information about the library.
#' @family lib
#' @examples 
#' # Create temp directory
#' tmp <- tempdir()
#' 
#' # Create data library
#' libname(dat, tmp)
#' 
#' # Add data to library
#' lib_add(dat, trees, rock, beaver1)
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

