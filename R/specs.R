


#' @title Create an Import Specification
#' @description A function to capture the import specifications for a 
#' particular data file.  This information can be used on the 
#' \code{\link{libname}} function to correctly assign the data types for 
#' file formats that are not strongly typed, such as 'csv' and 'xlsx'.
#' @param ... Names pairs of column names and column data types.
#' Available types are: 'guess', 'logical', 'character', 'integer', 'numeric',
#' 'date', 'datetime', and 'time'.  The date/time data types accept an optional
#' input format.  To supply the input format, append it after the data type
#' following an equals sign, e.g.: 'date=%d%B%Y' or 'datetime=%d%m%Y %H:%M:%S'.
#' Default is NULL, meaning no column types are specified, and the function
#' should make its best guess for each column.
#' @param na A vector of values to be treated as NA.  For example, the 
#' vector \code{c('', ' ')} will cause empty strings and single blanks to 
#' be converted to NA values. Default is NULL, meaning the value of the 
#' \code{na} parameter will be taken from the \code{\link{specs}} function.
#' @param trim_ws Whether or not to trim white space from the input data values.
#' @param params Any follow-on parameters to the underlying import function.
#' Valid values are TRUE, and FALSE.  Default is TRUE.
#' @export
import_spec <- function(..., na = NULL, trim_ws = NULL, params = NULL) {
  
  # Create new structure of class "lib"
  ispec <- structure(list(), class = c("import_spec", "list"))
  
  ispec$col_types = list(...)
  ispec$na = na
  ispec$trim_ws = trim_ws
  ispec$params = params
  
  
  return(ispec)
  
}


#' @title Create an Import Spec Collection
#' @description A function to capture a set of import specifications for a 
#' directory of data files.  These specs can be used on the 
#' \code{\link{libname}} function to correctly assign the data types for 
#' file formats that are not strongly typed, such as 'csv' and 'xlsx'. The 
#' input specification is defined as an object so it can be stored and reused.
#' See the \code{\link{write.specs}} and \code{\link{read.specs}} functions
#' for additional information on storing specs.
#' @param col_types A vector of column data types and optional input format.
#' Available types are: 'guess', 'logical', 'character', 'integer', 'numeric',
#' 'date', 'datetime', and 'time'.  The date/time data types accept an optional
#' input format.  To supply the input format, append it after the data type
#' following an equals sign, e.g.: 'date=%d%B%Y' or 'datetime=%d%m%Y %H:%M:%S'.
#' Default is NULL, meaning no column types are specified, and the function
#' should make its best guess for each column.
#' @param na A vector of values to be treated as NA.  For example, the 
#' vector \code{c('', ' ')} will cause empty strings and single blanks to 
#' be converted to NA values. Default is that empty strings ('') are considered
#' NA.
#' @param ... Named input specs.  The name should correspond to the file name.
#' @param trim_ws Whether or not to trim white space from the input data values.
#' Valid values are TRUE, and FALSE.  Default is TRUE.
#' @export
specs <- function(col_types = NULL, na = "", trim_ws = TRUE, ...) {
  
  # Create new structure of class "lib"
  s <- structure(list(), class = c("specs", "list"))
  
  s$col_types = col_types
  s$na = na
  s$trim_ws = trim_ws
  s$specs = list(...)
  
  
  return(s)
  
}

#' @title Read input specs from the file system
#' @description A function to read input specifications from the file system.
#' The function accepts a full or relative path to the spec file, and returns
#' the specs as an object.
#' @param file_path The full or relative path to the file system.  This 
#' parameter is required. If the file_path does not contain the '.specs' file
#' extension, the function will add it.
#' @return The specifications object.
#' @export 
read.specs <- function(file_path) {
  
  pth <- file_path
  
  file_path <- "data/forker.specs"
  if (grepl(".specs", file_path, fixed = TRUE) == FALSE) {
    pth <- paste0(file_path, ".specs")
    
    if (!file.exists(pth)) {
      if (dir.exists(file_path)) {
        lst <- list.files(file_path, ".specs")
        if (length(lst) == 0) {
         stop(paste0("Spec file not found: ", file_path))
          
        } else if (length(lst) > 1) {
          
          stop("More than one spec file found.")
        } else {
          
         pth <- lst[[1]] 
        }
        
      } else {
        
        stop(paste0("File not found: ", file_path))
      }
    }
  }
  
  obj <- readRDS(pth)
  
  return(obj)
  
}

#' @title Write input specs to the file system
#' @description A function to write input specifications to the file system.
#' The function accepts a specifications object and a full or relative
#' path.  The function returns the full file path.
#' @param x A specifications object of class 'specs'.
#' @param dir_path A full or relative path to save the specs. 
#' @param file_name The file name to save to specs, without a file extension.
#' The file extension will be added automatically.  If no file name is 
#' supplied, the function will use the variable name as the file name. 
#' @export 
write.specs <- function(x, dir_path = getwd(), file_name = NULL) {
  
  if (!"specs" %in% class(x))
    stop("Input object must be of class 'specs'.")
  
  if (is.null(file_name)) {
    nm <- deparse1(substitute(x, env = environment()))
    pth <- file.path(dir_path, paste0(nm, ".specs"))
    
  } else {
    
    pth <- file.path(dir_path, paste0(file_name, ".specs"))
  }
   # print(pth)
  saveRDS(x, pth)
  
  return(pth)
}
