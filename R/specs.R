

# Spec Definition ---------------------------------------------------------


#' @title Create an Import Spec Collection
#' @encoding UTF-8
#' @description A function to capture a set of import specifications for a 
#' directory of data files.  These specs can be used on the 
#' \code{\link{libname}} function to correctly assign the data types for 
#' imported data files. The 
#' import engines will guess at the data types for any columns that
#' are not explicitly defined in the import specifications. Import
#' specifications are defined with the \code{\link{import_spec}} function.
#' The import spec syntax is the same for all data engines.
#' 
#' Note that the \code{na} and \code{trim_ws} parameters on the \code{specs} 
#' function will be applied globally to all files in the library.  
#' These global settings can be overridden on the \code{\link{import_spec}} 
#' for any particular data file. 
#' 
#' Also note that the \code{specs} collection is defined as an object 
#' so it can be stored and reused.
#' See the \code{\link{write.specs}} and \code{\link{read.specs}} functions
#' for additional information on saving specs.
#' @param ... Named input specs.  The name should correspond to the file name,
#' without the file extension.
#' The spec is defined as an \code{import_spec} object.  See the 
#' \code{\link{import_spec}} function for additional information on 
#' parameters for that object.
#' @param na A vector of values to be treated as NA.  For example, the 
#' vector \code{c('', ' ')} will cause empty strings and single blanks to 
#' be converted to NA values. For most file types, 
#' empty strings and the string 'NA' \code{('', 'NA')} are considered NA.  
#' For SAS® datasets and transport files, a single blank and a single dot 
#' \code{c(" ", ".")} are considered NA. The value of the 
#' \code{na} parameter on the \code{specs} function can be overridden
#' by the \code{na} parameter on the \code{import_spec} function.
#' @param trim_ws Whether or not to trim white space from the input data values.
#' Valid values are TRUE, and FALSE.  Default is TRUE.  The value of the 
#' \code{trim_ws} parameter on the \code{specs} function can be overridden
#' by the \code{trim_ws} parameter on the \code{import_spec} function.
#' @return The import specifications object.
#' @seealso \code{\link{libname}} to create a data library, 
#' \code{\link{dictionary}} for generating a data dictionary, and 
#' \code{\link{import_spec}} for additional information on defining an 
#' import spec.
#' @family specs
#' @examples 
#' library(readr)
#' 
#' # Create temp path
#' tmp <- file.path(tempdir(), "mtcars.csv")
#' 
#' # Create data for illustration purposes
#' df <- data.frame(vehicle = rownames(mtcars), mtcars[c("mpg", "cyl", "disp")],
#'                  stringsAsFactors = FALSE)
#' 
#' # Kill rownames
#' rownames(df) <- NULL
#' 
#' # Add some columns
#' df <- datastep(df[1:10, ], {
#' 
#'         recdt <- "10JUN1974"
#' 
#'         if (mpg >= 20)
#'           mpgcat <- "High"
#'         else 
#'           mpgcat <- "Low"
#'       
#'         if (cyl == 8)
#'           cyl8 <- TRUE
#'   })
#'   
#' df
#' #              vehicle  mpg cyl  disp     recdt mpgcat cyl8
#' # 1          Mazda RX4 21.0   6 160.0 10JUN1974   High   NA
#' # 2      Mazda RX4 Wag 21.0   6 160.0 10JUN1974   High   NA
#' # 3         Datsun 710 22.8   4 108.0 10JUN1974   High   NA
#' # 4     Hornet 4 Drive 21.4   6 258.0 10JUN1974   High   NA
#' # 5  Hornet Sportabout 18.7   8 360.0 10JUN1974    Low TRUE
#' # 6            Valiant 18.1   6 225.0 10JUN1974    Low   NA
#' # 7         Duster 360 14.3   8 360.0 10JUN1974    Low TRUE
#' # 8          Merc 240D 24.4   4 146.7 10JUN1974   High   NA
#' # 9           Merc 230 22.8   4 140.8 10JUN1974   High   NA
#' # 10          Merc 280 19.2   6 167.6 10JUN1974    Low   NA
#'
#' # Save to temp directory for this example
#' write_csv(df, tmp)
#' 
#' ## Start Example ##
#' 
#' # Define import spec
#' spcs <- specs(mtcars = import_spec(vehicle = "character",
#'                                    cyl = "integer",
#'                                    recdt = "date=%d%b%Y",
#'                                    mpgcat = "guess",
#'                                    cyl8 = "logical"))
#'                                    
#' # Create library
#' libname(dat, tempdir(), "csv", import_specs = spcs)
#' # $mtcars
#' # library 'dat': 1 items
#' # - attributes: csv not loaded
#' # - path: C:\Users\User\AppData\Local\Temp\RtmpqAMV6L
#' # - items:
#' #     Name Extension Rows Cols   Size        LastModified
#' # 1 mtcars       csv   10    7 9.3 Kb 2020-11-29 09:47:52
#' 
#' # View data types
#' dictionary(dat)
#' # # A tibble: 7 x 10
#' #   Name   Column  Class     Label Description Format Width Justify  Rows   NAs
#' #   <chr>  <chr>   <chr>     <chr> <chr>       <lgl>  <int> <chr>   <int> <int>
#' # 1 mtcars vehicle character NA    NA          NA        17 NA         10     0
#' # 2 mtcars mpg     numeric   NA    NA          NA        NA NA         10     0
#' # 3 mtcars cyl     integer   NA    NA          NA        NA NA         10     0
#' # 4 mtcars disp    numeric   NA    NA          NA        NA NA         10     0
#' # 5 mtcars mpgcat  character NA    NA          NA         4 NA         10     0
#' # 6 mtcars recdt   Date      NA    NA          NA        NA NA         10     0
#' # 7 mtcars cyl8    logical   NA    NA          NA        NA NA         10     8
#' 
#' # Clean up
#' lib_delete(dat)
#' @export
specs <- function(..., na = c("", "NA"), trim_ws = TRUE) {
  
  # Create new structure of class "specs"
  s <- structure(list(), class = c("specs", "list"))
  
  # s$col_types = col_types
  s$na = na
  s$trim_ws = trim_ws
  s$specs = list(...)
  
  
  return(s)
  
}

#' @title Create an Import Specification
#' @description A function to create the import specifications for a 
#' particular data file.  This information can be used on the 
#' \code{\link{libname}} function to correctly assign the data types for 
#' columns on imported data. The import specifications are defined as 
#' name/value pairs, where the name is the column name and the value is the
#' data type indicator.  Available data type indicators are 
#' 'guess', 'logical', 'character', 'integer', 'numeric',
#' 'date', 'datetime', and 'time'.  See the \code{\link{specs}} function
#' for an example of using import specs.
#' @param ... Named pairs of column names and column data types.
#' Available types are: 'guess', 'logical', 'character', 'integer', 'numeric',
#' 'date', 'datetime', and 'time'.  The date/time data types accept an optional
#' input format.  To supply the input format, append it after the data type
#' following an equals sign, e.g.: 'date=\%d\%B\%Y' or 
#' 'datetime=\%d\%m\%Y \%H:\%M:\%S'. Default is NULL, meaning no column 
#' types are specified, and the function should make its best 
#' guess for each column.
#' @param na A vector of values to be treated as NA.  For example, the 
#' vector \code{c('', ' ')} will cause empty strings and single blanks to 
#' be converted to NA values. Default is NULL, meaning the value of the 
#' \code{na} parameter will be taken from the \code{\link{specs}} function.
#' Any value supplied on the \code{import_spec} function will override the 
#' value from the \code{specs} function.
#' @param trim_ws Whether or not to trim white space from the input data values.
#' The default is NULL, meaning the value of the \code{trim_ws} parameter
#' will be taken from the \code{\link{specs}} function.  Any value supplied 
#' on the \code{import_spec} function will override the value from the 
#' \code{specs} function.
#' @return The import specification object.
#' @seealso \code{\link{libname}} to create a data library, and 
#' \code{\link{specs}} for an example using import specs.
#' @family specs
#' @export
import_spec <- function(..., na = NULL, trim_ws = NULL) {
  
  # Create new structure of class "import_spec"
  ispec <- structure(list(), class = c("import_spec", "list"))
  
  ispec$col_types = list(...)
  ispec$na = na
  ispec$trim_ws = trim_ws
  
  return(ispec)
  
}



# External Utilities ------------------------------------------------------


#' @title Read import specs from the file system
#' @description A function to read import specifications from the file system.
#' The function accepts a full or relative path to the spec file, and returns
#' the specs as an object.  If the \code{file_path} parameter is passed
#' as a directory name, the function will search for a file with a '.specs'
#' extension and read it.
#' @param file_path The full or relative path to the file system.  Default is
#' the current working directory. If the \code{file_path} is a file name that 
#' does not contain the '.specs' file extension, the function will add the
#' extension.  If the \code{file_path} contains a directory name, 
#' the function will search the directory for a file with an extension 
#' of '.specs'.  If more than one file with an extension of '.specs' is founds, 
#' the function will generate an error.  
#' @return The specifications object.
#' @family specs
#' @export 
read.specs <- function(file_path = getwd()) {
  
  pth <- file_path
  
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
          
         pth <- file.path(file_path, lst[[1]])
        }
        
      } else {
        
        stop(paste0("File not found: ", file_path))
      }
    }
  }
  
  obj <- readRDS(pth)
  
  return(obj)
  
}

#' @title Write import specs to the file system
#' @description A function to write import specifications to the file system.
#' The function accepts a specifications object and a full or relative
#' path.  The function returns the full file path.  This function is 
#' useful so that you can define import specifications once, and reuse them
#' in multiple programs or across multiple teams.
#' @param x A specifications object of class 'specs'.
#' @param dir_path A full or relative path to save the specs. Default is the 
#' current working directory.
#' @param file_name The file name to save to specs, without a file extension.
#' The file extension will be added automatically.  If no file name is 
#' supplied, the function will use the variable name as the file name. 
#' @return The full file path.
#' @family specs
#' @export 
write.specs <- function(x, dir_path = getwd(), file_name = NULL) {
  
  if (!"specs" %in% class(x))
    stop("Input object must be of class 'specs'.")
  
  # Construct File Path
  if (is.null(file_name)) {
    nm <- paste(deparse(substitute(x, env = environment())), collapse = "")
    pth <- file.path(dir_path, paste0(nm, ".specs"))
    
  } else {
    
    pth <- file.path(dir_path, paste0(file_name, ".specs"))
  }
  
  # Save as an RDS with a .specs extension
  saveRDS(x, pth)
  
  return(pth)
}

#' @title Print import specifications
#' @description A function to print the import specification collection.
#' @param x The specifications to print.
#' @param ... Any follow-on parameters to the print function.
#' @param verbose Whether or not to print the specifications in verbose style.
#' By default, the parameter is FALSE, meaning to print in summary style.
#' @return The specification object, invisibly.
#' @family specs
#' @import crayon
#' @export
print.specs <- function(x, ..., verbose = FALSE) {
  
  if (verbose == TRUE) {
    
    # Print list form
    print(unclass(x))  
    
  } else {
    
    # Prepare color
    grey60 <- make_style(grey60 = "#999999")
    
    # Print a nice header
    cat(grey60(paste0("# import specs: ", length(x), 
                      " items\n")))
    
    cat(paste("- na:", paste0("\"", x$na, "\"", collapse = ", "), "\n"))

    cat(paste0("- trim_ws: ", as.character(x$trim_ws), "\n"))
    
    if (length(x$specs) > 0) {
      cat("- items:\n")
      
      for (nm in names(x$specs)) {
        
        cat(grey60(paste0("# import spec: ", nm, "\n")))
        
        cat(paste("- na:", paste0("\"", x$specs[[nm]]$na, "\"", collapse = ", "), "\n"))
        
        cat(paste0("- trim_ws: ", as.character(x$specs[[nm]]$trim_ws), "\n"))
        
        if (length(x$specs[[nm]]$col_types) > 0)
          cat("- column types:\n")
        
        for (colnm in names(x$specs[[nm]]$col_types)) {
          
           cat(paste0("  ", colnm, ": ", x$specs[[nm]]$col_types[[colnm]], "\n"))
          
        }
        
      }
    }
    
    
  }
  
  invisible(x)
}
