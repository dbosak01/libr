
# Formatting Catalog Definition -------------------------------------------


#' @title Create a format catalog
#' @description A format catalog is a collection of formats.  This format
#' collection allows you to manage and store formats as a unit.  The 
#' \code{fcat} function defines the format catalog.
#' @details A format catalog is an S3 object of class "fcat".  The purpose of 
#' the catalog is to combine related formats, and allow you to manipulate all
#' of them as a single object.  The format catalog can be saved to/from a file 
#' using the \code{\link{write.fcat}} and \code{\link{read.fcat}} functions. 
#' A format catalog can also 
#' be converted to/from a data frame using the \code{\link{as.fcat.data.frame}}
#' and \code{\link{as.data.frame.fcat}} functions.  Formats are accessed in the
#' catalog using list syntax. 
#' 
#' A format catalog can be used to assign formats to a data frame
#' or tibble using the \code{formats} function. See the \code{\link{formats}}
#' function for additional details.
#' 
#' A format catalog may contain any type of format except a formatting list.
#' Allowed formats include a formatting string, a named vector lookup, a 
#' user-defined format, and a vectorized formatting function.  A formatting 
#' list can be converted to a format catalog and saved independently.  See the 
#' 
#' @param ... A set of formats. Pass the formats as a name/value pair.  Multiple
#' name/value pairs may be separated by a comma.
#' @return The format catalog object.
#' @seealso \code{\link{formats}} function for assigning formats to a data 
#' frame, and the \code{\link{fdata}} and \code{\link{fapply}} functions for
#' applying formats.
#' @family fcat
#' @examples 
#' # Create format catalog
#' c1 <- fcat(num_fmt  = "%.1f",
#'            label_fmt = value(condition(x == "A", "Label A"),
#'                              condition(x == "B", "Label B"),
#'                              condition(TRUE, "Other")),
#'            date_fmt = "%d%b%Y")
#' 
#' # Use formats in the catalog
#' fapply(2, c1$num_fmt)
#' fapply(c("A", "B", "C", "B"), c1$label_fmt)
#' fapply(Sys.Date(), c1$date_fmt)
#' @export
fcat <- function(...) {
  
  # Create new structure of class "fcat"
  f <- structure(list(...), class = c("fcat", "list"))
  
  
  return(f)
  
}



# Conversion Functions -----------------------------------------------------

#' @title Generic Casting Method for Format Catalogs
#' @description A generic method for casting objects to
#' a format catalog.  Individual objects will inherit from this function.
#' @param x The object to cast.
#' @return A formatting object, created using the information in the 
#' input object.
#' @seealso For class-specific methods, see \code{\link{as.fcat.data.frame}},
#' \code{\link{as.fcat.list}}, and \code{\link{as.fcat.fmt_lst}}.
#' @family fcat
#' @export
as.fcat <- function (x) {
  UseMethod("as.fcat", x)
}

#' @title Convert a data frame to a format catalog
#' @description This function takes a data frame as input
#' and converts it to a format catalog based on the information contained
#' in the data frame. The data frame should have 5 columns: "Name", "Type",
#' "Expression", "Label" and "Order".  
#' @details 
#' The \code{as.fcat.data.frame} converts a data frame to a format catalog. A
#' corresponding conversion for class "tbl_df" converts a tibble.
#' 
#' To understand the format of the data frame, create a format and use
#' the \code{as.data.frame} method to convert the format to a data frame.
#' Then observe the columns and organization of the data.
#' @section Input Data Frame Specifications:
#' The input data frame should contain the following columns:
#' \itemize{
#' \item \strong{Name}: The name of the format
#' \item \strong{Type}: The type of format.  See the type codes below.
#' \item \strong{Expression}: The formatting expression. The expression will 
#' hold different types of values depending on the format type.
#' \item \strong{Label}: The label for user-defined, "U" type formats.
#' \item \strong{Order}: The order for user-defined, "U" type formats. 
#' }
#' Any additional columns will be ignored.  Column names are case-insensitive.
#' 
#' Valid values for the "Type" column are as follows:
#' \itemize{
#' \item \strong{U}: User Defined List created with the \code{value} function.
#' \item \strong{S}: A formatting string of formatting codes.
#' \item \strong{F}: A vectorized function.
#' \item \strong{V}: A named vector lookup.}
#' 
#' The "Label" and "Order" columns are used only for a type "U", user-defined
#' format created with the \code{\link{value}} function.
#' @param x The data frame to convert.
#' @return A format catalog based on the information contained in the 
#' input data frame.
#' @examples 
#' # Create a format catalog
#' c1 <- fcat(num_fmt  = "%.1f",
#'            label_fmt = value(condition(x == "A", "Label A"),
#'                              condition(x == "B", "Label B"),
#'                              condition(TRUE, "Other")),
#'            date_fmt = "%d-%b-%Y")
#'            
#' # Convert catalog to data frame to view the structure
#' df <- as.data.frame(c1)
#' print(df)
#' 
#' #       Name Type Expression   Label Order
#' # 1   num_fmt    S       %.1f            NA
#' # 2 label_fmt    U   x == "A" Label A    NA
#' # 3 label_fmt    U   x == "B" Label B    NA
#' # 4 label_fmt    U       TRUE   Other    NA
#' # 5  date_fmt    S   %d-%b-%Y            NA
#' 
#' # Convert data frame back to a format catalog
#' c2 <- as.fcat(df)
#' 
#' # Use re-converted catalog
#' fapply(123.456, c2$num_fmt)
#' fapply(c("A", "B", "C", "B"), c2$label_fmt)
#' fapply(Sys.Date(), c2$date_fmt)
#' @export
as.fcat.data.frame <- function(x) {
  
  names(x) <- titleCase(names(x))
  
  s <- split(x, x$Name)
  ret <- fcat()
  for (df in s) {
    
    nm <- df[1, "Name"]
    typ <- df[1, "Type"]
    
    if (typ == "U") {
      ret[[nm]] <- as.fmt(df)
    } else if (typ == "S") {
      ret[[nm]] <- df[1, "Expression"]
    } else if (typ == "F") {
      ret[[nm]] <- eval(str2lang(df[1, "Expression"]))
    } else if (typ == "V") {
      ret[[nm]] <- eval(str2lang(df[1, "Expression"]))
    }
  }
  
  return(ret)
  
}


#' @inherit as.fcat.data.frame
#' @export
as.fcat.tbl_df <- function(x) {
  
  ret <- as.fcat.data.frame(as.data.frame(x))
  
  return(ret)
}


#' @title Convert a list to a format catalog
#' @param x The list to convert.  List must contained named formats.
#' @return A format catalog based on the formats contained in the input list.
#' @family fcat
#' @export
as.fcat.list <- function(x) {
  
  
  class(x) = c("fmt_lst", class(x))
  
  ret <- x
  
  
  return(ret)
}

#' @title Convert a formatting list to a format catalog
#' @param x The formatting list to convert.
#' @family fcat
#' @export
as.fcat.fmt_lst <- function(x) {
  
  
  
  ret <- x$formats
  
  class(ret) <- list("fcat", "list")
  
  
  return(ret)
}

#' @title Convert a format catalog to a data frame
#' @description This function takes the information stored in a format 
#' catalog, and coverts it to the data frame.  The data frame format is 
#' useful for storage, editing, saving to a spreadsheet, etc.  The 
#' data frame show the name of the formats, their class, and the format 
#' expression.  For use-defined formats, the data frame populates 
#' additional columns for the label and order.
#' @param x The format catalog to convert.
#' @param row.names Row names of the return data frame.  Default is NULL.
#' @param optional TRUE or FALSE value indicating whether converting to
#' syntactic variable names is options.  In the case of formats, the 
#' resulting data frame will always be returned with syntactic names, and 
#' this parameter is ignored.
#' @param ... Any follow-on parameters.
#' @return A data frame that contains the values stored in the format 
#' catalog.  
#' @family fcat
#' @examples 
#' # Create a format catalog
#' c1 <- fcat(num_fmt  = "%.1f",
#'            label_fmt = value(condition(x == "A", "Label A"),
#'                              condition(x == "B", "Label B"),
#'                              condition(TRUE, "Other")),
#'            date_fmt = "%d%b%Y")
#'            
#' # Convert catalog to data frame to view the structure
#' df <- as.data.frame(c1)
#' print(df)
#' 
#' #       Name Type Expression   Label Order
#' # 1   num_fmt    S       %.1f            NA
#' # 2 label_fmt    U   x == "A" Label A    NA
#' # 3 label_fmt    U   x == "B" Label B    NA
#' # 4 label_fmt    U       TRUE   Other    NA
#' # 5  date_fmt    S     %d%b%Y            NA
#' 
#' # Convert data frame back to a format catalog
#' c2 <- as.fcat(df)
#' @export
as.data.frame.fcat <- function(x, row.names = NULL, optional = FALSE, ...) {
  
  tmp <- list()
  
  for (nm in names(x)) {
    
    if (any(class(x[[nm]]) == "fmt")) {
      
      tmp[[nm]] <- as.data.frame.fmt(x[[nm]], name = nm)
      
    } else if (all(class(x[[nm]]) == "character")) {
      
      if (length(x[[nm]]) == 1 & is.null(names(x[[nm]]))) {
        tmp[[nm]] <- data.frame(Name = nm, 
                                Type = "S",
                                Expression = x[[nm]],
                                Label = "", 
                                Order = NA)
      } else {
        tmp[[nm]] <- data.frame(Name = nm, 
                                Type = "V",
                                Expression = deparse1(x[[nm]]),
                                Label = "", 
                                Order = NA)
      }
      
    } else if (any(class(x[[nm]]) == "function")) {
      
      tmp[[nm]] <-  data.frame(Name = nm, 
                               Type = "F",
                               Expression = deparse1(x[[nm]]),
                               Label = "", 
                               Order = NA)
      
      
    }
    
  }
  
  
  ret <- do.call("rbind", tmp)
  
  if (!is.null(row.names))
    rownames(ret) <- row.names
  else
    rownames(ret) <- NULL
  
  return(ret)
  
}


# Utility Functions -------------------------------------------------------


#' @title Write a format catalog to the file system
#' @description The \code{write.fcat} function writes the format catalog
#' to the file system.  By default, the catalog will be written to the 
#' current working directory, using the variable name as the file name.  These
#' defaults can be overridden using the appropriate parameters.  The catalog
#' will be saved with a file extension of ".fcat". 
#' 
#' Note that the format catalog is saved as an RDS file.  The ".fcat" file 
#' extension only serves to distinguish the format catalog from other RDS
#' files.
#' @param x The format catalog to write.
#' @param dir_path The directory path to write the catalog to. Default is the 
#' current working directory.
#' @param file_name The name of the file to save the catalog as.  Default is
#' the name of the variable that contains the catalog.  The ".fcat" file
#' extension will be added automatically.
#' @return The full path of the saved format catalog.
#' @family fcat
#' @examples 
#' # Create format catalog
#' c1 <- fcat(num_fmt  = "%.1f",
#'            label_fmt = value(condition(x == "A", "Label A"),
#'                              condition(x == "B", "Label B"),
#'                              condition(TRUE, "Other")),
#'            date_fmt = "%d%b%Y")
#'            
#' # Get temp directory
#' tmp <- tempdir()            
#'            
#' # Save catalog to file system
#' pth <- write.fcat(c1, dir_path = tmp)
#' 
#' # Read from file system
#' c2 <- read.fcat(pth)
#' 
#' # Use formats in the catalog
#' fapply(2, c1$num_fmt)
#' fapply(c("A", "B", "C", "B"), c1$label_fmt)
#' fapply(Sys.Date(), c1$date_fmt)
#' @export
write.fcat <- function(x, dir_path = getwd(), file_name = deparse(substitute(x, 
                                                                             env = environment()))) {
  
  pth <- file.path(dir_path, paste0(file_name, ".fcat"))
  
  
  if (file.exists(pth))
    file.remove(pth)
  
  saveRDS(x, pth)
  
  return(pth)
}


#' @title Read a format catalog from the file system
#' @description The \code{read.fcat} function reads a format catalog
#' from the file system.  The function accepts a path to the format catalog,
#' reads the catalog, and returns it.
#' 
#' Note that the format catalog is saved as an RDS file.  The ".fcat" file 
#' extension only serves to distinguish the format catalog from other RDS
#' files.
#' @param file_path The path to the format catalog.
#' @return The format catalog as an R object.
#' @family fcat
#' @examples 
#' # Create format catalog
#' c1 <- fcat(num_fmt  = "%.1f",
#'            label_fmt = value(condition(x == "A", "Label A"),
#'                              condition(x == "B", "Label B"),
#'                              condition(TRUE, "Other")),
#'            date_fmt = "%d%b%Y")
#'            
#' # Get temp directory
#' tmp <- tempdir()            
#'            
#' # Save catalog to file system
#' pth <- write.fcat(c1, dir_path = tmp)
#' 
#' # Read from file system
#' c2 <- read.fcat(pth)
#' 
#' # Use formats in the catalog
#' fapply(2, c1$num_fmt)
#' fapply(c("A", "B", "C", "B"), c1$label_fmt)
#' fapply(Sys.Date(), c1$date_fmt)
#' @export
read.fcat <- function(file_path) {
  
  ret <-  readRDS(file_path)
  
  
  return(ret)
}

#' @title Print a format catalog
#' @description A class-specific instance of the \code{print} function for 
#' format catalogs.  The function prints the format catalog in a tabular manner.  
#' Use \code{verbose = TRUE} to print the catalog as a list.
#' @param x The format catalog to print.
#' @param ... Any follow-on parameters.
#' @param verbose Whether or not to print the format catalog in verbose style.
#' By default, the parameter is FALSE, meaning to print in tabular style.
#' @return The object, invisibly.
#' @family fcat
#' @examples 
#' #' # Create format catalog
#' c1 <- fcat(num_fmt  = "%.1f",
#'            label_fmt = value(condition(x == "A", "Label A"),
#'                              condition(x == "B", "Label B"),
#'                              condition(TRUE, "Other")),
#'            date_fmt = "%d%b%Y")
#'            
#' # Print the catalog
#' print(c1)
#' @export
print.fcat <- function(x, ..., verbose = FALSE) {
  
  if (verbose == TRUE) {
    
    print(unclass(x))  
    
  } else {
    
    dat <- as.data.frame(x)
    
    print(dat)
    
  }
  
  invisible(x)
}


#' @title Class test for a format catalog
#' @description This function tests whether an object is a format catalog.  The
#' format catalog has a class of "fcat".  
#' @param x The object to test.
#' @return TRUE or FALSE, depending on whether or not the object is a 
#' format catalog.
#' @family fcat
#' @examples 
#' # Create format catalog
#' c1 <- fcat(num_fmt  = "%.1f",
#'            label_fmt = value(condition(x == "A", "Label A"),
#'                              condition(x == "B", "Label B"),
#'                              condition(TRUE, "Other")),
#'            date_fmt = "%d%b%Y")
#'            
#' # Test for "fcat" class
#' is.fcat(c1)  
#' is.fcat(Sys.Date())          
#' @export
is.fcat <- function(x) {
  
  ret <- FALSE
  if (any(class(x) == "fcat"))
    ret <-  TRUE
  
  return(ret)
}
