

# dsarray Definition ------------------------------------------------------


#' @title Create a Datastep Array
#' @encoding UTF-8
#' @description A function to capture a set of column names with
#' which to create a datastep array.  The datastep array can be used inside
#' the \code{\link{datastep}} to loop over columns.  
#' @param ... Column names to include as part of the datastep array.
#' Separate column identifiers with commas.
#' @return The import specifications object.
#' @seealso \code{\link{libname}} to create a data library, and
#' \code{\link{dictionary}} for generating a data dictionary
#' @family datastep
#' @examples 
#' library(libr)
#' @export
dsarray <- function(...) {
  
  # Create new structure of class "dsarray"
  s <- structure(c(...), class = c("dsarray", "character"))
  
  names(s) <- s

  return(s)
  
}


#' @title Indexer for Datastep Array
#' @encoding UTF-8
#' @description A custom indexer for the Datastep Array.  The indexer will
#' return an individual row/column value for the specified column name.  
#' For additional details, see the \code{\link{dsarray}} function.
#' @param x The dsarray object.
#' @param i The index of the datastep array item to return a value for.
#' This index can be a column name or position in the array.  If no index
#' is supplied, a vector of all dataset values will be returned.
#' @return The value of the specified column for the current row in the 
#' datastep.  If no index is supplied, a vector of all column values will
#' be returned.
#' @family datastep
#' @export
`[.dsarray` <- function(x, i = NULL) {

  
  # Get row from the parent frame
  rw <- get("rw", envir = parent.frame())
  
  
  if (!is.null(i)) {

    # If index is a column name, return the column
    # Otherwise, lookup the column name from the array
    if (all(class(i) == "character"))
      ret <- rw[[i]]
    else 
      ret <- rw[[x[[i]]]] 
  
  } else {
    
    # Initialize an empty vector
    ret <- c()
    
    # Populate the vector with desired values
    for (j in x) {
      ret[j] <- rw[[j]]
    }
    
  }
  
  return(ret)

}


#' @title Length function for dsarray class
#' @encoding UTF-8
#' @description A length function for the Datastep Array.  
#' @param x The dsarray object.
#' @return The number of items in the specified dsarray.
#' @family datastep
#' @export
length.dsarray <- function(x) {
  
 return(length(as.character(x))) 
  
}





