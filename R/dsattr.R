
#' @title Assign Datastep Variable Attributes
#' @encoding UTF-8
#' @description A function to capture a set of column names with
#' which to create a datastep array.  The datastep array can be used inside
#' the \code{\link{datastep}} to loop over columns.  
#' @param class The desired class of the column. Valid values are 
#' 'character', 'numeric', 'integer', 'logical', and 'complex'.
#' @param label The label to associate with this column.  Accepts
#' any string value.
#' @param description A description for this column.  Accepts any string
#' value.
#' @param width The desired character width for the column.  
#' Applying a width to a column will make it fixed-width.  Column will
#' be space-padded according to the justify parameter.
#' @param format The format associated with this column.  See the 
#' \code{\link[fmtr]{fmtr}} package for more information about formatting.
#' @param justify The desired justification for the column.  This parameter
#' it normally used only for fixed-width, character columns.  Valid values
#' are 'left', 'right', 'center', and 'centre'.  
#' @param ... Any other attributes you wish to assign to this column. 
#' @return The import specifications object.
#' @seealso \code{\link{libname}} to create a data library, and
#' \code{\link{dictionary}} for generating a data dictionary
#' @family datastep
#' @examples 
#' library(libr)
#' @export
dsattr <- function(class = NULL, label = NULL, description = NULL, 
                   width = NULL, format = NULL, 
                   justify = NULL, ...) {
  
  # Create new structure of class "dsattr"
  s <- structure(list(), class = c("dsattr", "list"))
  

  s$class <- class
  s$label <- label
  s$format <- format
  s$width <- width
  s$justify <- justify
  s$description <- description
  
  l <- list(...)
  
  for (nm in names(l)) {
    s[[nm]] <- l[[nm]] 
  }
  
  return(s)
  
}
