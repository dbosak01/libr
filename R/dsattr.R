
#' @title Assign Datastep Variable Attributes
#' @encoding UTF-8
#' @description An object with which to assign attributes
#' to a column in a \code{\link{datastep}}.  The parameters allow you
#' to set the following attributes: 'class', 'label', 'description', 'width', 
#' 'justify', and 'format'.  Any other desired attributes can be set 
#' with \code{...}.
#' 
#' The attributes available in the \code{dsattr} class are closely aligned
#' with those available in the \strong{fmtr} package, and are intended
#' to be used together. See the \code{\link[fmtr]{fmtr}} package documentation
#' for additional details.  In particular, the \code{\link[fmtr]{fdata}} and
#' \code{\link[fmtr]{fapply}} functions provide relevant examples and 
#' explanations.
#' @param class The desired class of the column. Valid values are 
#' 'character', 'numeric', 'integer', 'logical', and 'complex'.
#' @param label The label to associate with this column.  Accepts
#' any string value. The label will appear as a column header on some 
#' data viewers.
#' @param description A description for this column.  Accepts any string
#' value.  The description is intended to be a longer explanation of the 
#' purpose or source of the variable.
#' @param width The desired width for the column in number of characters.
#' @param format The format associated with this column.  See the 
#' \code{\link[fmtr]{fmtr}} package for more information about formatting.
#' @param justify The desired justification for the column.  This parameter
#' it normally used only for fixed-width, character columns.  Valid values
#' are 'left', 'right', 'center', and 'centre'.  
#' @param ... Any other attributes you wish to assign to this column. Pass
#' these additional attributes as a name/value pair.
#' @return The datastep attributes object.
#' @seealso The \code{\link[fmtr]{fdata}} function in the \strong{fmtr}
#' package for more information
#' on formatting and rendering dataframes.
#' @family datastep
#' @examples 
#' library(libr)
#' 
#' # Create small sample dataframe
#' dat <- mtcars[1:10, c("mpg", "cyl")]
#' 
#' # Perform datastep and assign attributes
#' dat1 <- datastep(dat, 
#'                  attrib = list(mpg = dsattr(label = "Miles Per Gallon"),
#'                                cyl = dsattr(label = "Cylinders"),
#'                                mpgcat = dsattr(label = "Fuel Efficiency")),
#'                 {
#'                 
#'                   if (mpg >= 20) 
#'                     mpgcat = "High"
#'                   else 
#'                     mpgcat = "Low"
#'                 
#'                 })
#' 
#' # Print results                 
#' dat1
#' #                    mpg cyl mpgcat
#' # Mazda RX4         21.0   6   High
#' # Mazda RX4 Wag     21.0   6   High
#' # Datsun 710        22.8   4   High
#' # Hornet 4 Drive    21.4   6   High
#' # Hornet Sportabout 18.7   8    Low
#' # Valiant           18.1   6    Low
#' # Duster 360        14.3   8    Low
#' # Merc 240D         24.4   4   High
#' # Merc 230          22.8   4   High
#' # Merc 280          19.2   6    Low
#'                 
#' # Examine label attributes
#' attr(dat1$mpg, "label") 
#' # [1] "Miles Per Gallon"
#' 
#' attr(dat1$cyl, "label") 
#' # [1] "Cylinders"
#' 
#' attr(dat1$mpgcat, "label")
#' # [1] "Fuel Efficiency"
#' 
#' # See labels in viewer 
#' # View(dat1)
#' @export
dsattr <- function(class = NULL, label = NULL, description = NULL, 
                   width = NULL, format = NULL, 
                   justify = NULL, ...) {
  
  # Create new structure of class "dsattr"
  s <- structure(list(), class = c("dsattr", "list"))
  
  # Assign parameters to class properties
  s$class <- class
  s$label <- label
  s$format <- format
  s$width <- width
  s$justify <- justify
  s$description <- description
  
  # Deal with ... custom attributes
  l <- list(...)
  for (nm in names(l)) {
    s[[nm]] <- l[[nm]] 
  }
  
  return(s)
  
}
