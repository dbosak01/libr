

# Dictionary Definition ---------------------------------------------------



#' @title Create a Data Dictionary
#' @param x The input library, data frame, or tibble.
#' @description A function to create a data dictionary for a data frame,
#' a tibble, or a data library.  The function will generate a tibble of 
#' information about the data.  The tibble will contain the following columns:
#' \itemize{
#'   \item{\strong{Name:} The name of the data object.}
#'   \item{\strong{Column:} The name of the column.}
#'   \item{\strong{Class:} The class of the column.}
#'   \item{\strong{Label:} The value of the label attribute.}
#'   \item{\strong{Description:} A description applied to this column.}
#'   \item{\strong{Format:} The value of the format attribute.}
#'   \item{\strong{Width:} The max character width of the data in this column.}
#'   \item{\strong{Justify:} The justification or alignment attribute value.}
#'   \item{\strong{Rows:} The number of data rows.}
#'   \item{\strong{NAs:} The number of NA values in this column.}
#' }
#' @import tibble
#' @seealso \code{\link{libname}} to create a data library.  Also 
#' see the \code{\link[fmtr]{fmtr}} package for functions to set the
#' format, description, width, and justification attributes on a data frame.
#' @examples 
#' # Create temp directory
#' tmp <- tempdir()
#' 
#' # Create library
#' libname(dat, tmp)
#' 
#' # Add data to the library
#' lib_add(dat, beaver1)
#' lib_add(dat, iris)
#' 
#' # Examine the dictionary for the library
#' dictionary(dat)
#' # A tibble: 9 x 10
#' #   Name    Column       Class   Label Description Format Width Justify  Rows   NAs
#' #   <chr>   <chr>        <chr>   <lgl> <lgl>       <lgl>  <lgl> <lgl>   <int> <int>
#' # 1 beaver1 day          numeric NA    NA          NA     NA    NA        114     0
#' # 2 beaver1 time         numeric NA    NA          NA     NA    NA        114     0
#' # 3 beaver1 temp         numeric NA    NA          NA     NA    NA        114     0
#' # 4 beaver1 activ        numeric NA    NA          NA     NA    NA        114     0
#' # 5 iris    Sepal.Length numeric NA    NA          NA     NA    NA        150     0
#' # 6 iris    Sepal.Width  numeric NA    NA          NA     NA    NA        150     0
#' # 7 iris    Petal.Length numeric NA    NA          NA     NA    NA        150     0
#' # 8 iris    Petal.Width  numeric NA    NA          NA     NA    NA        150     0
#' # 9 iris    Species      factor  NA    NA          NA     NA    NA        150     0
#' 
#' # Clean up
#' lib_delete(dat)
#' @export
dictionary <- function(x) {
  
  if (!any(class(x) == "lib") & !any(class(x) == "data.frame"))
    stop("Input object must be a library or data frame.")
   
  ret <- NULL
  if (any(class(x) == "lib")) {
      
    
    for (nm in names(x)) {
      
      
      if (is.null(ret))
        ret <- getDictionary(x[[nm]], nm)
      else 
        ret <- rbind(ret, getDictionary(x[[nm]], nm))
      
      
    }
    
  } else {
    
    nm <- deparse1(substitute(x, env = environment()))
    ret = getDictionary(x, nm)
  
  }
  
  ret <- as_tibble(ret)
  
  return(ret)
}


# Utilities ---------------------------------------------------------------


#' @noRd
getDictionary <- function(x, dsnm) {
  
  ret <- NULL
  rw <- NULL
  for (nm in names(x)) {
    
    lbl <- attr(x[[nm]], "label")
    desc <- attr(x[[nm]], "description")
    fmt <- attr(x[[nm]], "format")
    jst <- attr(x[[nm]], "justify")
    
    rw <- data.frame(Name = dsnm,
                     Column = nm,
                     Class = class(x[[nm]]),
                     Label = ifelse(!is.null(lbl), lbl, as.character(NA)),
                     Description = ifelse(!is.null(desc), desc, as.character(NA)),
                     Format = ifelse(!is.null(fmt), fmt, NA),
                     Width = ifelse(typeof(x[[nm]]) == "character", 
                                    max(nchar(x[[nm]])),
                                    NA),
                     Justify = ifelse(!is.null(jst), jst, as.character(NA)),
                     Rows = nrow(x),
                     NAs = sum(is.na(x[[nm]])))
                     
                     
    if (is.null(ret))
      ret <- rw
    else 
      ret <- rbind(ret, rw)
    
  }
  
  return(ret)
  
}
