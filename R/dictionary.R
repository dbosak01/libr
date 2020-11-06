


#' @title Create a Data Dictionary
#' @param x The input library, data frame, or tibble.
#' @description A function to create a data dictionary for a data frame,
#' a tibble, or a data library.  The function will generate a tibble of 
#' information about the data.  The tibble will contain the following columns:
#' @import tibble
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

#' @noRd
getDictionary <- function(x, dsnm) {
  
  ret <- NULL
  rw <- NULL
  for (nm in names(x)) {
    
    lbl <- attr(x[[nm]], "label")
    desc <- attr(x[[nm]], "description")
    fmt <- attr(x[[nm]], "format")
    
    rw <- data.frame(Name = dsnm,
                     Column = nm,
                     Class = class(x[[nm]]),
                     Label = ifelse(!is.null(lbl), lbl, NA),
                     Description = ifelse(!is.null(desc), desc, NA),
                     Format = ifelse(!is.null(fmt), fmt, NA),
                     Width = ifelse(typeof(x[[nm]]) == "character", 
                                    max(nchar(x[[nm]])),
                                    NA),
                     Rows = nrow(x),
                     NAs = sum(is.na(x[[nm]])))
                     
                     
    if (is.null(ret))
      ret <- rw
    else 
      ret <- rbind(ret, rw)
    
  }
  
  return(ret)
  
}
