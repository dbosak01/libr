


#' @title Create a Data Dictionary
#' @param x The input library, data frame, or tibble.
#' @description A function to create a data dictionary for a data frame,
#' a tibble, or a data library.
#' @export
dictionary <- function(x) {
  
  ret <- NULL
  if (any(class(x) == "lib")) {
      
    
    for (dat in x) {
      
      if (is.null(ret))
        ret <- getDictionary(dat)
      else 
        ret <- rbind(ret, getDictionary(dat))
      
      
    }
    
  } else {
    
    ret = getDictionary(x)
  
  }
}

#' @noRd
getDictionary <- function(x) {
  
  
  
}
