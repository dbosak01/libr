
#' A function to add automatic variables to a dataset
#' @noRd
add_autos <- function(df, groups = NULL, sort_check = FALSE) {
  
  
  if (!is.null(groups)) {
    
    # Concatenate multiple columns into a single vector separated by a pipe
    res <- apply(df[groups], MARGIN  = 1, 
                 FUN = function(i) paste(i, collapse = "|"))
    
    # Perform sort check if requested
    if (sort_check) {
      firstvals <- bychanges(res)
      ddat <- unique(res)
      
      if (length(ddat) != length(firstvals)) {
        stop(paste("Input data is not sorted according to the 'by' variable",
                   "parameter.\n  Either sort the input data properly or",
                   "set the sort_check parameter to FALSE."))
      }
      
    }
    
    # Clear out any names on input 
    #names(res) <- NULL
    
    df[["first."]] <- byfirst(res)
    df[["last."]] <- bylast(res)
    
    # Add first and last for each by variable
    for (nm in groups) {
      
      df[[paste0("first.", nm)]] <- byfirst(df[[nm]])
      df[[paste0("last.", nm)]] <- bylast(df[[nm]])
      
    }
  
  } else {
    
    if (nrow(df) > 0) {
      df[["first."]] <- c(TRUE, rep(FALSE, times = nrow(df) - 1))
      df[["last."]] <- c(rep(FALSE, times = nrow(df) - 1), TRUE)
    }
    
  }
  
  if (nrow(df) > 0)
    df[["..delete"]] <- FALSE
  else {
    

    
    if (ncol(df) == 0) {
      df <- data.frame("..delete" = FALSE, stringsAsFactors = FALSE)
    } else {
      nms <- names(df)
      
      nlst <- list()
      for (nm in nms){
       nlst[[nm]] <- NA 
      }
      
      nlst[["..delete"]] <- FALSE
      df <- as.data.frame(nlst, row.names = NULL, stringsAsFactors = FALSE)
      
    }
    

    
    
  }
  
  
  return(df)
  
}


remove_autos <- function(data, groups) {
  
  ret <- data
  
  ret[["first."]] <- NULL
  ret[["last."]] <- NULL
  ret[["..delete"]] <- NULL 
  
  if (!is.null(groups)) {
    for (nm in groups) {
      
      ret[[paste0("first.", nm)]] <- NULL
      ret[[paste0("last.", nm)]] <- NULL
      
    }
  }
  
  return(ret)
}
