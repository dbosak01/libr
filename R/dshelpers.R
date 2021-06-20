
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
    
    df["first."] <- byfirst(res)
    df["last."] <- bylast(res)
  
  } else {
    
    df["first."] <- c(TRUE, rep(FALSE, times = nrow(df) - 1))
    df["last."] <- c(rep(FALSE, times = nrow(df) - 1), TRUE)
    
  }
  
  df["n."] <- seq_len(nrow(df))
  
  return(df)
  
}

