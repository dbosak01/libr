

# Datastep Definition -----------------------------------------------------


#' @title Step through data row by row
#' @description The \code{datastep} function allows you to perform
#' row-wise conditional processing on a data frame or tibble. The function
#' contains parameters to drop, keep, or rename variables.  It also 
#' has the ability to perform by-group processing, and identify first
#' and last rows in the group.
#' @details 
#' The \code{datastep} function provides four automatic variables. These 
#' variables are generated automatically for every data step, and can 
#' be accessed at any point within the data step. 
#' \itemize{
#'   \item{\strong{data}: Represents the input data frame.}
#'   \item{\strong{n.}: Contains the row number.}
#'   \item{\strong{first.}: Indicates the beginning of a by-group.}
#'   \item{\strong{last.}: Indicates the end of a by-group.}
#' }
#' These automatic variables will be dropped from the data frame at the end
#' of the data step.  If you wish to keep the automatic variable values,
#' assign the automatic variable to a new variable and keep that variable.
#' 
#' @param data The data to step through.
#' @param steps The operations to perform on the data.  This parameter is 
#' typically specified as a set of R statements contained within 
#' curly braces.
#' @param keep A vector of quoted variable names to keep in the output
#' data set. By default, all variables are kept.
#' @param drop A vector of quoted variable names to drop from the output
#' data set. By default, no variables are dropped.
#' @param rename A named vector of quoted variables to rename.  The current
#' variable name should be on the left had side of the vector name/value pair,
#' and the new variable name should be on the right.  The rename operation
#' is performed after the data step, the keep, and the drop.  Therefore, 
#' the data steps should use the old variable name.  By default, all variable
#' retain their original names.
#' @param by A vector of quoted variable names to use for by-group processing.
#' This parameter will activate the \code{first.} and \code{last.} special
#' variables, that indicate the first or last rows in a group.  These 
#' special variables are useful for conditional processing on groups.
#' @param calculate Steps to set up calculated variables.  
#' Calculated variables are common performed with summary functions such as
#' \code{mean}, \code{median}, \code{min}, \code{max}, etc.  It is more 
#' efficient to set up calculated variables with the calculate parameter and then 
#' use those variables in the data step, rather than perform the summary
#' function inside the data step.
#' @return The processed data frame or tibble.
#' @import dplyr
#' @export
datastep <- function(data, steps, keep = NULL,
                     drop = NULL, rename = NULL,
                     by = NULL, calculate = NULL) {

  # Put code in a variable for safe-keeping
  code <- substitute(steps, env = environment())

  # Put aggregate functions in a variable 
  agg <- substitute(calculate, env = environment())
  if (deparse1(agg) != "NULL") {
   data <- within(data, eval(agg), keepAttrs = TRUE)
  }
  
  ret <- NULL
  firstval <- NULL
  
  # Set by if data is a grouped tibble
  if (is.null(by) && "grouped_df" %in% class(data)) {
    if (!is.null(attr(data, "groups"))) {
      grpdf <- attr(data, "groups")
      nms <- names(grpdf)
      if (!is.null(nms)) {
        nms <- nms[nms != ".rows"]
        if (length(nms) > 0)
          by <- nms
      }
    }
  }


  # Step through row by row
  for (n. in seq_len(nrow(data))) {

    # Deal with first. and last.
    # These can be accessed from within the evaluated code,
    # which is really cool.
    if (!is.null(by)) {
      if (is.null(firstval)) {
        firstval <- data[n., by]
        first. <- TRUE
      } else {

        # Compare current by group to previous row
        if (dfcomp(firstval, data[n., by]) == FALSE) {
          first. <- TRUE
          firstval <- data[n., by]
        } else {
          first. <- FALSE
        }
      }

      # If it's the last row of the data frame, mark last.
      if (n. == nrow(data)) {
        last. <- TRUE
      } else {
        
        # Compare by group to next row to determine last.
        if (dfcomp(data[n. + 1, by],  data[n., by]) == FALSE) {
          last. <- TRUE
        } else {
          last. <- FALSE
        }
      }

    } else {
      
      # If no by group is specified, mark the first and last rows
      # of the entire data frame
      
      # If it's the first row
      if (n. == 1)
        first. <- TRUE
      else
        first. <- FALSE

      # If it's the last row
      if (n. == nrow(data))
        last. <- TRUE
      else
        last. <- FALSE

    }
  

    # Evaluate the code for the row
    r1 <- within(data[n., ], eval(code), keepAttrs = TRUE)

    # Bind resulting row
    if (is.null(ret))
      ret <- r1
    else
      ret <- bind_rows(ret, r1)
  }

  # Perform drop operation
  if (!is.null(drop))
    ret <- ret[ , !names(ret) %in% drop]

  # Perform keep operation
  if (!is.null(keep)) {
    ret <- ret[ , keep]
  }

  # Perform rename operation
  if (!is.null(rename)) {
    nms <- names(ret)
    names(ret) <- ifelse(nms %in% names(rename), rename, nms)
  }

  return(ret)
}


# Utilities ---------------------------------------------------------------


#' @description A function to compare two by groups, passed as data frames
#' @noRd
dfcomp <- function(df1, df2) {
  ret <- FALSE
  
  if (is.null(df1) && is.null(df2))
    ret <- TRUE
  else if (is.na(df1) && is.na(df2))
    ret <- TRUE
  else if (is.data.frame(df1) && is.data.frame(df2)) {
    
    for (i in seq_along(df1)) {
      if (df1[[i]] != df2[[i]])
        return(FALSE)
    }
    ret <- TRUE
    
  } else if (class(df1) == class(df1)) {
    
    ret <- df1 == df2
    
  }
  
  return(ret)
}



