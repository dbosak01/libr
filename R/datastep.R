

# Datastep Definition -----------------------------------------------------


#' @title Step through data row-by-row
#' @description The \code{datastep} function allows you to perform
#' row-wise conditional processing on a data frame or tibble. The function
#' contains parameters to drop, keep, or rename variables.  It also 
#' has the ability to perform by-group processing, and identify first
#' and last rows in a group.
#' @details 
#' Note that the \code{keep}, \code{drop}, and \code{rename} parameters
#' are output parameters.  These parameters will be applied after the
#' data step statements are executed.  Therefore, within the data step, 
#' refer to variables using the input variable name. New variables may 
#' be created on the fly, just by assigning a value to the new
#' variable name.
#' 
#' The \code{keep}, \code{drop}, and \code{rename} parameters require 
#' quoted variable names, as the variables may not yet exist at the 
#' time they are passed into the function.  Within a data step or 
#' calculate function, however, 
#' variable names do not need to be quoted. 
#' 
#' The \code{datastep} function provides four automatic variables. These 
#' variables are generated for every data step, and can 
#' be accessed at any point within the data step: 
#' \itemize{
#'   \item{\strong{data}: Represents the input data frame.}
#'   \item{\strong{n.}: Contains the row number.}
#'   \item{\strong{first.}: Indicates the beginning of a by-group.}
#'   \item{\strong{last.}: Indicates the end of a by-group.}
#' }
#' Automatic variables will be dropped from the data frame at the end
#' of the data step.  If you wish to keep the automatic variable values,
#' assign the automatic variable to a new variable and keep that variable.
#' 
#' The \code{calculate} parameter is used to perform vectorized functions
#' on the data prior to executing the data step.  For example, you 
#' may want to determine a mean for a variable in the \code{calculate}
#' block, and then make decisions on that mean in the data step block. 
#' 
#' The \code{retain} parameter allows you to access the prior row value.
#' At the start of the data step, the retained variable is seeded with the 
#' initial value. For each subsequent step, the variable is seeded with the
#' value of the prior step/row.  This functionality allows you to increment 
#' values or perform cumulative operations.
#' 
#' Note that the data step is pipe-friendly.  It can be used within 
#' a \strong{dplyr} pipeline.  The data step allows you to perform
#' deeply nested and complex conditionals within the pipeline.  The data
#' step is also very readable compared to other pipeline conditionals.
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
#' variable name should be on the left hand side of the name/value pair,
#' and the new variable name should be on the right.  The rename operation
#' is performed after the data step, the keep, and the drop.  Therefore, 
#' the data steps should use the input variable name.  By default, all variables
#' retain their original names.
#' @param by A vector of quoted variable names to use for by-group processing.
#' This parameter will activate the \code{first.} and \code{last.} automatic
#' variables, that indicate the first or last rows in a group.  These 
#' automatic variables are useful for conditional processing on groups.
#' @param calculate Steps to set up calculated variables.  
#' Calculated variables are commonly performed with summary functions such as
#' \code{mean}, \code{median}, \code{min}, \code{max}, etc.  It is more 
#' efficient to set up calculated variables with the calculate parameter and then 
#' use those variables in the data step, rather than perform the summary
#' function inside the data step.
#' @param retain A list of variable names and initial values 
#' to retain.  Retained variables will begin the data step with the initial
#' value.  Then for each iteration of the data step, the variable will
#' be populated with the ending value from the previous step.  The retain
#' functionality allows you to perform cumulative operations or decisions
#' based on the value of the previous iteration of the data step.  Initial 
#' values should be of the expected data type for the column.  For example, 
#' for a numeric column set the initial value to a zero, and for a
#' character column, set the initial value to an empty string, i.e.
#' \code{retain = list(col1 = 0, col2 = "")}.  There is no default initial 
#' value for a variable.  You must supply an initial value for each retained
#' variable.
#' @param arrays A named list of \code{\link{dsarray}} objects.
#' @param attrib A named list of \code{\link{dsattr}} objects.
#' @param sort_check Checks to see if the input data is sorted according to
#' the \code{by} variable parameter.  The sort check will give an error
#' if the input data is not sorted according to the \code{by} variable.
#' The check is turned on if the value of 
#' \code{sort_check} is TRUE, and turned off if FALSE.  The default value
#' is TRUE.  Turn the sort check off if you want to perform by-group 
#' processing on unsorted data, or data that is not sorted according
#' to the by-group.
#' @return The processed data frame or tibble.  
#' @family datastep
#' @examples 
#' # Example #1: Simple Data Step
#' df <- datastep(mtcars[1:10,], 
#'                keep = c("mpg", "cyl", "disp", "mpgcat", "recdt"), {
#'                  
#'   if (mpg >= 20) 
#'     mpgcat <- "High"
#'   else 
#'     mpgcat <- "Low"
#'                  
#'   recdt <- as.Date("1974-06-10")
#'                  
#'   if (cyl == 8)
#'     is8cyl <- TRUE
#'                  
#' })
#' 
#' df
#' #                    mpg cyl  disp mpgcat      recdt
#' # Mazda RX4         21.0   6 160.0   High 1974-06-10
#' # Mazda RX4 Wag     21.0   6 160.0   High 1974-06-10
#' # Datsun 710        22.8   4 108.0   High 1974-06-10
#' # Hornet 4 Drive    21.4   6 258.0   High 1974-06-10
#' # Hornet Sportabout 18.7   8 360.0    Low 1974-06-10
#' # Valiant           18.1   6 225.0    Low 1974-06-10
#' # Duster 360        14.3   8 360.0    Low 1974-06-10
#' # Merc 240D         24.4   4 146.7   High 1974-06-10
#' # Merc 230          22.8   4 140.8   High 1974-06-10
#' # Merc 280          19.2   6 167.6    Low 1974-06-10
#'
#' # Example #2: By-group Processing
#' df <- datastep(mtcars[1:10,], 
#'                keep = c("mpg", "cyl", "gear", "grp"), 
#'                by = c("gear"), sort_check = FALSE, {
#'                  
#'   if (first.)
#'     grp <- "Start"
#'   else if (last.)
#'     grp <- "End"
#'   else 
#'     grp <- "-"
#'                  
#' })
#' 
#' df
#' #                    mpg cyl gear   grp
#' # Mazda RX4         21.0   6    4 Start
#' # Mazda RX4 Wag     21.0   6    4     -
#' # Datsun 710        22.8   4    4   End
#' # Hornet 4 Drive    21.4   6    3 Start
#' # Hornet Sportabout 18.7   8    3     -
#' # Valiant           18.1   6    3     -
#' # Duster 360        14.3   8    3   End
#' # Merc 240D         24.4   4    4 Start
#' # Merc 230          22.8   4    4     -
#' # Merc 280          19.2   6    4   End 
#' 
#' # Example #3: Calculate Block
#' df <- datastep(mtcars, 
#'                keep = c("mpg", "cyl", "mean_mpg", "mpgcat"), 
#'                calculate = { mean_mpg = mean(mpg) }, {
#'                  
#'   if (mpg >= mean_mpg)
#'     mpgcat <- "High"
#'   else 
#'     mpgcat <- "Low"
#'                  
#' })
#' 
#' df[1:10,]
#' #                    mpg cyl mean_mpg mpgcat
#' # Mazda RX4         21.0   6 20.09062   High
#' # Mazda RX4 Wag     21.0   6 20.09062   High
#' # Datsun 710        22.8   4 20.09062   High
#' # Hornet 4 Drive    21.4   6 20.09062   High
#' # Hornet Sportabout 18.7   8 20.09062    Low
#' # Valiant           18.1   6 20.09062    Low
#' # Duster 360        14.3   8 20.09062    Low
#' # Merc 240D         24.4   4 20.09062   High
#' # Merc 230          22.8   4 20.09062   High
#' # Merc 280          19.2   6 20.09062    Low
#'
#' # Example #4: Data pipeline
#' library(dplyr)
#' library(magrittr)
#' 
#' # Add datastep to dplyr pipeline
#' df <- mtcars %>% 
#'   select(mpg, cyl, gear) %>% 
#'   mutate(mean_mpg = mean(mpg)) %>% 
#'   datastep({
#'     
#'     if (mpg >= mean_mpg)
#'       mpgcat <- "High"
#'     else 
#'       mpgcat <- "Low"
#'     
#'   }) %>% 
#'   filter(row_number() <= 10)
#' 
#' df
#' #     mpg cyl gear mean_mpg mpgcat
#' # 1  21.0   6    4 20.09062   High
#' # 2  21.0   6    4 20.09062   High
#' # 3  22.8   4    4 20.09062   High
#' # 4  21.4   6    3 20.09062   High
#' # 5  18.7   8    3 20.09062    Low
#' # 6  18.1   6    3 20.09062    Low
#' # 7  14.3   8    3 20.09062    Low
#' # 8  24.4   4    4 20.09062   High
#' # 9  22.8   4    4 20.09062   High
#' # 10 19.2   6    4 20.09062    Low
#' 
#' # Example #5: Drop, Retain and Rename
#' df <- datastep(mtcars[1:10, ], 
#'                drop = c("disp", "hp", "drat", "qsec", 
#'                         "vs", "am", "gear", "carb"), 
#'                retain = list(cumwt = 0 ),
#'                rename = c(mpg = "MPG", cyl = "Cylinders", wt = "Wgt", 
#'                           cumwt = "Cumulative Wgt"), {
#'                  
#'   cumwt <- cumwt + wt
#'                  
#' })
#' 
#' df
#' #                    MPG Cylinders   Wgt Cumulative Wgt
#' # Mazda RX4         21.0         6 2.620          2.620
#' # Mazda RX4 Wag     21.0         6 2.875          5.495
#' # Datsun 710        22.8         4 2.320          7.815
#' # Hornet 4 Drive    21.4         6 3.215         11.030
#' # Hornet Sportabout 18.7         8 3.440         14.470
#' # Valiant           18.1         6 3.460         17.930
#' # Duster 360        14.3         8 3.570         21.500
#' # Merc 240D         24.4         4 3.190         24.690
#' # Merc 230          22.8         4 3.150         27.840
#' # Merc 280          19.2         6 3.440         31.280
#' @import dplyr
#' @export
datastep <- function(data, steps, keep = NULL,
                     drop = NULL, rename = NULL,
                     by = NULL, calculate = NULL,
                     retain = NULL, arrays = NULL,
                     attrib = NULL,
                     sort_check = TRUE) {
  
  if (!"data.frame" %in% class(data))
    stop("input data must be inherited from data.frame")
  
  
  if (!is.null(retain)) {
    if (!"list" %in% class(retain))
      stop("retain parameter value must be of class 'list'")
    
  }
  
  # Capture number of starting columns
  startcols <- ncol(data)
  
  # Apply variable attributes
  if (!is.null(attrib)) {
    for (nm in names(attrib)) { 
      if ("dsattr" %in% class(attrib[[nm]])) {
        
        # If the attrib is a dsattr
        if (!nm %in% names(data)) {
          data[[nm]] <- attrib[[nm]][["default"]]
        }
        for (at in names(attrib[[nm]])) {
          
          if (at != "class")
            attr(data[[nm]], at) <-  attrib[[nm]][[at]]
          
        }
      } else {
        
        # If the attrib is not a dsattr, use as default value
        if (!nm %in% names(data)) {
          data[[nm]] <- attrib[[nm]]
        }
        
        
      }
    }
  }
  
  # Assign arrays to variables in this environment
  if (!is.null(arrays)) {
    for (nm in names(arrays)) {
      
      assign(nm, arrays[[nm]]) 
      
    }
  }
  
  # Put code in a variable for safe-keeping
  code <- substitute(steps, env = environment())
  
  # Put aggregate functions in a variable 
  agg <- substitute(calculate, env = environment())
  if (paste0(deparse(agg), collapse = "") != "NULL") {
    data <- within(data, eval(agg), keepAttrs = TRUE)
  }
  
  ret <- list()
  firstval <- NULL
  firstvals <- list()
  rowcount <- nrow(data)
  orig_class <- class(data)
  
  # Set by if data is a grouped tibble
  if (is.null(by) && "grouped_df" %in% class(data)) {
    if (!is.null(attr(data, "groups"))) {
      grpdf <- attr(data, "groups")
      nms <- names(grpdf)
      if (!is.null(nms)) {
        nms <- nms[nms != ".rows"]
        if (length(nms) > 0) {
          by <- nms
          
        }
      }
    }
  }
  
  # Save off any attributes
  if (ncol(data) > 1) {
    # Deal with 1 column situation
    data_attributes <- data[1, ]
  } else {
    data_attributes <- data.frame(data[1, ])
    names(data_attributes) <- names(data)
  }
  # Tibble subset will keep attributes, but data.frame will not
  if (!"tbl_df" %in% class(data)) {
    data_attributes <- copy_attributes(data, data_attributes)
    
  }
  
  # For some reason the grouped tibble kills performance.
  # Temporarily convert to a data frame.  
  # Seriously like 20X performance increase.
  if (any("grouped_df" == class(data)))
    data <- as.data.frame(data)
  
  # Add automatic variables
  data <- add_autos(data, by, sort_check)
  
  # Step through row by row
  for (n. in seq_len(rowcount)) {
    
    # If one column, subset comes back with a vector
    if (ncol(data) > 1)
      rw <- data[n., ]
    else {
      rw <- data.frame(data[n., ])
      names(rw) <- names(data)
    }
    
    
    
    
    # Deal with retained variables
    if (!is.null(retain)) {
      if (length(ret) == 0) {
        for (nm in names(retain)) {
          
          # Populate with initial value
          rw[[nm]] <- retain[[nm]]
          
        }
        
      } else {
        for (nm in names(retain)) {
          
          # Populate with value from previous row   
          #data[n., nm] <- ret[n. - 1, nm]  way backup
          
          rw[[nm]] <- ret[[n. - 1]][[nm]] # current
          
          
        }
      }
    }
    
    # Evaluate the code for the row
    ret[[n.]]  <- within(rw, eval(code), keepAttrs = TRUE)
    
  }
  
  # Bind all rows
  ret <- bind_rows(ret, .id = "column_label")
  ret["column_label"] <- NULL
  
  
  # Remove automatic variables
  ret["n."] <- NULL
  ret["first."] <- NULL
  ret["last."] <- NULL

  
  # Perform drop operation
  if (!is.null(drop))
    ret <- ret[ , !names(ret) %in% drop]
  
  # Perform keep operation
  if (!is.null(keep)) {
    ret <- ret[ , keep]
  }
  
  
  # Convert back to tibble if original was a tibble
  if ("tbl_df" %in% orig_class & !"tbl_df" %in% class(ret)) {
    ret <- as_tibble(ret)
  }
  
  # Put back grouping attributes if original data was grouped
  if (!is.null(by) & "grouped_df" %in% orig_class) {
    
    if (all(by %in% names(ret)))
      ret <- group_by(ret, across({{by}})) 
    
  }
  
  # Restore attributes from original data 
  ret <- copy_attributes(data_attributes, ret)
  
  
  # Perform rename operation
  if (!is.null(rename)) {
    nms <- names(ret)
    names(ret) <- ifelse(nms %in% names(rename), rename, nms)
  }
  
  endcols <- ncol(ret)
  if (startcols > endcols)
    log_logr(paste0("datastep: columns increased from ", startcols, " to ", 
                    endcols))
  else if (startcols < endcols)
    log_logr(paste0("datastep: columns decreased from ", startcols, " to ", 
                    endcols))
  else 
    log_logr(paste0("datastep: columns started with ", startcols, 
                    " and ended with ", endcols))
  
  return(ret)
}




#' @noRd
datastep_back <- function(data, steps, keep = NULL,
                     drop = NULL, rename = NULL,
                     by = NULL, calculate = NULL,
                     retain = NULL,
                     sort_check = TRUE) {
  
  if (!"data.frame" %in% class(data))
    stop("input data must be inherited from data.frame")
  
  
  if (!is.null(retain)) {
    if (!"list" %in% class(retain))
      stop("retain parameter value must be of class 'list'")
    
  }
  
  # Capture number of starting columns
  startcols <- ncol(data)
  
  # Put code in a variable for safe-keeping
  code <- substitute(steps, env = environment())
  
  # Put aggregate functions in a variable 
  agg <- substitute(calculate, env = environment())
  if (paste0(deparse(agg), collapse = "") != "NULL") {
    data <- within(data, eval(agg), keepAttrs = TRUE)
  }
  
  ret <- list()
  firstval <- NULL
  firstvals <- list()
  rowcount <- nrow(data)
  orig_class <- class(data)
  
  # Set by if data is a grouped tibble
  if (is.null(by) && "grouped_df" %in% class(data)) {
    if (!is.null(attr(data, "groups"))) {
      grpdf <- attr(data, "groups")
      nms <- names(grpdf)
      if (!is.null(nms)) {
        nms <- nms[nms != ".rows"]
        if (length(nms) > 0) {
          by <- nms
          
        }
      }
    }
  }
  
  # Deal with 1 column situation
  if (ncol(data) > 1) {
    data_attributes <- data[1, ]
  } else {
    data_attributes <- data.frame(data[1, ])
    names(data_attributes) <- names(data)
  }
  
  
  # For some reason the grouped tibble kills performance.
  # Temporarily convert to a data frame.  
  # Seriously like 20X performance increase.
  if (all("data.frame" != class(data)))
    data <- as.data.frame(data)

  
  # Increases performance
  if (!is.null(by)) {
    bydata <- as.data.frame(data[ , by])
  }
  
  
  # Step through row by row
  for (n. in seq_len(rowcount)) {
    
    # If one column, subset comes back with a vector
    if (ncol(data) > 1)
      rw <- data[n., ]
    else {
      rw <- data.frame(data[n., ])
      names(rw) <- names(data)
    }
    
    if (!is.null(by))
      byrw <- rw[1, by]
    
    # Deal with first. and last.
    # These can be accessed from within the evaluated code,
    # which is really cool.
    if (!is.null(by)) {
      if (is.null(firstval)) {
        firstval <- byrw
        if (length(names(firstval)) == 0)
          names(firstval) <- by
        first. <- TRUE
      } else {
        
        # Compare current by group to previous row
        if (dfcomp(firstval, byrw) == FALSE) {
          first. <- TRUE
          firstval <- byrw
          if (length(names(firstval)) == 0)
            names(firstval) <- by
        } else {
          first. <- FALSE
        }
      }
      
      # If it's the last row of the data frame, mark last.
      if (n. == rowcount) {
        last. <- TRUE
      } else {
        
        # Compare by group to next row to determine last.
        # print(bydata)
        # print(n.)
        # print(bydata[n. + 1, ])
        # print(byrw)
        if (dfcomp(bydata[n. + 1, ],  byrw) == FALSE) {
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
      if (n. == rowcount)
        last. <- TRUE
      else
        last. <- FALSE
      
    }
    
    
    # Deal with retained variables
    if (!is.null(retain)) {
      if (length(ret) == 0) {
        for (nm in names(retain)) {
          
          # Populate with initial value
          rw[[nm]] <- retain[[nm]]
          
        }
        
      } else {
        for (nm in names(retain)) {
          
          # Populate with value from previous row   
          #data[n., nm] <- ret[n. - 1, nm]  way backup
          
          rw[[nm]] <- ret[[n. - 1]][[nm]] # current
          
          
        }
      }
    }
    
    # Evaluate the code for the row
    ret[[n.]]  <- within(rw, eval(code), keepAttrs = TRUE)
    
    
    # Keep track of the groups
    if (!is.null(by) & first. & sort_check) {
      firstvals[[length(firstvals) + 1]] <- firstval
    }
  }
  
  # Bind all rows
  ret <- bind_rows(ret, .id = "column_label")
  ret["column_label"] <- NULL
  
  if (sort_check & !is.null(by)) {
    if (length(firstvals) > 0) {
      d <- bind_rows(firstvals, .id = "column_label")
      d["column_label"] <- NULL
      ddat <- distinct(d)
      if (nrow(ddat) != nrow(d)) {
        stop(paste("Input data is not sorted according to the 'by' variable",
                   "parameter.\n  Either sort the input data properly or",
                   "set the sort_check parameter to FALSE."))
      }
    }
  }
  
  # Perform drop operation
  if (!is.null(drop))
    ret <- ret[ , !names(ret) %in% drop]
  
  # Perform keep operation
  if (!is.null(keep)) {
    ret <- ret[ , keep]
  }
  
  
  # Convert back to tibble if original was a tibble
  if ("tbl_df" %in% orig_class & !"tbl_df" %in% class(ret)) {
    ret <- as_tibble(ret)
  }
  
  # Put back grouping attributes if original data was grouped
  if (!is.null(by) & "grouped_df" %in% orig_class) {
    
    if (all(by %in% names(ret)))
      ret <- group_by(ret, across({{by}})) 
    
  }
  
  # Restore attributes from original data 
  ret <- copy_attributes(data_attributes, ret)
  
  
  # Perform rename operation
  if (!is.null(rename)) {
    nms <- names(ret)
    names(ret) <- ifelse(nms %in% names(rename), rename, nms)
  }
  
  endcols <- ncol(ret)
  if (startcols > endcols)
    log_logr(paste0("datastep: columns increased from ", startcols, " to ", 
                    endcols))
  else if (startcols < endcols)
    log_logr(paste0("datastep: columns decreased from ", startcols, " to ", 
                    endcols))
  else 
    log_logr(paste0("datastep: columns started with ", startcols, 
                    " and ended with ", endcols))
  
  return(ret)
}



# Utilities ---------------------------------------------------------------


#' @description A function to compare two by groups, passed as data frames
#' @noRd
dfcomp <- function(df1, df2) {
  names(df1) <- NULL
  names(df2) <- NULL
  ret <- FALSE

  if (all(is.null(df1)) && all(is.null(df2)))
    ret <- TRUE
  else if (all(is.na(df1)) && all(is.na(df2)))
    ret <- TRUE
  if (all(is.data.frame(df1) && is.data.frame(df2))) {

    for (i in seq_along(df1)) {
      if (any(df1[[i]] != df2[[i]]))
        return(FALSE)
    }
    ret <- TRUE

  } else if (all(class(df1) == class(df1))) {

    ret <- all(df1 == df2)

  }
  
  return(ret)
}


#' @noRd
copy_attributes <- function(df1, df2) {
  
  ret <- df2
  
  for (nm in names(df2)) {
    
    attributes(ret[[nm]]) <- attributes(df1[[nm]])
    
    # col <- df1[[nm]]
    # for (at in names(attributes(col))) {
    #   
    #   attr(ret[[nm]], at) <- attr(col, at)
    #   
    # }
    
  }
  
  return(ret)
}

