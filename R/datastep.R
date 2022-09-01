# Set up environment for shared variables
e <- new.env(parent = emptyenv())
e$output <- list()

# Datastep Definition -----------------------------------------------------


#' @title Step through data row-by-row
#' @description The \code{datastep} function allows you to perform
#' row-wise conditional processing on a data frame, data table, or tibble. 
#' The function
#' contains parameters to drop, keep, or rename variables, perform
#' by-group processing, and perform row-wise or column-wise calculations.  
#' @details 
#' Two parameters on the \code{datastep} function are required: 
#' \strong{data} and \strong{steps}.  The \strong{data} parameter is
#' the input data to the data step.  The \strong{steps} parameter contains
#' the code statements you want to apply to the data. The \strong{steps}
#' should be wrapped in curly braces.  When running, the data step
#' will loop through the input data row-by-row, and execute the steps for 
#' each row.  Variables inside the data step can be accessed using 
#' non-standard evaluation (meaning they  do not have to be quoted).
#' 
#' Note that the data step is pipe-friendly.  It can be used within 
#' a \strong{dplyr} pipeline.  The data step allows you to perform
#' deeply nested and complex conditionals within the pipeline.  The data
#' step is also very readable compared to other pipeline conditionals.
#' 
#' @section Automatic Variables:
#' The \code{datastep} function provides five automatic variables. These 
#' variables are generated for every data step, and can 
#' be accessed at any point within the data step: 
#' \itemize{
#'   \item{\strong{data}: Represents the entire input data frame.}
#'   \item{\strong{rw}: Represents the current row.}
#'   \item{\strong{n.}: Contains the row number.}
#'   \item{\strong{first.}: Indicates the beginning of a by-group.}
#'   \item{\strong{last.}: Indicates the end of a by-group.}
#' }
#' Automatic variables will be dropped from the data frame at the end
#' of the data step.  If you wish to keep the automatic variable values,
#' assign the automatic variable to a new variable and keep that variable.
#' 
#' @section Column Attributes:
#' To set attributes for a column on your data, use the \code{attrib}
#' parameter.  Example attributes include 'label', 'description', 
#' and 'format'.  These types of attributes are set using a named list and a 
#' \code{\link{dsattr}} object. The name of the list item
#' is the column name you want to set attributes on. 
#' The value of the list item is the \code{dsattr} object.
#' For a complete list of available attributes, 
#' see the \code{\link{dsattr}} documentation.
#' 
#' It should be mentioned  that the \code{dsattr} object is not required.  
#' You can also set attributes with a name and a default value.  
#' The default value can be any valid data value, such as a number or string.
#' 
#' The label and format attributes may also be set with the 'label' and 
#' 'format' parameters. These parameters accept a named list with the 
#' labels or formats, and will be assigned to the output data frame.  
#' 
#' @section Optional Parameters:
#' Optional parameters on the \code{datastep} allow you to shape 
#' the output dataset or enhance the operation of the \code{datastep}.  Some
#' parameters are classified as input parameters, and others as output 
#' parameters.  Input parameters modify the data before the data step
#' operations takes place.  Output parameters operate on the data
#' after the data step.
#' 
#' The \code{keep}, \code{drop}, and \code{rename} parameters
#' are output parameters.  These parameters will be applied after the
#' data step statements are executed.  Therefore, within the data step, 
#' refer to variables using the input variable name. New variables may 
#' be created on the fly, just by assigning a value to the new
#' variable name.
#' 
#' The \code{keep}, \code{drop}, and \code{rename} parameters require 
#' quoted variable names, as the variables may not yet exist at the 
#' time they are passed into the function.  Within a data step or 
#' calculate block, however, 
#' variable names do not need to be quoted. 
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
#' \code{calculate} and \code{retain} are both input parameters.
#' 
#' @section Data Step Arrays:
#' There are times you may want to iterate over columns in your data step.  Such 
#' iteration is particularly useful when you have a wide dataset,
#' and wish to perform the same operation on several columns.
#' For instance, you may want to calculate the mean for 10 different
#' variables on your dataset.
#' 
#' The \code{arrays} parameter allows you to iterate across columns.  
#' This parameter accepts a named list of 
#' \code{\link{dsarray}} objects.  The \code{dsarray} is essentially
#' a list of columns.  You can use a \code{for} loop to iterate over the
#' \code{dsarray}, and also send it into a vectorized function.  Data 
#' step arrays allow to you to perform row-wise calculations. 
#' For instance, you can calculate 
#' a sum or mean by row for the variables in your array.
#' 
#' @section Output Column Order:
#' By default, the data step will retain the column order of any variables that
#' already exist on the input data set. New variables created 
#' in a data step will be appended to the right of existing variables.  
#' Yet these new variables can sometimes appear in an order that is 
#' unexpected or undesirable.  
#' 
#' There are two ways to control the order of output columns:
#' the \code{keep} parameter and the \code{attrib} parameter.
#' 
#' Columns names included on the 
#' \code{keep} parameter will appear in the order indicated on the keep
#' vector.  This ordering mechanism is appropriate when you have a small
#' number of columns and can easily pass the entire keep list.
#' 
#' To control the order of new variables only, use the \code{attrib} parameter.
#' New variables for which attributes are defined will appear in the 
#' order indicated on the \code{attrib} list.  The \code{attrib} list
#' is useful when you are adding a relatively small number of columns to 
#' an existing data set, and don't want to pass all the column names. 
#' 
#' Remember
#' that you can supply an attribute list with default values only,
#' such as \code{attrib = list(column1 = 0, column2 = "")}.  This style of 
#' attribute definition is convenient if you are only trying to control
#' the order of columns.
#' 
#' If the above two mechanisms to control column order are not sufficient,
#' use the data frame subset operators or column ordering functions 
#' provided by other packages.
#' 
#' @section Datastep Performance:
#' The \code{datastep} is intended to be used on small and medium-sized 
#' datasets.  It is not recommended for large datasets.
#' If your dataset is greater than one million rows, you should consider
#' other techniques for processing your data.  While there is no 
#' built-in restriction on the number of rows, performance of the
#' \code{datastep} can become unacceptable with a large number of rows.
#' @param data The data to step through.
#' @param steps The operations to perform on the data.  This parameter is 
#' specified as a set of R statements contained within 
#' curly braces. If no steps are desired, pass empty curly braces.
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
#' Calculated variables are commonly generated with summary functions such as
#' \code{mean}, \code{median}, \code{min}, \code{max}, etc.  It is more 
#' efficient to set up calculated variables with the calculate parameter and then 
#' use those variables in the data step, rather than perform the summary
#' function inside the data step.  The calculate block will be executed 
#' immediately before the data step.
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
#' @param attrib A named list of attributes.  The list can be either
#' \code{\link{dsattr}} objects or single default values.  The \code{dsattr}
#' object allows you to set more attributes on each column.  The 
#' single default value is convenient if you simply want to create a variable.
#' By default, variables will be created on the fly with no attributes.
#' @param arrays A named list of \code{\link{dsarray}} objects. The 
#' \code{dsarray} is a list of columns which you can 
#' iterate over inside the data step.  You can iterate over a \code{dsarray}
#' either with a \code{for} loop, or with a vectorized function. 
#' The default value of the \code{arrays} parameter is NULL, meaning
#' no arrays are defined.
#' @param sort_check Checks to see if the input data is sorted according to
#' the \code{by} variable parameter.  The sort check will give an error
#' if the input data is not sorted according to the \code{by} variable.
#' The check is turned on if the value of 
#' \code{sort_check} is TRUE, and turned off if FALSE.  The default value
#' is TRUE.  Turn the sort check off if you want to perform by-group 
#' processing on unsorted data, or data that is not sorted according
#' to the by-group.
#' @param format A named list of formats to assign to the data
#' frame.  Formats will be assigned both before and after the datastep.
#' @param label A named list of labels to assign to the output data frame.
#' @param where An expression to filter the output dataset.  The where
#' clause will be applied prior to any drop, keep, or rename statement.
#' Use the \code{expression} function to assign the where clause.
#' @param set A dataset or list of datasets to append to the input 
#' data frame.  The set operation will occur at the beginning of the datastep,
#' prior to the execution of any steps.
#' @param merge A dataset or list of datasets to merge with the input
#' data.  The merge operation will occur at the beginning of the datastep,
#' prior to the execution of any steps.  When the \code{merge} operation is 
#' requested, the \code{by} parameter will be used to indicate which variable(s)
#' to merge by.  
#' @param merge_by If the \code{merge} parameter is set, the \code{merge_by} 
#' parameter will be used to identify the variable(s) to merge by. If merge 
#' variables are the same on both datasets, the names may be passed as a simple 
#' quoted vector. If the variable names are different, pass the variables 
#' to merge on as a named vector.  For example, \code{c(ITEMID = ITEMCODE)}
#' would specify that the join should occur on the "ITEMID" from the 
#' dataset specified in the \code{data} parameter, and the "ITEMCODE"
#' variable from the dataset specified on the \code{merge} parameter.
#' @param merge_in A vector of column names to be used to hold the merge flags.  
#' The column names should correspond to the datasets being merged. The
#' merge flags will contains 0 or 1 values to indicate whether the record
#' came from the corresponding table.
#' @return The processed data frame, tibble, or data table.  
#' @family datastep
#' @seealso \code{\link{libname}} function to create a data library, and
#' the \code{\link{dictionary}} function to create a data dictionary.
#' @examples 
#' # Example #1: Simple Data Step
#' df <- datastep(mtcars[1:10,], 
#'                keep = c("mpg", "cyl", "disp", "mpgcat", "recdt", "is8cyl"), {
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
#'   else 
#'     is8cyl <- FALSE
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
#' 
#' # Example #6: Attributes and Arrays
#' 
#' # Create sample data
#' dat <- read.table(header = TRUE, text = '
#'    Year  Q1   Q2  Q3  Q4
#'    2000 125  137 152 140
#'    2001 132  145 138  87
#'    2002 101  104 115 121')
#'  
#' # Use attrib list to control column order and add labels
#' # Use array to calculate row sums and means, and get best quarter
#' df <- datastep(dat,
#'                attrib = list(Tot = dsattr(0, label = "Year Total"),
#'                              Avg = dsattr(0, label = "Year Average"),
#'                              Best = dsattr(0, label = "Best Quarter")),
#'                arrays = list(qtrs = dsarray("Q1", "Q2", "Q3", "Q4")),
#'                drop = "q",
#'                steps = {
#'                
#'                  # Empty brackets return all array values
#'                  Tot <- sum(qtrs[])
#'                  Avg <- mean(qtrs[])
#'                  
#'                  # Iterate to find best quarter
#'                  for (q in qtrs) {
#'                    if (qtrs[q] == max(qtrs[]))
#'                      Best <- q
#'                  }
#'                })
#'                
#' df
#' #   Year  Q1  Q2  Q3  Q4 Tot    Avg Best
#' # 1 2000 125 137 152 140 554 138.50   Q3
#' # 2 2001 132 145 138  87 502 125.50   Q2
#' # 3 2002 101 104 115 121 441 110.25   Q4
#' 
#' dictionary(df)
#' #   A tibble: 8 x 10
#' #   Name  Column Class     Label        Description Format Width Justify  Rows   NAs
#' #   <chr> <chr>  <chr>     <chr>        <chr>       <lgl>  <int> <chr>   <int> <int>
#' # 1 df    Year   integer   NA           NA          NA        NA NA          3     0
#' # 2 df    Q1     integer   NA           NA          NA        NA NA          3     0
#' # 3 df    Q2     integer   NA           NA          NA        NA NA          3     0
#' # 4 df    Q3     integer   NA           NA          NA        NA NA          3     0
#' # 5 df    Q4     integer   NA           NA          NA        NA NA          3     0
#' # 6 df    Tot    integer   Year Total   NA          NA        NA NA          3     0
#' # 7 df    Avg    numeric   Year Average NA          NA        NA NA          3     0
#' # 8 df    Best   character Best Quarter NA          NA         2 NA          3     0
#' 
#' @import dplyr
#' @export
datastep <- function(data, steps, keep = NULL,
                     drop = NULL, rename = NULL,
                     by = NULL, calculate = NULL,
                     retain = NULL, attrib = NULL,
                     arrays = NULL,
                     sort_check = TRUE,
                     format = NULL,
                     label = NULL,
                     where = NULL, 
                     set = NULL,
                     merge = NULL,
                     merge_by = NULL,
                     merge_in = NULL) {
  
  if (!"data.frame" %in% class(data))
    stop("input data must be inherited from data.frame")
  
  
  if (!is.null(retain)) {
    if (!"list" %in% class(retain))
      stop("retain parameter value must be of class 'list'")
    
  }
  
  if (!is.null(attrib)) {
    if (!"list" %in% class(attrib))
      stop("attrib parameter value must be of class 'list'")
    
  }
  
  if (!is.null(arrays)) {
    if (!"list" %in% class(arrays))
      stop("arrays parameter value must be of class 'list'")
    
  }
  
  # Deal with single value unquoted parameter values
  oby <- deparse(substitute(by, env = environment()))
  by <- tryCatch({if (typeof(by) %in% c("character", "NULL")) by else oby},
                 error = function(cond) {oby})
  
  odrop <- deparse(substitute(drop, env = environment()))
  drop <- tryCatch({if (typeof(drop) %in% c("character", "NULL")) drop else odrop},
                 error = function(cond) {odrop})
  
  okeep <- deparse(substitute(keep, env = environment()))
  keep <- tryCatch({if (typeof(keep) %in% c("character", "NULL")) keep else okeep},
                 error = function(cond) {okeep})
  
  omby <- deparse(substitute(merge_by, env = environment()))
  merge_by <- tryCatch({if (typeof(merge_by) %in% c("character", "NULL")) merge_by else omby},
                   error = function(cond) {omby})
  
  
  # Capture number of starting columns
  startcols <- ncol(data)
  
  # Put code in a variable for safe-keeping
  code <- substitute(steps, env = environment())
  
  # Determine if there is an output function
  hout <- has_output(deparse(code))
  
  # Give warning if there are no rows and no output()
  if (hout == FALSE & nrow(data) == 0) {
    warning("Input dataset has no rows.") 
  }
  
  # Save off incoming dataset class
  dataclass <- class(data)
  
  # Deal with set parameter
  if (!is.null(set)) {
    data <- perform_set(data, set)
  }
  
  # Deal with merge parameter
  if (!is.null(merge)) {
    data <- perform_merge(data, merge, merge_by, merge_in) 
  }
  
  # Clear output list
  e$output <- list()
  
  # Put aggregate functions in a variable 
  agg <- substitute(calculate, env = environment())
  
  # Execute aggregate functions
  if (paste0(deparse(agg), collapse = "") != "NULL") {
    data <- within(data, eval(agg), keepAttrs = TRUE)
  }
  
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
  # Otherwise they won't be accessible from datastep code
  if (!is.null(arrays)) {
    for (nm in names(arrays)) {
      
      assign(nm, arrays[[nm]]) 
      
    }
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
  data_attributes <- data[1, , drop = FALSE]
  
  # Tibble subset will keep attributes, but data.frame will not
  if (!"tbl_df" %in% class(data)) {
    data_attributes <- copy_attributes(data, data_attributes)
    
  }
  if (!is.null(format)) {
    data_attributes <- assign_attributes(data_attributes, format, "format")
  }
  
  # For some reason the grouped tibble kills performance.
  # Temporarily convert to a data frame.  
  # Seriously like 20X performance increase.
  if (any("grouped_df" == class(data)))
    data <- as.data.frame(data, stringsAsFactors = FALSE)
  
  # data.table is not that bad, but data.frame is better.
  if (any("data.table" == class(data)))
    data <- as.data.frame(data, stringsAsFactors = FALSE)
  
  # Strip any crazy classes, as they can mess up datastep functions
  data_classes <- class(data)
  if (any(!class(data) %in% c("data.frame", "list"))) {
    data <- as.data.frame(unclass(data), stringsAsFactors = FALSE) 
  }
  
  # Add automatic variables
  data <- add_autos(data, by, sort_check)
  
  # Increase rowcount if needed
  if (nrow(data) > rowcount) {
    rowcount <- nrow(data)
  }
  
  # Step through row by row
  for (n. in seq_len(rowcount)) {
    
    # Subset by row
    rw <- data[n., , drop = FALSE]
    
    # Put back any attributes dropped during row subset
    rw <- copy_attributes(data_attributes, rw)
    
    
    
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
    ret[[n.]]  <-  within(rw, eval(code), keepAttrs = TRUE)
    

  }
  
  # Bind all rows
  if (hout) {
    ret <- bind_rows(e$output, .id = "column_label")
    
  } else {
    ret <- bind_rows(ret, .id = "column_label")
  }
  ret["column_label"] <- NULL
  
  
  # Delete
  if ("..delete" %in% names(ret)) {
    ret <- tryCatch({subset(ret, ret[["..delete"]] == FALSE)},
                    error = function(cond){ret})
  }
  
  # Where Before
  if (!is.null(where)) {
    ret <- tryCatch({subset(ret, eval(where))},
                    error = function(cond){ret})
  }
  
  # Improve column order
  rtnms <- rev(names(ret))
  orgnms <- names(data)
  ret <- ret[ ,c(orgnms, rtnms[!rtnms %in% orgnms])]
  
  # Remove automatic variables
  ret["first."] <- NULL
  ret["last."] <- NULL
  ret["..delete"] <- NULL
  
  # Perform drop operation
  if (!is.null(drop)) {
    
    if (!all(drop %in% names(ret))) {
      
      message("Drop parameter '" %p%  drop[!drop %in% names(ret)] %p% 
                "' not found on output dataset.")
      
      drop <-  drop[drop %in% names(ret)] 
    }
    
    ret <- ret[ , !names(ret) %in% drop, drop = FALSE]
    
  }
  
  # Perform keep operation
  if (!is.null(keep)) {
    if (!all(keep %in% names(ret))) {
      
      message("Keep parameter '" %p%  keep[!keep %in% names(ret)] %p% 
              "' not found on output dataset.")
      
      keep <-  keep[keep %in% names(ret)] 
    }
    
    ret <- ret[ , keep, drop = FALSE]
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
  
  # Convert back to tibble if original was a tibble
  if ("data.table" %in% orig_class & !"data.table" %in% class(ret)) {
    ret <- data.table::as.data.table(ret)
  }
  
  # Restore any stripped classes
  if (any(!data_classes %in% class(ret))) {
    
    class(ret) <- data_classes
  }
  
  # Restore attributes from original data 
  ret <- copy_attributes(data_attributes, ret)
  
  
  # Perform rename operation
  if (!is.null(rename)) {
    nms <- names(ret)
    names(ret) <- ifelse(nms %in% names(rename), rename, nms)
  }
  
  # Where After
  if (!is.null(where)) {
    ret <- tryCatch({subset(ret, eval(where))},
                    error = function(cond){ret})
  }
  
  # Labels
  if (!is.null(label)) {
    ret <- assign_attributes(ret, label, "label")
  }
  
  # Formatting
  if (!is.null(format)) {
    ret <- assign_attributes(ret, format, "format")
  }
  
  # Clear out rownames
  rownames(ret) <- NULL
  
  endcols <- ncol(ret)
  if (startcols > endcols)
    log_logr(paste0("datastep: columns decreased from ", startcols, " to ", 
                    endcols))
  else if (startcols < endcols)
    log_logr(paste0("datastep: columns increased from ", startcols, " to ", 
                    endcols))
  else 
    log_logr(paste0("datastep: columns started with ", startcols, 
                    " and ended with ", endcols))
  
  if (log_output()) {
    log_logr(ret)
    print(ret)
  }
  
  return(ret)
}



#' @title Removes an observation from a datastep
#' @description The \code{delete} function will remove an observation
#' from the output of a datastep.  The function takes no parameters.  To use 
#' the function, simply call it on the rows you want to delete.  Typically
#' it is called within a conditional.
#' @return Observation is marked with a delete flag.  No return value.
#' @export
#'
#' @seealso The \code{\link{datastep}} function.
#' @examples
#' #' # Remove all cars that are not 4 cylinder
#' df <- datastep(mtcars, 
#'                keep = c("mpg", "cyl", "disp"), {
#'                  
#'   if (cyl != 4)
#'     delete()
#'                  
#' })
#' 
#' df
#' #     mpg cyl  disp
#' # 1  22.8   4 108.0
#' # 2  24.4   4 146.7
#' # 3  22.8   4 140.8
#' # 4  32.4   4  78.7
#' # 5  30.4   4  75.7
#' # 6  33.9   4  71.1
#' # 7  21.5   4 120.1
#' # 8  27.3   4  79.0
#' # 9  26.0   4 120.3
#' # 10 30.4   4  95.1
#' # 11 21.4   4 121.0
delete <- function() {
  
  # Parent frame hold environment with ds row
  pf <- parent.frame()
  
  # Set by reference
  pf$..delete <- TRUE
  
  
}


#' @title Outputs an observation from a datastep
#' @description The \code{output} function will output an observation
#' from a datastep.  The function takes no parameters.  To use 
#' the function, simply call it on the rows you want to output.  Typically
#' it is called within a conditional.  The output function is interesting
#' in that you can output multiple rows for the same input observation.
#' @return Observation is marked with a output flag.  No return value.
#' @export
#'
#' @seealso The \code{\link{datastep}} function.
#' @examples
#' #' # Example 1: Output all cars that are 4 cylinder 
#' df <- datastep(mtcars, 
#'                keep = c("mpg", "cyl", "disp"), {
#'                  
#'   if (cyl == 4)
#'     output()
#'                  
#' })
#' 
#' df
#' #     mpg cyl  disp
#' # 1  22.8   4 108.0
#' # 2  24.4   4 146.7
#' # 3  22.8   4 140.8
#' # 4  32.4   4  78.7
#' # 5  30.4   4  75.7
#' # 6  33.9   4  71.1
#' # 7  21.5   4 120.1
#' # 8  27.3   4  79.0
#' # 9  26.0   4 120.3
#' # 10 30.4   4  95.1
#' # 11 21.4   4 121.0
#' 
#' # Example 2: Output two rows for each 6 cylinder car
#' 
#' # Prepare sample data
#' dat <- data.frame(name = rownames(mtcars), mtcars, stringsAsFactors = FALSE)
#' 
#' # Perform datastep
#' df <- datastep(dat, 
#'                keep = c("name", "mpg", "cyl", "disp", "seq"), {
#'                  
#'   if (cyl == 6) {
#'     seq <- 1
#'     output()
#'     seq <- 2
#'     output()
#'   }
#'                  
#' })
#' 
#' df
#' #              name  mpg cyl  disp seq
#' # 1       Mazda RX4 21.0   6 160.0   1
#' # 2       Mazda RX4 21.0   6 160.0   2
#' # 3   Mazda RX4 Wag 21.0   6 160.0   1
#' # 4   Mazda RX4 Wag 21.0   6 160.0   2
#' # 5  Hornet 4 Drive 21.4   6 258.0   1
#' # 6  Hornet 4 Drive 21.4   6 258.0   2
#' # 7         Valiant 18.1   6 225.0   1
#' # 8         Valiant 18.1   6 225.0   2
#' # 9        Merc 280 19.2   6 167.6   1
#' # 10       Merc 280 19.2   6 167.6   2
#' # 11      Merc 280C 17.8   6 167.6   1
#' # 12      Merc 280C 17.8   6 167.6   2
#' # 13   Ferrari Dino 19.7   6 145.0   1
#' # 14   Ferrari Dino 19.7   6 145.0   2
#' 
#' # Example 3: Create data frame using output() functions
#' df <- datastep(data.frame(), {
#' 
#'   # Row 1
#'   COL1 <- 1
#'   COL2 <- "One"
#'   output()
#'   
#'   # Row 2
#'   COL1 <- 2
#'   COL2 <- "Two"
#'   output()
#' 
#' })
#' 
#' df
#' #   COL1 COL2
#' # 1    1  One
#' # 2    2  Two
output <- function() {
  
  #browser()
  # Parent frame hold row
  pf <- parent.frame()
  
  # Convert to list so it can be converted to a data frame
  nlst <- as.list(pf)
  nlst[["..delete"]] <- pf$..delete
  
  # Convert to data frame and append to output list
  e$output[[length(e$output) + 1]] <- as.data.frame(nlst)

  
}

# Utilities ---------------------------------------------------------------


assign_attribute_list <- function(df, lst) {
  
  ret <- df
  
  anms <- names(lst)
  
  for (nm in names(ret)) {
     
    if (nm %in% anms) {
      
      attributes(ret[[nm]]) <- lst[[nm]] 
    }
  }
   
  return(ret)
}

# Collects column attributes into a list,
# preserving any attributes already in the list.
collect_attributes <- function(alst, df, idcols, sfx) {
 
  if (is.null(alst))
    ret <- list()
  else 
    ret <- alst
  
  anms <- names(ret)
  
  for (nm in names(df)) {
    
    if (!nm %in% anms) {
      
     ret[[nm]] <- attributes(df[[nm]]) 
    } else if (!nm %in% idcols) {
      
      # If name already exists, add suffixes
      nnm <- paste0(nm, sfx[1])
      tnm <- paste0(nm, sfx[2])
      ret[[nnm]] <- ret[[nm]]
      ret[[tnm]] <- attributes(df[[nm]]) 
      ret[[nm]] <- NULL
    }
    
  }
  
  return(ret)
  
}


# Copy column attributes from one df to another.
# Used during datastep operations to restore attributes
# lost when using Base R functions. 
#' @noRd
copy_attributes <- function(df1, df2) {
  
  ret <- df2
  
  for (nm in names(df2)) {

    att <- attributes(df1[[nm]])
    if (!is.null(att))
      attributes(ret[[nm]]) <- att
    
    # col <- df1[[nm]]
    # for (at in names(attributes(col))) {
    #   
    #   attr(ret[[nm]], at) <- attr(col, at)
    #   
    # }
    
  }
  
  return(ret)
}

copy_attributes_sp <- function(df1, df2) {
  
  ret <- df2
  
  for (nm in names(df2)) {
    
    col <- df1[[nm]]
    if (!is.null(col)) {
      for (at in names(attributes(col))) {
        
        if (!at %in% c("levels")) { 
  
          attr(ret[[nm]], at) <- attr(col, at)
        }
  
      }
    }
    
  }
  
  return(ret)
}


# Copies attributes on data frame from one df to another
# Skips rownames and names, which can cause trouble.
copy_df_attributes <- function(src, trgt) {
  
  atts <- attributes(src)
  
  ret <- trgt
  
  for (anm in names(atts)) {
    
    if (!anm %in% c("names", "row.names")) { 
      attr(ret, anm) <- atts[[anm]] 
    }
  }
  
  return(ret)
}

# General function to assign column attributes to 
# a data frame.  Can assign basically any attributes
# like labels or formats or whatever.  Used to apply
# attributes assigned on the function call.
assign_attributes <- function(df, alst, attr) {
  
  nmsdf <- names(df)
  nmslst <- names(alst)
  
  ret <- df
  
  for (nm in nmslst) {
    
    if (nm %in% nmsdf) { 
    
      attr(ret[[nm]], attr) <- alst[[nm]] 
    }
  }
  
  return(ret)
  
}

# Test to see whether the code has an output statement.
# Will change how the datastep is conducted.
has_output <- function(codestr) {
 
  
  ret <- FALSE
  
  opos <- grepl("output()", codestr, fixed = TRUE)
  
  if (any(opos == TRUE))
    ret <- TRUE
  
  return(ret)

}

# Perform the set operation.  Works on main
# dataset plus one or more datasets.
perform_set <- function(dta, stdta) {
  
  # Put in list
  if ("data.frame" %in% class(stdta))
    dtalst <- list(stdta)
  else
    dtalst <- stdta
  
  # Collect Names
  fnms <- names(dta)
  
  # Assign counter to ensure stacking
  dta[["..ds"]] <- 0
  
  ret <- dta
  
  # Stack datasets
  for (i in seq_len(length(dtalst))){
    
    tmp <- dtalst[[i]]
    nnms <- names(tmp)
    fnms <- c(fnms, nnms[!nnms %in% fnms])
    tmp[["..ds"]] <- i
    ret <- merge(ret, tmp, all = TRUE, sort = FALSE) 
    
  }
  
  # Clean up counter
  ret[["..ds"]] <- NULL
  dta[["..ds"]] <- NULL
  
  # Rename so first dataset drives naming
  ret <- ret[ , fnms]
  
  ret <- copy_attributes_sp(dta, ret)
  ret <- copy_df_attributes(dta, ret)
  
  return(ret)
  
}

# Perform merge operation.  Works on one or more datasets.
perform_merge <- function(dta, mrgdta, mrgby, mrgin) {
  
  # Put in list
  if ("data.frame" %in% class(mrgdta))
    dtalst <- list(mrgdta)
  else
    dtalst <- mrgdta
  
  ret <- dta
  
  # Capture names for sorting
  fnms <- names(dta)
  
  xnms <- names(mrgby)
  ynms <- mrgby 
  names(ynms) <- NULL
  if (is.null(xnms)) {
    xnms <- ynms
    ynms <- NULL 
  } else {
    
    if (length(ynms) != length(xnms)) {
      xnms <- ynms
      ynms <- NULL
      
    }
  }
  
  if (!is.null(mrgin)) {
    
    ret[[mrgin[1]]] <- 1

  }
  
  # Initialize attribute list with left df
  alst <- collect_attributes(NULL, ret, mrgby, c())
  
  # Merge datasets
  for (i in seq_len(length(dtalst))){
    
    tmp <- dtalst[[i]]
    
    # Create suffixes (if needed)
    sfx <- c("." %p% i, "." %p% (i + 1))
    
    # Construct name list from original dfs
    fnms <- fix_names(fnms, names(tmp), mrgby, sfx)
    
    # Collect attributes from right df
    alst <- collect_attributes(alst, tmp, mrgby, sfx)
    
    # Add in variables
    if (!is.null(mrgin)) {
      if (!is.na(mrgin[i + 1])) {
       
        tmp[[mrgin[i + 1]]] <- 1 
      }
    }
    
    # Bail if merge columns are not in source df
    if (!all(xnms %in% names(ret))) {
      stop("Merge column name '", xnms[!xnms %in% names(ret)],
           "' not found in left dataset.")

    }
    
    # If there is no merge_by, just append to the end
    # Otherwise, perform the join.
    if (is.null(mrgby)) {
      
      # Deal with mismatched number of rows
      if (nrow(ret) > nrow(tmp))
        ret <- cbind(ret, fill_missing(tmp, nrow(ret)))
      else if (nrow(tmp) > nrow(ret))
        ret <- cbind(fill_missing(ret, nrow(tmp)), tmp)
      else 
        ret <- cbind(ret, tmp)
      
      # Assign corrected names
      names(ret) <- fnms
      
    } else {
    
      
      if (is.null(ynms)) {
        # When merge by column names are the same
        ret <- merge(ret, tmp, by = xnms, suffix = sfx,
                     all = TRUE,
                     sort = FALSE) 
      } else {
        
        if (!all(ynms %in% names(tmp))) {
          stop("Merge column name '", ynms[!ynms %in% names(tmp)], 
               "' not found in right dataset.")
        }
  
        # When merge column names are different
        ret <- merge(ret, tmp, by.x = xnms, by.y = ynms, suffix = sfx,
                     all = TRUE,
                     sort = FALSE) 
        
      }
      
    }
    
  }
  
  # Fill zero for non-matches
  if (!is.null(mrgin)) {
    for (nm in mrgin) { 
    
      ret[[nm]] <- ifelse(is.na(ret[[nm]]), 0, ret[[nm]]) 
    }
  }
  
  if (!is.null(mrgin))
    ret <- ret[ , c(fnms, mrgin)]
  else
    ret <- ret[ , fnms]
  
  ret <- assign_attribute_list(ret, alst)
  ret <- copy_df_attributes(dta, ret)
  
  return(ret)
  
}

# A function to perform naming for merged datasets.
# This will keep the preferred column order and append
# indexes for repeated column names.
fix_names <- function(nms1, nms2, keys, sfxs) {
  
  if (is.null(keys))
    keys <- ""
  
  ret <- c()
  for (i in seq_along(nms1)) {
    if (nms1[i] %in% keys) 
      ret[i] <- nms1[i]
    else if (nms1[i] %in% nms2)
      ret[i] <- paste0(nms1[i], sfxs[1])
    else 
      ret[i] <- nms1[i]
  }
  
  for (i in seq_along(nms2)) {
    
    if (!nms2[i] %in% keys) {
        
      if (nms2[i] %in% nms1)
        ret[length(ret) + 1] <- paste0(nms2[i], sfxs[2])
      else 
        ret[length(ret) + 1] <- nms2[i]
    }
  }
  
  return(ret)
}

# Fill in missing rows on a dataset. Takes
# a dataset and a number of rows for the desired row count.
# This is used when cbinding to make sure the datasets
# are the same number of rows.
fill_missing <- function(ds, num) {
  
  if (num > nrow(ds)) {
    nas <- rep(NA, num - nrow(ds))
    nw <- list()
    
    for (nm in names(ds)) {
      
      nw[[nm]] <- nas
    }
    
    dfn <- as.data.frame(nw, stringsAsFactors = FALSE)
    
    ret <- rbind(ds, nw)
  
  } else {
    ret <- ds 
  }
  
  
  return(ret)
  
}
