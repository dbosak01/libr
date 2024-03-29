

# dsarray Definition ------------------------------------------------------


#' @title Create a Data Step Array
#' @encoding UTF-8
#' @description A data step array is an object that allows you to iterate
#' across a set of columns inside a \code{\link{datastep}}.  This structure is
#' useful when you need to perform the same or similar operations on many columns.  
#' 
#' @details
#' The datastep array has an indexer that allows you to access a particular
#' column value.  The indexer can be used within a for loop to iterate over
#' the array. In this manner, you can place a set of conditions inside
#' the for loop and run the same conditional logic on all the columns 
#' in the array. 
#' 
#' You can also use the datastep array with an empty indexer in vectorized 
#' functions like \code{\link[base]{sum}}, \code{\link[base]{mean}}, 
#' and \code{\link[base]{max}}.  The empty indexer will return all the 
#' values in the array for the current row.
#' @param ... Column names to include as part of the datastep array.  The 
#' names can be provided as quoted strings or a vector of strings.
#' If names are provided as quoted strings, separate the strings with commas 
#' (i.e. \code{dsarray("col1", "col2", "col3")}).
#' @return The datastep array object.
#' @seealso \code{\link{libname}} to create a data library, and
#' \code{\link{dictionary}} for generating a data dictionary
#' @family datastep
#' @examples 
#' library(libr)
#' 
#' # Create AirPassengers Data Frame
#' df <- as.data.frame(t(matrix(AirPassengers, 12, 
#'                     dimnames = list(month.abb, seq(1949, 1960)))),
#'                     stringsAsFactors = FALSE)
#' 
#' # Use datastep array to get year tot, mean, and top month
#' dat <- datastep(df,
#'                 arrays = list(months = dsarray(names(df))),
#'                 attrib = list(Tot = 0, Mean = 0, Top = ""),
#'                 drop = "mth",
#'                 {
#'                 
#'                   Tot <- sum(months[])
#'                   Mean <- mean(months[])
#'                 
#'                   for (mth in months) {
#'                     if (months[mth] == max(months[])) {
#'                       Top <- mth
#'                     }
#'                   }
#'                   
#'                 })
#' 
#' dat
#' #      Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec  Tot     Mean Top
#' # 1949 112 118 132 129 121 135 148 148 136 119 104 118 1520 126.6667 Aug
#' # 1950 115 126 141 135 125 149 170 170 158 133 114 140 1676 139.6667 Aug
#' # 1951 145 150 178 163 172 178 199 199 184 162 146 166 2042 170.1667 Aug
#' # 1952 171 180 193 181 183 218 230 242 209 191 172 194 2364 197.0000 Aug
#' # 1953 196 196 236 235 229 243 264 272 237 211 180 201 2700 225.0000 Aug
#' # 1954 204 188 235 227 234 264 302 293 259 229 203 229 2867 238.9167 Jul
#' # 1955 242 233 267 269 270 315 364 347 312 274 237 278 3408 284.0000 Jul
#' # 1956 284 277 317 313 318 374 413 405 355 306 271 306 3939 328.2500 Jul
#' # 1957 315 301 356 348 355 422 465 467 404 347 305 336 4421 368.4167 Aug
#' # 1958 340 318 362 348 363 435 491 505 404 359 310 337 4572 381.0000 Aug
#' # 1959 360 342 406 396 420 472 548 559 463 407 362 405 5140 428.3333 Aug
#' # 1960 417 391 419 461 472 535 622 606 508 461 390 432 5714 476.1667 Jul
#' @export
dsarray <- function(...) {
  
  # Create new structure of class "dsarray"
  s <- structure(c(...), class = c("dsarray", "character"))
  
  names(s) <- s

  return(s)
  
}


#' @title Indexer for Data Step Array
#' @encoding UTF-8
#' @description A custom indexer for the Datastep Array.  The indexer will
#' return a value for all columns or a specified column. To 
#' access all columns, leave the indexer empty. Otherwise, specify the
#' the column name(s) or number(s) to return data for.  The indexer will
#' always act upon the current row in the datastep.  
#' For additional details, see the \code{\link{dsarray}} function.
#' @param x The \code{\link{dsarray}} object.
#' @param i The index of the datastep array item to return a value for.
#' This index can be a column name or position in the array. It can also
#' be a vector of column names or positions.  If no index
#' is supplied, a vector of all array values will be returned.
#' @return The value of the specified column for the current row in the 
#' datastep.  If no index is supplied, a vector of all column values will
#' be returned.
#' @family datastep
#' @examples 
#' library(libr)
#' 
#' # Create AirPassengers Data Frame
#' df <- as.data.frame(t(matrix(AirPassengers, 12, 
#'                     dimnames = list(month.abb, seq(1949, 1960)))), 
#'                     stringsAsFactors = FALSE)
#' 
#' # Use datastep array to get sums by quarter
#' # Examine different ways of referencing data inside datastep
#' dat <- datastep(df,
#'                 keep = c("Q1", "Q2", "Q3", "Q4", "Tot"),
#'                 arrays = list(months = dsarray(names(df))),
#'                 {
#'                 
#'                    # Reference by column name
#'                    Q1 <- Jan + Feb + Mar
#'                    
#'                    # Reference by array positions
#'                    Q2 <- sum(months[4:6])
#'                    
#'                    # Reference by array names
#'                    Q3 <- sum(months[c("Jul", "Aug", "Sep")])
#'                    
#'                    # Reference by row position
#'                    Q4 <- rw$Oct + rw[["Nov"]] + rw[[12]]
#'                    
#'                    # Empty indexer returns all column values in array
#'                    Tot <- sum(months[])
#'                   
#'                 })
#' 
#' dat
#' #        Q1   Q2   Q3   Q4  Tot
#' # 1949  362  385  432  341 1520
#' # 1950  382  409  498  387 1676
#' # 1951  473  513  582  474 2042
#' # 1952  544  582  681  557 2364
#' # 1953  628  707  773  592 2700
#' # 1954  627  725  854  661 2867
#' # 1955  742  854 1023  789 3408
#' # 1956  878 1005 1173  883 3939
#' # 1957  972 1125 1336  988 4421
#' # 1958 1020 1146 1400 1006 4572
#' # 1959 1108 1288 1570 1174 5140
#' # 1960 1227 1468 1736 1283 5714
#' @export
`[.dsarray` <- function(x, i = NULL) {

  
  # Get row from the parent frame
  rw <- get("rw", envir = parent.frame())
  
  
  if (!is.null(i)) {
    
    ret <- c()

    # If index is a column name, return the column
    # Otherwise, lookup the column name from the array
    for (j in i) {
      
      #print(paste0("Class of i:", class(i)))
      if (any(class(i) == "character")) {
        #print(paste0("Value of rw[[j]]:", rw[[j]], " ", class(rw[[j]])))
        if ("factor" %in% class(rw[[j]]))
          ret[j] <- as.character(rw[[j]])
        else 
          ret[j] <- rw[[j]]
        #print(paste0("ret[j]:", ret[j], " ", j, " ", class(ret[j])))
      } else { 
        #print(paste0("Value of rw[[x[[j]]]]:", rw[[x[[j]]]]))
        if ("factor" %in% class(rw[[x[[j]]]] ))
          ret[x[[j]]] <- as.character(rw[[x[[j]]]])
        else 
          ret[x[[j]]] <- rw[[x[[j]]]]
         
      }
    }
  
  } else {
    
    # Initialize an empty vector
    ret <- c()
    
    # Populate the vector with desired values
    for (j in x) {
      if ("factor" %in% class(rw[[j]]))
        ret[j] <- as.character(rw[[j]])
      else 
        ret[j] <- rw[[j]]
    }
    
  }

  
  return(ret)

}


#' @title Length function for dsarray class
#' @encoding UTF-8
#' @description A length function for the data step array \code{\link{dsarray}}.
#' The length function can by used either inside or outside the data step.
#' @param x The \code{\link{dsarray}} object.
#' @return The number of items in the specified \code{\link{dsarray}}.
#' @family datastep
#' @examples 
#' # Define datastep array
#' carr <- dsarray(names(mtcars))
#' 
#' length(carr)
#' # 11
#' @export
length.dsarray <- function(x) {
  
 return(length(as.character(x))) 
  
}





