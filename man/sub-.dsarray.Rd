% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dsarray.R
\encoding{UTF-8}
\name{[.dsarray}
\alias{[.dsarray}
\title{Indexer for Data Step Array}
\usage{
\method{[}{dsarray}(x, i = NULL)
}
\arguments{
\item{x}{The \code{\link{dsarray}} object.}

\item{i}{The index of the datastep array item to return a value for.
This index can be a column name or position in the array. It can also
be a vector of column names or positions.  If no index
is supplied, a vector of all array values will be returned.}
}
\value{
The value of the specified column for the current row in the 
datastep.  If no index is supplied, a vector of all column values will
be returned.
}
\description{
A custom indexer for the Datastep Array.  The indexer will
return a value for all columns or a specified column. To 
access all columns, leave the indexer empty. Otherwise, specify the
the column name(s) or number(s) to return data for.  The indexer will
always act upon the current row in the datastep.  
For additional details, see the \code{\link{dsarray}} function.
}
\examples{
library(libr)

# Create AirPassengers Data Frame
df <- as.data.frame(t(matrix(AirPassengers, 12, 
                    dimnames = list(month.abb, seq(1949, 1960)))), 
                    stringsAsFactors = FALSE)

# Use datastep array to get sums by quarter
# Examine different ways of referencing data inside datastep
dat <- datastep(df,
                keep = c("Q1", "Q2", "Q3", "Q4", "Tot"),
                arrays = list(months = dsarray(names(df))),
                {
                
                   # Reference by column name
                   Q1 <- Jan + Feb + Mar
                   
                   # Reference by array positions
                   Q2 <- sum(months[4:6])
                   
                   # Reference by array names
                   Q3 <- sum(months[c("Jul", "Aug", "Sep")])
                   
                   # Reference by row position
                   Q4 <- rw$Oct + rw[["Nov"]] + rw[[12]]
                   
                   # Empty indexer returns all column values in array
                   Tot <- sum(months[])
                  
                })

dat
#        Q1   Q2   Q3   Q4  Tot
# 1949  362  385  432  341 1520
# 1950  382  409  498  387 1676
# 1951  473  513  582  474 2042
# 1952  544  582  681  557 2364
# 1953  628  707  773  592 2700
# 1954  627  725  854  661 2867
# 1955  742  854 1023  789 3408
# 1956  878 1005 1173  883 3939
# 1957  972 1125 1336  988 4421
# 1958 1020 1146 1400 1006 4572
# 1959 1108 1288 1570 1174 5140
# 1960 1227 1468 1736 1283 5714
}
\seealso{
Other datastep: 
\code{\link{datastep}()},
\code{\link{delete}()},
\code{\link{dsarray}()},
\code{\link{dsattr}()},
\code{\link{length.dsarray}()},
\code{\link{output}()}
}
\concept{datastep}
