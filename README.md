<!-- badges: start -->
  
[![libr version](https://www.r-pkg.org/badges/version/libr)](https://cran.r-project.org/package=libr)
[![libr lifecycle](https://img.shields.io/badge/lifecycle-stable-blue.svg)](https://cran.r-project.org/package=libr)
[![libr downloads](https://cranlogs.r-pkg.org/badges/libr)](https://cran.r-project.org/package=libr)
[![libr total downloads](https://cranlogs.r-pkg.org/badges/grand-total/libr)](https://cran.r-project.org/package=libr)
[![R-CMD-check](https://github.com/dbosak01/libr/workflows/R-CMD-check/badge.svg)](https://github.com/dbosak01/libr/actions)
[![Codecov test coverage](https://codecov.io/gh/dbosak01/libr/branch/master/graph/badge.svg)](https://codecov.io/gh/dbosak01/libr?branch=master)

<!-- badges: end -->
  
# Introduction to **libr** 
<img src="./man/images/libr1.png" align="left" height="138" style="margin-right: 20px"/>

R is a very flexible and powerful language.  But there are some inconveniences
when working with data:

1. **Loading many related datasets**:  When you have many data files
that are related in some way, it is troublesome when you want to use 
them all in your analysis.  Most data import packages make you load each
file individually. And the thing is, *almost all data* comes in sets of related 
files.  So you would think that there would be an easy mechanism 
to load them all at once.

2. **Examining the Attributes of Your Data**:  Once you have a set of related
data files loaded into your program, the next problem is how to examine 
the structure of *all of them*.  R has the `str()` function 
if you want to understand the structure of a single dataset.  But what if 
there are many?  There is no simple way to examine the attributes 
of many related datasets.  And further, some valuable attributes are missing 
from the `str()` function output.

3. **Row-By-Row Processing of Data**: R processes data column by column. 
In most cases, column-by-column is a perfectly suitable way of processing data. 
But in some cases it is desirable to process data row-by-row.  Row-by-row 
processing is useful when you have many related columns, and want to 
perform complex conditional processing across them.  It is also very convenient
when you have grouped data, and want to perform conditional processing
within a group.

4. **Comparison Operator**: Once you are comparing two
columns of data, you run into the next inconvenience: the R comparison operator
(==).  This operator is highly susceptible to crashing.  All you need is
one NA value in one of the columns, and the comparison will crash. Since
NA values are so frequently encountered in data, it would convenient if
there were an infix operator that didn't crash when it encountered an
aberrant value.  

### Solution

The **libr** package was developed to address all of the above problems. It does so
by introducing four concepts:

* Data Libraries
* Data Dictionaries
* A Data Step
* An Enhanced Equality Operator

By introducing these concepts, the **libr** package makes working with
data in R much easier and faster.  The **libr** package can greatly simplify
your data processing code.  And it makes some types of data operations effortless 
that are quite tricky to do with **Base R** or even **tidyverse** functions.

### Installation

To install the **libr** package, run the following 
command from your R console:

    install.packages("libr")


Then put the following line at the top of your program or script:

    library(libr)
    
The **libr** package will then be ready to use! For examples and usage 
information, please visit the **libr** documentation site 
[here](https://libr.r-sassy.org/articles/libr.html)

### Development Version

The **libr** package is under active development.  If you want the 
latest development version, you can download it from [github](https://github.com)
with the following command line:

    devtools::install_github("https://github.com/dbosak01/libr")
    
### Getting Help

If you need help, the first place 
to turn to is the [libr](https://libr.r-sassy.org) web site. The web site
has full documentation on all **libr** functions.

If you want to look at the code for the **libr** package, visit the
github page [here](https://github.com/dbosak01/libr).

If you encounter a bug or have a feature request, please submit an issue 
[here](https://github.com/dbosak01/libr/issues).

### See Also

The **libr** package is part of the **sassy** meta-package. 
The **sassy** meta-package includes several packages that help make R
easier for SAS® programmers.  You can read more about the **sassy** package
[here](https://sassy.r-sassy.org).
