# libr <img src="./man/images/books.svg" align="right" height="138" />

<!-- badges: start -->
  
[![libr version](https://www.r-pkg.org/badges/version/libr)](https://cran.r-project.org/package=libr)
[![libr lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://cran.r-project.org/package=libr)
[![libr downloads](https://cranlogs.r-pkg.org/badges/grand-total/libr)](https://cran.r-project.org/package=libr)
[![Travis build status](https://travis-ci.com/dbosak01/libr.svg?branch=master)](https://travis-ci.com/dbosak01/libr)

<!-- badges: end -->
  
  The **libr** package brings the concept of data libraries and data 
dictionaries to R.  Using the **libr** package, an entire directory of
data can be read and loaded into a library in one step.  With one additional
step, those same data file can be loaded into the global environment
for each access.  

## How to use
### Primary Functions
There are four main **libr** functions:

* `libname()`
* `lib_load()`
* `lib_unload()`
* `lib_write()`

The `libname()` function creates a data library.  The function has parameters
for the library name and a directory to associate it with.  If the directory
has existing data files, those data files will be automatically loaded
into the library.  Once in the library, the data can be accessed using list
syntax.

If you prefer to access the data from the global environment, simply call
the `lib_load()` function on the library.  This function will load the 
library data into global memory, where it can be accessed using a two-level
(<library>.<dataset>) name.  

When you are done with the data, call the `lib_unload()` function to remove
the data from global memory and put it back in the library list.  To write
any modified data to disk, call the `lib_write()` function.  

The example below will create a library, add data to it, and write that data
to disk:

```



```

### Additional Functions
There are several functions as part of the **libr** package.

* `dictionary()`





