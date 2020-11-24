# libr <img src="./man/images/books.svg" align="right" height="138" />

<!-- badges: start -->
  
[![libr version](https://www.r-pkg.org/badges/version/libr)](https://cran.r-project.org/package=libr)
[![libr lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://cran.r-project.org/package=libr)
[![libr downloads](https://cranlogs.r-pkg.org/badges/grand-total/libr)](https://cran.r-project.org/package=libr)
[![Travis build status](https://travis-ci.com/dbosak01/libr.svg?branch=master)](https://travis-ci.com/dbosak01/libr)

<!-- badges: end -->
  
The **libr** package brings the concepts of data libraries and data 
dictionaries to R.  A data library is an object used to define and manage
an entire directory of data files.  A data dictionary is a data frame full
of information about a data library, data frame, or tibble.  

The functions contained in the **libr** package are as follows:

* `libname()`: Creates a data library
* `dictionary()`: Creates a data dictionary
* `lib_load()`: Loads a library into the workspace
* `lib_unload()`: Unloads a library from the workspace
* `lib_sync()`: Synchronizes the workspace with the library list
* `lib_write()`: Writes library data to the file system
* `lib_add()`: Adds data to a library
* `lib_replace()`: Replaces data in a library
* `lib_remove()`: Removes data from a library
* `lib_copy()`: Copies a data library
* `lib_delete()`: Deletes a data library
* `lib_info()`: Returns a data frame of information about the library
* `lib_path()`: Returns the path of a data library
* `lib_size()`: Returns the size of the data library in bytes


## Example
The following example will illustrate some basic functionality of the 
**libr** package:
```
# Create temp directory
tmp <- tempdir()

# Save some data to temp directory
# for illustration purposes
saveRDS(trees, file.path(tmp, "trees.rds"))
saveRDS(rock, file.path(tmp, "rocks.rds"))

# Create library
libname(dat, tmp)

# Examine library
dat
# library 'dat': 2 items
# - attributes: not loaded
# - path: C:\Users\User\AppData\Local\Temp\RtmpCSJ6Gc
# - items:
#    Name Extension Rows Cols   Size        LastModified
# 1 rocks       rds   48    4 3.1 Kb 2020-11-05 23:25:34
# 2 trees       rds   31    3 2.4 Kb 2020-11-05 23:25:34

# Examine data dictionary for library
dictionary(dat)
# A tibble: 7 x 9
#   Name  Column Class   Label Description Format Width  Rows   NAs
#   <chr> <chr>  <chr>   <lgl> <lgl>       <lgl>  <lgl> <int> <int>
# 1 rocks area   integer NA    NA          NA     NA       48     0
# 2 rocks peri   numeric NA    NA          NA     NA       48     0
# 3 rocks shape  numeric NA    NA          NA     NA       48     0
# 4 rocks perm   numeric NA    NA          NA     NA       48     0
# 5 trees Girth  numeric NA    NA          NA     NA       31     0
# 6 trees Height numeric NA    NA          NA     NA       31     0
# 7 trees Volume numeric NA    NA          NA     NA       31     0

# Load library
lib_load(dat)

# Examine workspace
ls()
# [1] "dat" "dat.rocks" "dat.trees" "tmp"

# Use data from the library
summary(dat.rocks)

# Add data to the library
dat.trees_subset <- subset(dat.trees, Girth > 11)

# Add more data to the library
dat.cars <- mtcars

# Unload the library from memory
lib_unload(dat)

# Examine workspace again
ls()
# [1] "dat" "tmp"

# Write the library to disk
lib_write(dat)

# Examine the library again
dat
# library 'dat': 4 items
# - attributes: not loaded
# - path: C:\Users\User\AppData\Local\Temp\RtmpCSJ6Gc
# - items:
#           Name Extension Rows Cols   Size        LastModified
# 1        rocks       rds   48    4 3.1 Kb 2020-11-05 23:37:45
# 2        trees       rds   31    3 2.4 Kb 2020-11-05 23:37:45
# 3         cars       rds   32   11 7.3 Kb 2020-11-05 23:37:45
# 4 trees_subset       rds   23    3 1.8 Kb 2020-11-05 23:37:45

# Clean up
lib_delete(dat)

# Examine workspace again
ls()
# [1] "tmp"
```

## How to Use a Data Library
There are four main **libr** functions for creating and using a data library:

* `libname()`
* `lib_load()`
* `lib_unload()`
* `lib_write()`

The `libname()` function creates a data library.  The function has parameters
for the library name and a directory to associate it with.  If the directory
has existing data files, those data files will be automatically loaded
into the library.  Once in the library, the data can be accessed using list
syntax.

If you prefer to access the data via the workspace, simply call
the `lib_load()` function on the library.  This function will load the 
library data into the parent frame, where it can be accessed using a two-level
(<library>.<dataset>) name.  

When you are done with the data, call the `lib_unload()` function to remove
the data from the parent frame and put it back in the library list.  To write
any added or modified data to disk, call the `lib_write()` function.  

The **libr** package also contains a number of functions for manipulating
data libraries.  There are functions to add and remove data from a library,
as well as copy or delete an entire library.

The example below illustrates some of the additional functions:

```
# Create temp directory
tmp <- tempdir()

# Create libraries
libname(s1, tmp)

# Add data to library and adjust names
lib_add(s1, state.name, state.area, state.region, state.abb,
        name = c("name", "area", "region", "abb"))

# Copy library
lib_copy(s1, s2, file.path(tmp, "orig"))

# Remove data from library 1
lib_remove(s1, name = c("name", "area", "region", "abb"))

# Load libraries into memory
lib_load(s1)
lib_load(s2)

s1.combined <- data.frame(name = s2.name, abb = s2.abb, 
                              area = s2.area, region = s2.region)

s1.east <- subset(s1.combined, region == "Northeast")
s1.west <- subset(s1.combined, region == "West")
s1.north <- subset(s1.combined, region == "North Central")
s1.south <- subset(s1.combined, region == "South")

# Sync workspace with library list
lib_sync(s1)

# Save library to disk
lib_write(s1)

# View path
lib_path(s1)

# View size
lib_size(s1)

# View info
lib_info(s1)

# Display dictionary
dictionary(s1)

# Unload libraries
lib_unload(s1)
lib_unload(s2)

# Clean up
lib_delete(s1)
lib_delete(s2)
```

Note that the **libr** package is intended to be used with small and 
medium-sized data sets.  It is not recommended for big data, as big data
requires very careful control over which data is or is not loaded into memory.
The **libr** package, on the other hand, tends to load all data into memory 
indiscriminately.




