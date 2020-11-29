# libr <img src="./man/images/books.svg" align="right" height="138" />

<!-- badges: start -->
  
[![libr version](https://www.r-pkg.org/badges/version/libr)](https://cran.r-project.org/package=libr)
[![libr lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://cran.r-project.org/package=libr)
[![libr downloads](https://cranlogs.r-pkg.org/badges/grand-total/libr)](https://cran.r-project.org/package=libr)
[![Travis build status](https://travis-ci.com/dbosak01/libr.svg?branch=master)](https://travis-ci.com/dbosak01/libr)

<!-- badges: end -->
  
The **libr** package brings the concepts of data libraries, data 
dictionaries, and data steps to R.  A data library is an object used to define 
and manage an entire directory of data files.  A data dictionary is a data 
frame full of information about a data library, data frame, or tibble. A
a data step is a mechanism to perform row-by-row processing of data.


## Glossary 
The functions contained in the **libr** package are as follows:

### Data Library Functions
* `libname()`: Creates a data library
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
* `specs()`: Define import specs for a libname
* `import_spec()`: Define an import spec for a specific file

### Other Functions
* `dictionary()`: Creates a data dictionary
* `datastep()`: Perform row-by-row processing of data
* `%eq%`: An infix operator to check equality between objects


# Libnames and Dictionaries

The main motivation for using the **libr** package is to create and use 
data libraries and data dictionaries.  These concepts are useful when 
dealing with sets of related data files.  The `libname()` function allows
you to define a library for an entire directory of data files.  The library
can then be manipulated as a whole using the `lib_*` functions in the **libr**
package.

## Example 1: Basic Library Operations
The following example will illustrate some basic functionality of the 
**libr** package regarding the creation of libnames and use of dictionaries:
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
any added or modified data to disk, call the `lib_write()` function. The 
`lib_write()` function will only write data that has changed since the last
write.

The **libr** package also contains a number of functions for manipulating
data libraries.  There are functions to add and remove data from a library,
as well as copy or delete an entire library.

## Example 2: Additional Library Operations
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


# Data Step
Normally, R processes data column-by-column. The data step allows you 
to process data row-by-row.  Row-by-row processing of data is useful when you 
have related columns, and wish to perform conditional logic on those 
columns. The `datastep()` function allows you to perform this style of 
data processing. It is particularly advantageous when wish to perform deeply 
nested conditional logic, as the standard R conditionals do not allow you to 
write deeply nested logic easily.   

## Example 1: Simple Data Step
Here is an example of a simple data step:
```
# Add some columns to mtcars using data step logic
df <- datastep(mtcars[1:10, 1:3], {

    if (mpg >= 20) 
      mpgcat <- "High"
    else 
      mpgcat <- "Low"
      
    recdt <- as.Date("1974-06-10")
    
    if (cyl == 8)
      is8cyl <- TRUE

  })
  
# View results  
df
#                    mpg cyl  disp      recdt mpgcat is8cyl
# Mazda RX4         21.0   6 160.0 1974-06-10   High     NA
# Mazda RX4 Wag     21.0   6 160.0 1974-06-10   High     NA
# Datsun 710        22.8   4 108.0 1974-06-10   High     NA
# Hornet 4 Drive    21.4   6 258.0 1974-06-10   High     NA
# Hornet Sportabout 18.7   8 360.0 1974-06-10    Low   TRUE
# Valiant           18.1   6 225.0 1974-06-10    Low     NA
# Duster 360        14.3   8 360.0 1974-06-10    Low   TRUE
# Merc 240D         24.4   4 146.7 1974-06-10   High     NA
# Merc 230          22.8   4 140.8 1974-06-10   High     NA
# Merc 280          19.2   6 167.6 1974-06-10    Low     NA

```

The data step has parameters to perform basic shaping of the resulting
data frame.  These parameters are 'keep', 'drop', and 'rename'.  For example,
the above data step could have been performed by sending all columns into 
the data step, and keeping only the desired columns.  Using the `keep` 
parameter also allows you to order the resulting columns.

## Example 2: Keeping Data Frame Variables
```
# Keep and order output columns 
df <- datastep(mtcars[1:10,], 
  keep = c("mpg", "cyl", "disp", "mpgcat", "recdt"), {

    if (mpg >= 20) 
      mpgcat <- "High"
    else 
      mpgcat <- "Low"
      
    recdt <- as.Date("1974-06-10")
    
    if (cyl == 8)
      is8cyl <- TRUE

  })
  
df
#                    mpg cyl  disp mpgcat      recdt
# Mazda RX4         21.0   6 160.0   High 1974-06-10
# Mazda RX4 Wag     21.0   6 160.0   High 1974-06-10
# Datsun 710        22.8   4 108.0   High 1974-06-10
# Hornet 4 Drive    21.4   6 258.0   High 1974-06-10
# Hornet Sportabout 18.7   8 360.0    Low 1974-06-10
# Valiant           18.1   6 225.0    Low 1974-06-10
# Duster 360        14.3   8 360.0    Low 1974-06-10
# Merc 240D         24.4   4 146.7   High 1974-06-10
# Merc 230          22.8   4 140.8   High 1974-06-10
# Merc 280          19.2   6 167.6    Low 1974-06-10

```
The `datastep()` function also has the capabilities of performing by-group
processing.  A by-group is accomplished using the `by` parameter, and passing
a vector of column names that define the group.  Once a by-group is 
defined, the `first.` and `last.` automatic variables become active, which
allow you to identify the boundaries between groups.  Note that your
data must be sorted properly before sending it into the data step.

## Example 3: By Groups
```
# Identify start and end of by-groups
df <- datastep(mtcars[1:10,], 
  keep = c("mpg", "cyl", "gear", "grp"), 
  by = c("gear"), {

    if (first. & last.)
      grp <- "Start - End"
    else if (first.)
      grp <- "Start"
    else if (last.)
      grp <- "End"
    else 
      grp <- "-"

  })
  
df
#                    mpg cyl gear   grp
# Mazda RX4         21.0   6    4 Start
# Mazda RX4 Wag     21.0   6    4     -
# Datsun 710        22.8   4    4   End
# Hornet 4 Drive    21.4   6    3 Start
# Hornet Sportabout 18.7   8    3     -
# Valiant           18.1   6    3     -
# Duster 360        14.3   8    3   End
# Merc 240D         24.4   4    4 Start
# Merc 230          22.8   4    4     -
# Merc 280          19.2   6    4   End

```
There may be times when you want to combine row-by-row with column-by-column
vector operations.  For example, let's say you want to calculate a mean
and then perform conditional, row-by-row processing on the mean.  This 
situation can be handled using the `calculate` parameter on the `datastep()`
function. The function will execute the `calculate` block first, add any
assigned variables to the data frame, and then execute the data step.  Below 
is an example of such a scenario:

## Example 4: Calculate Block
```
# Categorize mpg as above or below the mean
df <- datastep(mtcars, 
  keep = c("mpg", "cyl", "mean_mpg", "mpgcat"), 
  calculate = {mean_mpg = mean(mpg)},
  {

    if (mpg >= mean_mpg)
      mpgcat <- "High"
    else 
      mpgcat <- "Low"

  })
  
df[1:10,]
#                    mpg cyl mean_mpg mpgcat
# Mazda RX4         21.0   6 20.09062   High
# Mazda RX4 Wag     21.0   6 20.09062   High
# Datsun 710        22.8   4 20.09062   High
# Hornet 4 Drive    21.4   6 20.09062   High
# Hornet Sportabout 18.7   8 20.09062    Low
# Valiant           18.1   6 20.09062    Low
# Duster 360        14.3   8 20.09062    Low
# Merc 240D         24.4   4 20.09062   High
# Merc 230          22.8   4 20.09062   High
# Merc 280          19.2   6 20.09062    Low

```
Note that the `datastep()` function is pipe-friendly, and can be combined
with **dplyr** functions in a data pipeline.  Also note that the `datastep()`
function will recognize any group attributes added by the `group_by()` 
function.  Therefore, within a **dplyr** pipeline, it is not necessary to 
use any `datastep` parameters.  The following example recreates the above 
data frame from Example 4, but with a **dplyr** pipeline.

## Example 5: Data Pipeline
```
library(dplyr)
library(magrittr)

# Add datastep to dplyr pipeline
df <- mtcars %>% 
  select(mpg, cyl, gear) %>% 
  mutate(mean_mpg = mean(mpg)) %>% 
  datastep({

    if (mpg >= mean_mpg)
      mpgcat <- "High"
    else 
      mpgcat <- "Low"

  }) %>% 
  filter(row_number() <= 10)
  
df
#     mpg cyl gear mean_mpg mpgcat
# 1  21.0   6    4 20.09062   High
# 2  21.0   6    4 20.09062   High
# 3  22.8   4    4 20.09062   High
# 4  21.4   6    3 20.09062   High
# 5  18.7   8    3 20.09062    Low
# 6  18.1   6    3 20.09062    Low
# 7  14.3   8    3 20.09062    Low
# 8  24.4   4    4 20.09062   High
# 9  22.8   4    4 20.09062   High
# 10 19.2   6    4 20.09062    Low

```

