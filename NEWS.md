# libr 1.3.5
* Added 'parquet' file format to `libname()` function.
* Added 'subset' parameter to `datastep()` function to filter the data on input.

# libr 1.3.4
* Fixed issue where `libname()` was failing on empty dataset.

# libr 1.3.3
* Fixed issue where `lib_write()` was not detecting changes to datasets in libname.

# libr 1.3.2
* Fixed bug on `libname()` when there are file names with multiple dots.

# libr 1.3.1
* Fixed bug on `datastep()` when running in `source.all()`.

# libr 1.3.0
* Fixed bug on sas7bdat where class on date columns was being stripped.
* Fixed bug on `datastep()` where expression that was removing input dataset
attributes (like labels).

# libr 1.2.9
* Fixed bug on `datastep()` when there are spaces in the column names 
and output() function is used.
* Some improvements to `datastep()` performance.
* Send message on writing "sas7bdat" that functionality is not available.
* Added where clause parameter to `libname()`.
* Added automatic variables "first.X" and "last.X" for each by variable.

# libr 1.2.8
* Fixed bug on datastep that sometimes was causing variables to lose their attributes.
* Added "where" parameter to datastep.
* Fix bug on datastep that was causing a single variable dataframe to be returned
as a vector.
* Cleared rownames on datastep exit.
* Added "MaxChar" column to dictionary to hold the maximum number of characters
for a data value in the column.  This is different from "width" which
is the user-defined, proscribed width.  MaxChar is the actual width with no 
padding.
* Added "rda" engine to libname.
* Allowed single value NSE on drop, keep, and by parameter of `datastep()`.
For multiple values, use the `v()` function.
* Added `output()` function to datastep.
* Improved datastep output column ordering. New variables were coming out
in the opposite order they were added.  Now they are better.
* Added "set" parameter to datastep.
* Added "merge", "merge_by", and "merge_in" parameter to `datastep()`.
* Fixed bug on `datastep()` rename.
* Added/fixed documentation.
* Updated logo.

# libr 1.2.5
* BREAKING CHANGE: Removed the `%eq%` operator from this package, as it has
been moved to the **common** package.  A dependency on **common** has 
been added to the **libr** package so that the `%eq%` operator
will be loaded and available.  However, it is still possible some changes
may be required to make the new location for the `%eq%` operator work properly.
* Apply "format" parameter on `datastep()` to both input and output dataset.


# libr 1.2.3
* Added "Rdata" option to `libname()` function engines.
* Added "label" parameter to `datastep()` function.
* Added "format" parameter to `datastep()` function.
* Added `lib_export()` function to export a library to a different 
directory and file format.
* Fixed bug on `dictionary()` that was causing a warning when there 
were no rows on the input data frame.

# libr 1.2.2

* Added FAQ and Complete Examples
* Fixed bug on `dictionary()` when encountering datetime variable with
multiple POSIX classes.


# libr 1.2.1 

* Added covr and codecov
* Fixed bug on `dictionary()` function that wasn't showing width attribute.
* Fixed bug on `datastep()` when applying attributes to a calculated variable.
* Small documentation fixes.


# libr 1.2.0

* Made package compatible to R version 3.6.
* Added GitHub actions to test previous versions of R.
* Increased performance of datastep() function generally.  Benchmarks show
about 40% improvement on average.
* Added _standard_eval_ parameter to `libname()` and `lib_copy()` functions to
allow user to pass library names as a variable.
* Added _quiet_ parameter on `libname()` function to minimize console output
if desired.
* Added _arrays_ parameter and `dsarray()` class to handle data step arrays. 
This functionality allows iteration across a list of variables inside a 
`datastep()`.
* Added _attributes_ parameter and `dsattr()` class to handle data step
attributes.  This functionality allows the user to add attributes 
to datastep variables.
* Fixed bug in `datastep()` where it was stripping column attributes on Base R
data frames.

# libr 1.1.3

* Fixed bug on datastep when data frame/tibble had a single column.


# libr 1.1.1

* Integrated libr with logr.  All library functions will automatically
provide logging entries if the autolog feature of the logr package is enabled.
* Added pkgdown site.
* Added _filter_ parameters to `libname()` and `lib_load()` functions. The
filter parameter allows the user to specify which data from the library
they want loaded into memory.
* Changed default "na" parameter on csv export to empty string instead of NA
to accommodate import into SAS.  SAS couldn't deal with the NA strings.
* Fixed bug in dbf engine when outputting tibbles.
* Fixed bug in %eq% operator when comparing objects with different numbers of 
classes.
* Greatly improved performance of datastep, especially on grouped tibbles.
  

# libr 1.0.1

A package to create data libraries, data dictionaries, and the ability
to perform a data step.  The major functions are:

* `libname()` function creates a data library
* `dictionary()` function creates a data dictionary
* `datastep()` function steps through data row-by-row
* `%eq%` allows comparison of any two R objects without error

The packages also contains a variety of functions to manipulate data libraries:
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
* `specs()`: Create a collection of import specifications
* `import_spec()`: Define an import specification for a file
