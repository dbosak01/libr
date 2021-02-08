# libr 1.1.3

* Fixed bug on datastep when data frame/tibble had a single column.


# libr 1.1.1

* Integrated libr with logr.  All library functions will automatically
provide logging entries if the autolog feature of the logr package is enabled.
* Added pkgdown site.
* Added filter parameters to `libname()` and `lib_load()` parameters. The
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
