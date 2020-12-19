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
