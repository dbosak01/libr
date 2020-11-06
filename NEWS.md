# libr 1.0.0

A package to create data libraries and data dictionaries:

* libname() function creates a data library
* dictionary() function creates a data dictionary

* Also contains a variety of functions to manipulate data libraries:
** lib_load(): Loads a library into the workspace
** lib_unload(): Unloads a library from the workspace
** lib_sync(): Synchronizes the workspace with the library list
** lib_write(): Writes library data to the file system
** lib_add(): Adds data to a library
** lib_remove(): Removes data from a library
** lib_copy(): Copies a data library
** lib_delete(): Deletes a data library
** lib_info(): Returns a data frame of information about the library
** lib_path(): Returns the path of a data library
** lib_size(): Returns the size of the data library in bytes
