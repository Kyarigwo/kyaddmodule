# kyaddmodule

I find that when I add a new module to a Haskell project, I first create a file,
then add some standard boiler plate at the top (imports, etc.), then create a spec
file for the tests.  So I got an itch to simplify this.

This project takes a template file, formatted similarly to the template files that
'Stack new' uses, and the name of the module, and creates one or more files.

The template file contains a sequence of sections, each section beginning with a
line like:
{-# START_FILE filenametemplate #-}
where 'filenametemplate' is a template that will be expanded to give the
file name and path relative to the current directory.

The section following the header line contains a template which is expanded to
give the contents of the file to create.

The template has only four variables that are expanded.  
* basename = base name of the module given
* fspath = the path to the module as a file system path
* modpath = the path to the module as a haskell module path
* modname = the full module name, as entered on the command line.
Hence if the module name given is:
Name1.Name2.Name3 the variables are set to:
basename = Name3
fspath = Name1/Name2
modpath = Name1.Name2
modname = Name1.Name2.Name3

