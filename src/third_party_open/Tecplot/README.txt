This directory contains parts of the TecIO library:
http://www.tecplot.com/downloads/tecio-library/

The binaries to be linked into an application are in lib/:
 * x64 only
 * static tecio.lib, which needs tecio.dll.
 * tecio.dll needs msvcr90.dll and msvcp90.dll, shipped in same directory.

The include files for use in your sources are in include/.