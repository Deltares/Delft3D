This package contains precompiled binaries of CodeSynthesis XSD, a
W3C XML Schema to C++ Data Binding compiler. For more information
about XSD visit

http://www.codesynthesis.com/products/xsd/

This README file describes how to start using XSD in UNIX or
UNIX-like (for example, Cygwin/Mingw) environments.


Prerequisites
-------------

The XSD runtime library and the generated code depend on the underlying
XML parser which can be Xerces-C++ for the C++/Tree mapping and Xerces-C++
or Expat for the C++/Parser mapping.

Xerces-C++ can be obtained from http://xerces.apache.org/xerces-c/. Most
GNU/Linux distributions provide precompiled binary packages for Xerces-C++.
You can also download precompiled Xerces-C++ libraries for a wide range of
platforms and compilers from http://xerces.apache.org/xerces-c/download.cgi

Expat can be obtained from http://www.libexpat.org/. Most GNU/Linux
distributions provide precompiled binary packages for Expat.


Building Examples
-----------------

To build examples you will need GNU make. All examples in the examples/
directory come with simple makefiles. For instance, to build a hello
example in examples/cxx/tree you could execute the following commands:

$ cd examples/cxx/tree/hello
$ make

The following make variables affect the compilation process and can
be overridden from the command line:

CXX         - C++ compiler, by default 'g++'
CXXFLAGS    - C++ options
CPPFLAGS    - C/C++ Preprocessor options

LIBS        - Libraries to link with, by default '-lxerces-c' for the
              C++/Tree examples and either '-lxerces-c' or '-lexpat' for
              the C++/Parser examples, depending on XML_PARSER
LDFLAGS     - Linker options

XSD         - XSD compiler, by default path to the XSD binary
XSDFLAGS    - XSD options

WITH_ZLIB   - Set this variable to 1 if you would like to build examples
              that depend on the zlib library

WITH_ACE    - Set this variable to 1 if you would like to build examples
              that depend on the ACE library

WITH_XDR    - Set this variable to 1 if you would like to build examples
              that depend on the XDR API (available out of the box on
	      most GNU/Linux and UNIX systems)

WITH_BOOST  - Set this variable to 1 if you would like to build examples
              that depend on the Boost date_time and serialization
	      libraries

WITH_XQILLA - Set this variable to 1 if you would like to build examples
              that depend on the XQilla library (XPath 2)

WITH_DBXML  - Set this variable to 1 if you would like to build examples
              that depend on the Berkeley DB XML library

Additionally, makefiles for the C++/Parser examples (examples/cxx/parser/)
allow you to choose the underlying XML parser:

XML_PARSER  - Underlying XML parser, can be 'xerces' (default) or 'expat'


For instance, if you would like to build an example using g++-4.0 instead
of the default g++ and would like to use Xerces-C++ from ~/xerces-c instead
of the default, system-wide installation, you could execute the following
command:

$ make CXX=g++-4.0 \
       CPPFLAGS="-I ~/xerces-c/include" \
       LDFLAGS="-L ~/xerces-c/lib"
