This example shows how to save/load the object model to/from a custom
format using the Boost serialization library as an example. You will
need the Boost serialization library[1] installed in order to build
and run this example.

[1] http://www.boost.org

The example consists of the following files:

library.xsd
  XML Schema which describes a library of books.

library.xml
  Sample XML instance document.

boost-archive-extraction.hxx
boost-archive-insertion.hxx
  Boost archive insertion and extraction operators for fundamental
  types. You will need to provide a similar set of operators for
  your own stream types.

library-prologue.hxx
  Contains a number of #include directives that are inserted into
  the generated code by the XSD compiler. The included files are:
  boost/archive/text_oarchive.hpp, boost/archive/text_oarchive.hpp,
  boost-archive-insertion.hxx, and boost-archive-insertion.hxx.

library.hxx
library.cxx
  C++ types that represent the given vocabulary as well as Boost
  archive insertion and extraction operations. These are generated
  by the XSD compiler from library.xsd. The --hxx-prologue-file
  option is used to insert the contents of the library-prologue.hxx
  file into the generated header file. The --generate-insertion and
  --generate-extraction options are used to generate the insertion
  and extraction operations for text_oarchive and text_iarchive
  types.

driver.cxx
  Driver for the example. It first calls one of the parsing functions
  that constructs the object model from the input XML file. It then
  saves the object model to text_oarchive and loads it back from
  text_iarchive. Additionally, it prints the resulting text
  representation as well as the content of the object model before
  saving it to text_oarchive and after loading it from text_iarchive.

To run the example on the sample XML instance document simply execute:

$ ./driver library.xml
