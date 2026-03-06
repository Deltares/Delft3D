// file      : examples/cxx/tree/custom/wildcard/driver.cxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#include <memory>   // std::auto_ptr
#include <iostream>

#include "wildcard.hxx"

using std::cerr;
using std::endl;

int
main (int argc, char* argv[])
{
  if (argc != 2)
  {
    cerr << "usage: " << argv[0] << " wildcard.xml" << endl;
    return 1;
  }

  try
  {
    using namespace wildcard;

    // Parse.
    //
    std::auto_ptr<data> d (data_ (argv[1]));

    // Print.
    //
    cerr << *d << endl;

    // Serialize.
    //
    xml_schema::namespace_infomap map;

    map["wc"].name = "http://www.codesynthesis.com/wildcard";
    map["wc"].schema = "wildcard.xsd";

    data_ (std::cout, *d, map);
  }
  catch (const xml_schema::exception& e)
  {
    cerr << e << endl;
    return 1;
  }
}
