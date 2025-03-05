// file      : examples/cxx/tree/binary/boost/driver.cxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#include <memory>   // std::auto_ptr
#include <cstring>  // std::memcpy
#include <sstream>
#include <iostream>

// You can generate insertion/extraction code for other archive
// types (for example, binary, XML, etc).
//
#include <boost/archive/text_oarchive.hpp>
#include <boost/archive/text_iarchive.hpp>

#include "library.hxx"

using std::cerr;
using std::endl;

int
main (int argc, char* argv[])
{
  if (argc != 2)
  {
    cerr << "usage: " << argv[0] << " library.xml" << endl;
    return 1;
  }

  try
  {
    using namespace library;
    using boost::archive::text_oarchive;
    using boost::archive::text_iarchive;

    // Read in the file.
    //
    std::auto_ptr<catalog> c (catalog_ (argv[1]));

    cerr << *c << endl;

    // Save into a text archive.
    //
    std::ostringstream ostr;
    text_oarchive oa (ostr);
    xml_schema::ostream<text_oarchive> os (oa);

    os << *c;

    // Print the text representation.
    //
    std::string str (ostr.str ());

    cerr << endl
         << "text representation: " << endl
         << str << endl;

    // Load from a text archive.
    //
    std::istringstream istr (str);
    text_iarchive ia (istr);
    xml_schema::istream<text_iarchive> is (ia);

    std::auto_ptr<catalog> copy (new catalog (is));

    cerr << *copy << endl;
  }
  catch (const xml_schema::exception& e)
  {
    cerr << e << endl;
    return 1;
  }
}
