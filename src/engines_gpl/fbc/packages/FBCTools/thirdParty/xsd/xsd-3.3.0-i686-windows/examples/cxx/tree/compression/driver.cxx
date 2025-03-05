// file      : examples/cxx/tree/compression/driver.cxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#include <memory>   // std::auto_ptr
#include <fstream>
#include <iostream>

#include <xercesc/util/PlatformUtils.hpp>

#include "library.hxx"

#include "compressed-input-source.hxx"
#include "compressed-format-target.hxx"

using namespace std;

int
main (int argc, char* argv[])
{
  if (argc != 2)
  {
    cerr << "usage: " << argv[0] << " library.xml" << endl;
    return 1;
  }

  int r (0);

  // We need to initialize the Xerces-C++ runtime because we are
  // using the Xerces-C++ input/output interfaces.
  //
  xercesc::XMLPlatformUtils::Initialize ();

  try
  {
    using namespace library;

    // Read in the XML file and obtain its object model.
    //
    ifstream ifs;
    ifs.exceptions (ifstream::badbit | ifstream::failbit);
    ifs.open (argv[1], ifstream::in | ifstream::binary);

    compressed_input_source cis (ifs, compressed_input_source::gzip, argv[1]);

    std::auto_ptr<catalog> c (
      catalog_ (cis, xml_schema::flags::dont_initialize));


    // Let's print what we've got.
    //
    for (catalog::book_const_iterator bi (c->book ().begin ());
         bi != c->book ().end ();
         ++bi)
    {
      cerr << endl
           << "ID           : " << bi->id () << endl
           << "ISBN         : " << bi->isbn () << endl
           << "Title        : " << bi->title () << endl
           << "Genre        : " << bi->genre () << endl;

      for (book::author_const_iterator ai (bi->author ().begin ());
           ai != bi->author ().end ();
           ++ai)
      {
        cerr << "Author       : " << ai->name () << endl;
        cerr << "  Born       : " << ai->born () << endl;

        if (ai->died ())
          cerr << "  Died       : " << *ai->died () << endl;

        if (ai->recommends ())
          cerr << "  Recommends : " << (*ai->recommends ())->title () << endl;
      }

      cerr  << "Available    : " << std::boolalpha << bi->available () << endl;
    }

    // Prepare namespace mapping and schema location information.
    //
    xml_schema::namespace_infomap map;

    map["lib"].name = "http://www.codesynthesis.com/library";
    map["lib"].schema = "library.xsd";

    ofstream ofs;
    ofs.exceptions (ofstream::badbit | ofstream::failbit | ofstream::eofbit);
    ofs.open ("out.xml.gz", ofstream::out | ofstream::binary);

    compressed_format_target cft (ofs, compressed_format_target::gzip);

    // Write it out.
    //
    catalog_ (cft, *c, map, "UTF-8", xml_schema::flags::dont_initialize);

    // Write out the compression stream trailer. If we don't do this
    // explicitly, it will be done automatically in the cft's destructor
    // but then any exceptions that might be throws will be ignored.
    //
    cft.close();
  }
  catch (const xml_schema::exception& e)
  {
    cerr << e << endl;
    r = 1;
  }
  catch (const compression_failure& e)
  {
    cerr << "compression failure: " << e.message () << endl;
    r = 1;
  }
  catch (const decompression_failure& e)
  {
    cerr << "decompression failure: " << e.message () << endl;
    r = 1;
  }
  catch (const ios_base::failure&)
  {
    cerr << "file open or read/write failure" << endl;
    r = 1;
  }

  xercesc::XMLPlatformUtils::Terminate ();
  return r;
}
