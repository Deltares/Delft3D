// file      : examples/cxx/parser/polymorphism/driver.cxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#include <iostream>

#include "supermen-pimpl.hxx"

using std::cerr;
using std::endl;

int
main (int argc, char* argv[])
{
  if (argc != 2)
  {
    cerr << "usage: " << argv[0] << " supermen.xml" << endl;
    return 1;
  }

  try
  {
    // Construct the parser.
    //
    xml_schema::string_pimpl string_p;
    xml_schema::boolean_pimpl boolean_p;
    xml_schema::unsigned_int_pimpl unsigned_int_p;

    person_pimpl person_p;
    superman_pimpl superman_p;
    batman_pimpl batman_p;

    xml_schema::parser_map_impl person_map;

    supermen_pimpl supermen_p;


    person_p.parsers (string_p);
    superman_p.parsers (string_p, boolean_p);
    batman_p.parsers (string_p, boolean_p, unsigned_int_p);

    // Here we are specifying a parser map which containes several parsers
    // that can be used to parse the person element.
    //
    person_map.insert (person_p);
    person_map.insert (superman_p);
    person_map.insert (batman_p);

    supermen_p.person_parser (person_map);

    // Parse the XML document. The last argument to the document's
    // constructor indicates that we are parsing polymorphic XML
    // documents.
    //
    xml_schema::document doc_p (supermen_p, "supermen", true);

    supermen_p.pre ();
    doc_p.parse (argv[1]);
    supermen_p.post_supermen ();
  }
  catch (const xml_schema::exception& e)
  {
    cerr << e << endl;
    return 1;
  }
  catch (const std::ios_base::failure&)
  {
    cerr << argv[1] << ": unable to open or read failure" << endl;
    return 1;
  }
}
