// file      : examples/cxx/tree/custom/commens/driver.cxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#include <memory>   // std::auto_ptr
#include <fstream>
#include <iostream>

#include <xercesc/dom/DOMDocument.hpp>
#include <xercesc/util/PlatformUtils.hpp>

#include "people.hxx"
#include "dom-parse.hxx"

using namespace std;

int
main (int argc, char* argv[])
{
  if (argc != 2)
  {
    cerr << "usage: " << argv[0] << " people.xml" << endl;
    return 1;
  }

  int r (0);

  // We need to initialize the Xerces-C++ runtime because we
  // are doing the XML-to-DOM parsing ourselves (see below).
  //
  xercesc::XMLPlatformUtils::Initialize ();

  try
  {
    using namespace people;
    namespace xml = xsd::cxx::xml;

    ifstream ifs;
    ifs.exceptions (ifstream::badbit | ifstream::failbit);
    ifs.open (argv[1]);

    // For performance reasons the internal XML to DOM parsing code
    // discards comments in the resulting DOM document. To overcome
    // this we are going to use our own parse() function from 
    // dom-parse.hxx that preserves comments in the resulting DOM
    // documents.
    //
    xml_schema::dom::auto_ptr<xercesc::DOMDocument> doc (
      parse (ifs, argv[1], true));

    // Parse the DOM document to the object model.
    //
    std::auto_ptr<catalog> c (catalog_ (*doc));

    // Change the object model.
    //
    catalog::person_sequence& ps (c->person ());

    for (catalog::person_iterator i (ps.begin ()); i != ps.end (); ++i)
    {
      i->age (i->age () + 1);
    }

    person john ("John Doe", 30);
    john.comment ("Record for John Doe");

    ps.push_back (john);

    // Serialize.
    //
    xml_schema::namespace_infomap map;

    map["ppl"].name = "http://www.codesynthesis.com/people";
    map["ppl"].schema = "people.xsd";

    catalog_ (std::cout, *c, map);
  }
  catch (const xml_schema::exception& e)
  {
    cerr << e << endl;
    r = 1;
  }
  catch (const std::ios_base::failure&)
  {
    cerr << argv[1] << ": unable to open or read failure" << endl;
    r = 1;
  }

  xercesc::XMLPlatformUtils::Terminate ();
  return r;
}
