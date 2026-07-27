// file      : examples/cxx/tree/custom/mixed/driver.cxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#include <memory>   // std::auto_ptr
#include <iostream>

#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/PlatformUtils.hpp>

#include "people.hxx"

// The following transcode() utility function is handy when working with
// Xerces. Include it after the generated header in order to get only char
// or wchar_t version depending on how you compiled your schemas.
//
#include <xsd/cxx/xml/string.hxx>

using std::cerr;
using std::endl;
using namespace xercesc;


void
xhtml2txt (const DOMElement*);

int
main (int argc, char* argv[])
{
  if (argc != 2)
  {
    cerr << "usage: " << argv[0] << " people.xml" << endl;
    return 1;
  }

  int r (0);

  // The Xerces-C++ DOM document that will be used to store the XHTML
  // fragments "out-live" the call to the parsing function. Therefore
  // we need to initialize the Xerces-C++ runtime ourselves.
  //
  XMLPlatformUtils::Initialize ();

  try
  {
    using namespace people;

    // Parse.
    //
    std::auto_ptr<directory> d (
      directory_ (argv[1], xml_schema::flags::dont_initialize));

    // Print what we've got.
    //
    const directory::person_sequence& s (d->person ());

    for (directory::person_const_iterator i (s.begin ()); i != s.end (); ++i)
    {
      cerr << "First  : " << i->first_name () << endl
           << "Last   : " << i->last_name () << endl
           << "Gender : " << i->gender () << endl
           << "Age    : " << i->age () << endl;

      const bio& b (i->bio ());
      const DOMElement* xhtml (b.xhtml ());

      if (xhtml != 0)
      {
        cerr << "Bio    : " << endl;
        xhtml2txt (xhtml);
      }

      cerr << endl;
    }

    // Serialize.
    //
    xml_schema::namespace_infomap map;

    map["ppl"].name = "http://www.codesynthesis.com/people";
    map["ppl"].schema = "people.xsd";

    directory_ (
      std::cout, *d, map, "UTF-8", xml_schema::flags::dont_initialize);
  }
  catch (const xml_schema::exception& e)
  {
    cerr << e << endl;
    r = 1;
  }

  XMLPlatformUtils::Terminate ();
  return r;
}

// Primitive XHTML to text converter that just prints all the text
// nodes and ignores everything else.
//
void
xhtml2txt (const DOMElement* e)
{
  namespace xml = xsd::cxx::xml;

  for (const DOMNode* n (e->getFirstChild ());
       n != 0;
       n = n->getNextSibling ())
  {
    switch (n->getNodeType ())
    {
    case DOMNode::TEXT_NODE:
      {
        cerr << xml::transcode<char> (n->getTextContent ());
        break;
      }
    case DOMNode::ELEMENT_NODE:
      {
        xhtml2txt (static_cast<const DOMElement*> (n));
        break;
      }
    default:
      break; // Ignore all other nodes (e.g., comments, etc).
    }
  }
}
