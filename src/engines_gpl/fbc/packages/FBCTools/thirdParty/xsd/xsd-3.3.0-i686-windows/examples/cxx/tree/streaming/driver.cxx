// file      : examples/cxx/tree/streaming/driver.cxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#include <iostream>
#include <fstream>

#include <xercesc/dom/DOM.hpp>

#include <xsd/cxx/xml/string.hxx>  // xml::string

#include "parser.hxx"
#include "serializer.hxx"
#include "position.hxx"

using namespace std;
using namespace xercesc;

static void
measure_position (unsigned int n, float& lat, float& lon);

int
main (int argc, char* argv[])
{
  if (argc != 2)
  {
    cerr << "usage: " << argv[0] << " position.xml" << endl;
    return 1;
  }

  int r (0);

  // We need to initialize the Xerces-C++ runtime because we are doing
  // the XML-to-DOM parsing ourselves.
  //
  xercesc::XMLPlatformUtils::Initialize ();

  try
  {
    using namespace op;
    namespace xml = xsd::cxx::xml;

    // Parse.
    //

    ifstream ifs;
    ifs.exceptions (ifstream::badbit | ifstream::failbit);
    ifs.open (argv[1]);

    parser p;

    // The first document we get is the "carcase" of the complete document.
    // That is, the root element with all the attributes but without any
    // content. We may need it to get to the attributes in the root element.
    //
    // There are two ways this can be done. The easiest approach is to
    // instantiate the root element's type (object in our case). This
    // will only work if all the content in the root element is optional.
    // Alternatively, we can manually look up attributes that we are
    // interested in and instantiate the corresponding type. The following
    // fragment shows how to use the second approach.
    //
    xml_schema::dom::auto_ptr<DOMDocument> doc (p.start (ifs, argv[1], true));

    // Find the id attribute.
    //
    DOMAttr* id_attr (
      doc->getDocumentElement ()->getAttributeNode (
        xml::string ("id").c_str ()));

    // Use the type and traits aliases from the object model.
    //
    object::id_type id (object::id_traits::create (*id_attr, 0, 0));
    cerr << "id:   " << id << endl;

    // The next chunk we get is the header element.
    //
    doc = p.next ();
    header hdr (*doc->getDocumentElement ());
    cerr << "name: " << hdr.name () << endl
         << "type: " << hdr.type () << endl;

    // The rest is position elements.
    //
    for (doc = p.next (); doc.get () != 0; doc = p.next ())
    {
      position p (*doc->getDocumentElement ());
      cerr << "lat: " << p.lat () << " lon: " << p.lon () << endl;
    }

    // Serialize.
    //

    ofstream ofs;
    ofs.exceptions (ios_base::badbit | ios_base::failbit);
    ofs.open ("out.xml");

    serializer s;

    // With this approach we manually write the XML declaration, opening
    // and closing root element tags, as well as any attributes in the
    // root element.
    //
    ofs << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" << endl
        << "<op:object xmlns:op=\"http://www.codesynthesis.com/op\"" << endl
        << "  xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"" << endl
        << "  xsi:schemaLocation=\"http://www.codesynthesis.com/op " <<
      "position.xsd\"" << endl
        << "  id=\"" << 123 << "\">" << endl;

    s.start (ofs);

    // Serialize the header.
    //
    header h ("Lion's Head", "rock");
    s.next ("header", h);

    // Serialize position elements, one at a time.
    //
    for (unsigned short i (0); i < 8; i++)
    {
      float lat, lon;
      measure_position (i, lat, lon);
      position p (lat, lon);
      s.next ("position", p);
    }

    // Close the root element.
    //
    ofs << endl
        << "</op:object>" << endl;

  }
  catch (const xml_schema::exception& e)
  {
    cerr << e << endl;
    r = 1;
  }
  catch (const ios_base::failure&)
  {
    cerr << "io failure" << endl;
    r = 1;
  }

  xercesc::XMLPlatformUtils::Terminate ();
  return r;
}

// Position measurement instrument interface.
//
struct measurements
{
  float lat;
  float lon;
};

measurements test_measurements [8] =
{
  {-33.8569F, 18.5083F},
  {-33.8568F, 18.5083F},
  {-33.8568F, 18.5082F},
  {-33.8570F, 18.5083F},
  {-33.8569F, 18.5084F},
  {-33.8570F, 18.5084F},
  {-33.8570F, 18.5082F},
  {-33.8569F, 18.5082F}
};

static void
measure_position (unsigned int n, float& lat, float& lon)
{
  // Call the instrument to measure the position.
  //
  lat = test_measurements[n].lat;
  lon = test_measurements[n].lon;
}
