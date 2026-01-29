// file      : examples/cxx/tree/multiroot/driver.cxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#include <memory>   // std::auto_ptr
#include <string>
#include <fstream>
#include <iostream>

#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/PlatformUtils.hpp>

#include <xsd/cxx/xml/string.hxx>       // xml::transcode

#include "dom-parse.hxx"

#include "protocol.hxx"

using namespace std;
using namespace protocol;

// Parse an XML document and return a pointer to request_t which can
// then be tested with dynamic_cast. If your vocabulary does not have
// a common base type for all root element types then you can use
// xml_schema::type which is a base for all generated types.
//
auto_ptr<request_t>
parse (istream& is, const string& id)
{
  using namespace xercesc;
  namespace xml = xsd::cxx::xml;

  // Parse an XML instance to a DOM document using the parse()
  // function from dom-parse.hxx.
  //
  xml_schema::dom::auto_ptr<DOMDocument> doc (parse (is, id, true));

  DOMElement* root (doc->getDocumentElement ());

  string ns (xml::transcode<char> (root->getNamespaceURI ()));
  string name (xml::transcode<char> (root->getLocalName ()));

  auto_ptr<request_t> r;

  // We could have handled the result directly in this function
  // instead of returning it as an opaque pointer and using
  // dynamic_cast later to figure out which request we are dealing
  // with.
  //
  if (ns == "http://www.codesynthesis.com/protocol")
  {
    if (name == "balance")
    {
      // Use the balance parsing function.
      //
      r.reset (balance (*doc).release ());
    }
    else if (name == "withdraw")
    {
      // Use the withdraw parsing function.
      //
      r.reset (withdraw (*doc).release ());
    }
  }

  if (r.get () == 0)
    cerr << "ignoring unknown request: " << ns << "#" << name << endl;

  return r;
}

int
main (int argc, char* argv[])
{
  if (argc != 2)
  {
    cerr << "usage: " << argv[0] << " request.xml" << endl;
    return 1;
  }

  int r (0);

  // We need to initialize the Xerces-C++ runtime because we
  // are doing the XML-to-DOM parsing ourselves.
  //
  xercesc::XMLPlatformUtils::Initialize ();

  try
  {
    ifstream ifs;
    ifs.exceptions (ifstream::badbit | ifstream::failbit);
    ifs.open (argv[1]);

    auto_ptr<request_t> r (parse (ifs, argv[1]));

    // Let's print what we've got.
    //
    if (balance_t* b = dynamic_cast<balance_t*> (r.get ()))
    {
      cerr << "balance request for acc# " << b->account () << endl;
    }
    else if (withdraw_t* w = dynamic_cast<withdraw_t*> (r.get ()))
    {
      cerr << "withdrawal request for acc# " << w->account () << ", "
           << "amount: " << w->amount () << endl;
    }
    else
    {
      cerr << "unknown request" << endl;
    }
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
