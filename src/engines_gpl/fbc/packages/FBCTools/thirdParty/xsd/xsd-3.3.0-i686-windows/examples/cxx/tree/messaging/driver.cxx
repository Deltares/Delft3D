// file      : examples/cxx/tree/messaging/driver.cxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#include <memory>   // std::auto_ptr
#include <string>
#include <fstream>
#include <typeinfo>
#include <iostream>

#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/PlatformUtils.hpp>

#include <xsd/cxx/xml/string.hxx>

#include "dom-parse.hxx"
#include "dom-serialize.hxx"

#include "protocol.hxx"

using namespace std;
using namespace protocol;
using namespace xercesc;

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
  XMLPlatformUtils::Initialize ();

  try
  {
    ifstream ifs;
    ifs.exceptions (ifstream::badbit | ifstream::failbit);
    ifs.open (argv[1]);

    auto_ptr<xml_schema::element_type> req, res;

    // Parse the XML request to a DOM document using the parse()
    // function from dom-parse.hxx.
    //
    {
      xml_schema::dom::auto_ptr<DOMDocument> doc (parse (ifs, argv[1], true));
      DOMElement& root (*doc->getDocumentElement ());

      req = xml_schema::element_map::parse (root);
    }

    // We can test which request we've got either using RTTI or by
    // comparing the element names, as shown below.
    //
    if (balance* b = dynamic_cast<balance*> (req.get ()))
    {
      account_t& a (b->value ());

      cerr << "balance request for acc# " << a.account () << endl;

      res.reset (new success (balance_t (a.account (), 1000)));
    }
    else if (req->_name () == withdraw::name ())
    {
      withdraw& w (static_cast<withdraw&> (*req));
      change_t& c (w.value ());

      wcerr << "withdrawal request for acc# " << c.account () << ", "
            << "amount: " << c.amount () << endl;

      if (c.amount () > 1000)
        res.reset (new insufficient_funds (balance_t (c.account (), 1000)));
      else
        res.reset (new success (balance_t (c.account (), 1000 - c.amount ())));

    }
    else if (req->_name () == deposit::name ())
    {
      deposit& d (static_cast<deposit&> (*req));
      change_t& c (d.value ());

      wcerr << "deposit request for acc# " << c.account () << ", "
            << "amount: " << c.amount () << endl;

      res.reset (new success (balance_t (c.account (), 1000 + c.amount ())));
    }

    // Serialize the response to a DOM document.
    //
    namespace xml = xsd::cxx::xml;

    const XMLCh ls_id [] = {chLatin_L, chLatin_S, chNull};

    DOMImplementation* impl (
      DOMImplementationRegistry::getDOMImplementation (ls_id));

    const string& name (res->_name ());
    const string& ns (res->_namespace ());

    xml_schema::dom::auto_ptr<DOMDocument> doc (
      impl->createDocument (
        xml::string (ns).c_str (),
        xml::string ("p:" + name).c_str (),
        0));

    xml_schema::element_map::serialize (*doc->getDocumentElement (), *res);

    // Serialize the DOM document to XML using the serialize() function
    // from dom-serialize.hxx.
    //
    cout << "response:" << endl
         << endl;

    serialize (cout, *doc);
  }
  catch (const xml_schema::no_element_info& e)
  {
    // This exception indicates that we tried to parse or serialize
    // an unknown element.
    //
    cerr << "unknown request: " << e.element_namespace () << "#" <<
      e.element_name () << endl;
    r = 1;
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

  XMLPlatformUtils::Terminate ();
  return r;
}
