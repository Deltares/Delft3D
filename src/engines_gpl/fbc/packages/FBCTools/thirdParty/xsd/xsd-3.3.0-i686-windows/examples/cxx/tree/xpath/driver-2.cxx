// file      : examples/cxx/tree/xpath/driver.cxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#include <memory>   // std::auto_ptr
#include <string>
#include <fstream>
#include <iostream>

#include <xercesc/dom/DOM.hpp>

#include <xqilla/xqilla-dom3.hpp>

#include <xsd/cxx/xml/string.hxx>       // xml::string, xml::transcode

#include "dom-parse.hxx"

#include "people.hxx"

using namespace std;
using namespace xercesc;
namespace xml = xsd::cxx::xml;

int
main (int argc, char* argv[])
{
  if (argc != 2)
  {
    cerr << "usage: " << argv[0] << " people.xml" << endl;
    return 1;
  }

  int r (0);

  // Initialise Xerces-C++ and XQilla.
  //
  XQillaPlatformUtils::initialize();

  // Get the XQilla DOMImplementation object with support for XPath.
  //
  DOMImplementation* impl (
    DOMImplementationRegistry::getDOMImplementation(
      xml::string ("XPath2 3.0").c_str ()));

  try
  {
    using namespace people;

    ifstream ifs;
    ifs.exceptions (ifstream::badbit | ifstream::failbit);
    ifs.open (argv[1]);

    // Parse the XML file to DOM using the XQilla DOMImplementation.
    //
    xml_schema::dom::auto_ptr<xercesc::DOMDocument> dom (
      parse (ifs, argv[1], true, impl));

    // Parse the DOM document to the object model. We also request that
    // the DOM document to be associated with the object model.
    //
    std::auto_ptr<directory> d (
      directory_ (dom,
                  xml_schema::flags::keep_dom | xml_schema::flags::own_dom));

    // Obtain the root element and document corresponding to the
    // directory object.
    //
    DOMElement* root (static_cast<DOMElement*> (d->_node ()));
    DOMDocument* doc (root->getOwnerDocument ());

    // Obtain namespace resolver.
    //
    xml_schema::dom::auto_ptr<XQillaNSResolver> resolver (
      (XQillaNSResolver*)doc->createNSResolver (root));

    // Set the namespace prefix for the people namespace that we can
    // use reliably in XPath expressions regardless of what is used
    // in XML documents.
    //
    resolver->addNamespaceBinding (
      xml::string ("p").c_str (),
      xml::string ("http://www.codesynthesis.com/people").c_str ());

    // Create XPath expression.
    //
    xml_schema::dom::auto_ptr<const XQillaExpression> expr (
      static_cast<const XQillaExpression*> (
        doc->createExpression (
          xml::string ("p:directory/person[age > 30]").c_str (),
          resolver.get ())));

    // Execute the query.
    //
    xml_schema::dom::auto_ptr<XPath2Result> r (
      static_cast<XPath2Result*> (
        expr->evaluate (doc, XPath2Result::ITERATOR_RESULT, 0)));

    // Iterate over the result.
    //
    cout << "Records matching the query:" << endl;

    while (r->iterateNext ())
    {
      const DOMNode* n (r->asNode ());

      // Obtain the object model node corresponding to this DOM node.
      //
      person* p (
        static_cast<person*> (
          n->getUserData (xml_schema::dom::tree_node_key)));

      // Print the data using the object model.
      //
      cout << endl
           << "First  : " << p->first_name () << endl
           << "Last   : " << p->last_name () << endl
           << "Gender : " << p->gender () << endl
           << "Age    : " << p->age () << endl;
    }
  }
  catch(const DOMException& e)
  {
    cerr << xml::transcode<char> (e.getMessage ()) << std::endl;
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

  XQillaPlatformUtils::terminate();
  return r;
}
