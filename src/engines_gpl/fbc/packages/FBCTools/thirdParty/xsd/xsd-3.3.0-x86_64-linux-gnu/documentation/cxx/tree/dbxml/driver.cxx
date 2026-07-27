// file      : examples/cxx/tree/dbxml/driver.cxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#include <memory>   // std::auto_ptr
#include <string>
#include <cassert>
#include <iostream>

#include <dbxml/DbXml.hpp>

#include "library.hxx"

using std::cerr;
using std::endl;
using std::string;
using std::auto_ptr;

using namespace DbXml;
using namespace xsd::cxx; // for xml::string

void
print_document (const string& desc,
                XmlContainer container,
                const string& name)
{
  XmlDocument doc (container.getDocument (name));

  string content;
  doc.getContent (content);

  cerr << endl
       << desc << endl
       << content << endl;
}

int
main ()
{
  try
  {
    using namespace library;
    using xml_schema::date;

    XmlManager manager;

    {
      XmlContainer container (manager.createContainer ("new.bdbxml"));

      XmlUpdateContext update_context (manager.createUpdateContext ());

      XmlQueryContext context (manager.createQueryContext ());
      context.setNamespace ("lib", "http://www.codesynthesis.com/library");


      // Create a new document from an object model.
      //
      {
        // Create a new catalog with one book.
        //
        catalog c;

        book b (20530902,                // ISBN
                "The Elements of Style", // Title
                genre::reference,        // Genre
                "ES");                   // ID

        author strunk ("William Strunk, Jr.", date (1869, 7, 1));
        strunk.died (date (1946, 9, 26));

        b.author ().push_back (strunk);
        c.book ().push_back (b);


        // Create a new XML document.
        //
        XmlDocument doc (manager.createDocument ());
        doc.setName ("new.xml");


        // Obtain its DOM representation and add the root element.
        //
        xercesc::DOMDocument& dom_doc (*doc.getContentAsDOM ());

        dom_doc.appendChild (
          dom_doc.createElementNS (
            xml::string ("http://www.codesynthesis.com/library").c_str (),
            xml::string ("lib:catalog").c_str ()));


        // Serialize the object model to the XML document. Also avoid
	// re-initializing the Xerces-C++ runtime since XmlManager has
	// it initialized.
        //
        catalog_ (dom_doc, c, xml_schema::flags::dont_initialize);


        // Place the document into the container.
        //
        container.putDocument (doc, update_context);

        print_document ("after create:", container, "new.xml");
      }

      // Create an object model from a document in DB.
      //
      {
        // Resolve the document in the container.
        //
        XmlDocument doc (container.getDocument ("new.xml"));


        // Create the object model from the document's DOM. Also avoid
	// re-initializing the Xerces-C++ runtime since XmlManager has
	// it initialized.
        //
        auto_ptr<catalog> c (catalog_ (*doc.getContentAsDOM (),
                                       xml_schema::flags::dont_initialize));

        cerr << *c << endl;
      }


      // Lookup a document fragment.
      //

      string query ("collection('new.bdbxml')/lib:catalog/book[@id='ES']");

      // Find "The Elements of Style".
      //
      XmlValue v;
      XmlResults results (manager.query (query, context));

      if (results.next (v))
      {
        // Create an object model from the document fragment.
        //
        auto_ptr<book> b (
          new book (
            *static_cast<xercesc::DOMElement*> (v.asNode ())));

        cerr << *b << endl;


        // Add another author, change the availability status.
        //
        author white ("E.B. White", date (1899, 7, 11));
        white.died (date (1985, 10, 1));

        b->author ().push_back (white);
        b->available (false);


        // Update the document fragment from the object model.
        //
        *static_cast<xercesc::DOMElement*> (v.asNode ()) << *b;


        // Update the document in the container.
        //
        XmlDocument doc (v.asDocument ());
        container.updateDocument (doc, update_context);
      }

      print_document ("after update:", container, "new.xml");
    }

    manager.removeContainer ("new.bdbxml");
  }
  catch (const std::exception& e)
  {
    cerr << e.what () << endl;
    return 1;
  }
}
