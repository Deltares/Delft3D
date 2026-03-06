// file      : examples/cxx/tree/mixed/driver.cxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#include <memory>   // std::auto_ptr
#include <iostream>

#include <xercesc/dom/DOM.hpp>

#include "text.hxx"

// The following transcode() utility function is handy when working with
// Xerces. Include it after the generated header in order to get only char
// or wchar_t version depending on how you compiled your schemas.
//
#include <xsd/cxx/xml/string.hxx>

using std::cout;
using std::cerr;
using std::endl;
using std::auto_ptr;

int
main (int argc, char* argv[])
{
  if (argc != 2)
  {
    cerr << "usage: " << argv[0] << " text.xml" << endl;
    return 1;
  }

  using namespace xercesc;

  int r (0);

  // The Xerces-C++ DOM objects that will be associated with the
  // document tree "out-live" the call to the parsing function.
  // Therefore we need to initialize the Xerces-C++ runtime
  // ourselves.
  //
  XMLPlatformUtils::Initialize ();

  try
  {
    auto_ptr<text> t (
      text_ (argv[1],
             xml_schema::flags::keep_dom |
             xml_schema::flags::dont_initialize));

    // Note that DOM association is preserved in copies but only if they
    // are "complete", i.e., made from the root of the tree.
    //
    text copy (*t);

    // Print text.
    //
    {
      namespace xml = xsd::cxx::xml;

      unsigned long ref (0);
      DOMNode* root (copy._node ());

      for (DOMNode* n (root->getFirstChild ());
           n != 0;
           n = n->getNextSibling ())
      {
        switch (n->getNodeType ())
        {
        case DOMNode::TEXT_NODE:
          {
            cout << xml::transcode<char> (n->getTextContent ());
            break;
          }
        case DOMNode::ELEMENT_NODE:
          {
            // Let's get back to a tree node from this DOM node.
            //
            xml_schema::type& t (
              *reinterpret_cast<xml_schema::type*> (
                n->getUserData (xml_schema::dom::tree_node_key)));

	    anchor& a (dynamic_cast<anchor&> (t));

            cout << a << "[" << ref << "]";

            // Or we could continue using DOM interface:
            //
            //cout << xml::transcode<char> (n->getTextContent ())
            //     << "[" << ref << "]";

            ++ref;
            break;
          }
        default:
          break; // Ignore all other nodes (e.g., comments, etc).
        }
      }
    }

    // Print references.
    //
    {
      unsigned long r (0);

      for (text::a_const_iterator i (copy.a ().begin ());
           i != copy.a ().end ();
           ++i, ++r)
      {
        cout << "[" << r << "] "  << i->href () << endl;
      }
    }
  }
  catch (const xml_schema::exception& e)
  {
    cerr << e << endl;
    r = 1;
  }

  XMLPlatformUtils::Terminate ();
  return r;
}
