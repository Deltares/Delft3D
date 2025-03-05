// file      : examples/cxx/tree/custom/mixed/people-custom.cxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#include <ostream>

// Include people.hxx instead of people-custom.hxx here.
//
#include "people.hxx"

namespace people
{
  using namespace xercesc;

  const XMLCh ls[] = {chLatin_L, chLatin_S, chNull};

  bio::
  bio ()
      : xhtml_ (0)
  {
    DOMImplementation* impl (
      DOMImplementationRegistry::getDOMImplementation (ls));

    doc_.reset (impl->createDocument ());
  }

  bio::
  bio (const DOMElement& e,
       xml_schema::flags f,
       xml_schema::container* c)
      : bio_base (e, f, c), xhtml_ (0)
  {
    DOMImplementation* impl (
      DOMImplementationRegistry::getDOMImplementation (ls));

    doc_.reset (impl->createDocument ());

    // Copy the xhtml element. Assume the first child element in
    // e is always xhtml.
    //
    for (DOMNode* n (e.getFirstChild ()); n != 0; n = n->getNextSibling ())
    {
      if (n->getNodeType () == DOMNode::ELEMENT_NODE)
      {
        xhtml_ = static_cast<DOMElement*> (doc_->importNode (n, true));
        break;
      }
    }
  }

  bio::
  bio (const bio& d,
       xml_schema::flags f,
       xml_schema::container* c)
      : bio_base (d, f, c), xhtml_ (0)
  {
    DOMImplementation* impl (
      DOMImplementationRegistry::getDOMImplementation (ls));

    doc_.reset (impl->createDocument ());

    xhtml_ = static_cast<DOMElement*> (
      doc_->importNode (const_cast<DOMElement*> (d.xhtml_), true));
  }

  bio* bio::
  _clone (xml_schema::flags f, xml_schema::container* c) const
  {
    return new bio (*this, f, c);
  }

  void
  operator<< (DOMElement& e, const bio& x)
  {
    // Allow our base to serialize first.
    //
    const bio_base& b (x);
    e << b;

    // Copy the XHTML fragment if we have one.
    //
    const DOMElement* xhtml (x.xhtml ());

    if (xhtml != 0)
    {
      DOMDocument* doc (e.getOwnerDocument ());
      e.appendChild (doc->importNode (const_cast<DOMElement*> (xhtml), true));
    }
  }
}
