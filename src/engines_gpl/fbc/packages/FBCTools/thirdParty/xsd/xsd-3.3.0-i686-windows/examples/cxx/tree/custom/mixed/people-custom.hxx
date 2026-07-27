// file      : examples/cxx/tree/custom/mixed/people-custom.hxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

// Do not include this file directly, use people.hxx instead. This
// file is included into generated people.hxx so we do not need to
// guard against multiple inclusions.
//

#include <cassert>
#include <xercesc/dom/DOM.hpp>

namespace people
{
  class bio: public bio_base
  {
    // Standard constructors.
    //
  public:
    bio ();

    bio (const xercesc::DOMElement&,
         xml_schema::flags = 0,
         xml_schema::container* = 0);

    bio (const bio&,
         xml_schema::flags = 0,
         xml_schema::container* = 0);

    virtual bio*
    _clone (xml_schema::flags = 0,
            xml_schema::container* = 0) const;

    // XHTML bio as a DOM document.
    //
  public:
    const xercesc::DOMElement*
    xhtml () const
    {
      return xhtml_;
    }

    xercesc::DOMElement*
    xhtml ()
    {
      return xhtml_;
    }

    // The element should belong to the DOMDocument returned by
    // the dom_document() functions.
    //
    void
    xhtml (xercesc::DOMElement* e)
    {
      assert (e->getOwnerDocument () == doc_.get ());

      if (xhtml_ != 0)
        xhtml_->release ();

      xhtml_ = e;
    }

    const xercesc::DOMDocument&
    dom_document () const
    {
      return *doc_;
    }

    xercesc::DOMDocument&
    dom_document ()
    {
      return *doc_;
    }

  private:
    xercesc::DOMElement* xhtml_;
    xml_schema::dom::auto_ptr<xercesc::DOMDocument> doc_;
  };

  // Serialization operator.
  //
  void
  operator<< (xercesc::DOMElement&, const bio&);
}
