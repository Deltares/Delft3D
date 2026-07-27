// file      : examples/cxx/tree/custom/comments/xml-schema-custom.hxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

// Do not include this file directly, use xml-schema.hxx instead. This
// file is included into generated xml-schema.hxx so we do not need to
// guard against multiple inclusions.
//

#include <string>

namespace xml_schema
{
  // When customizing anyType always inherit from the original type.
  //
  class type: public type_base
  {
  public:
    type ();
    type (const xercesc::DOMElement&, flags = 0, container* = 0);
    type (const xercesc::DOMAttr&, flags = 0, container* = 0);
    type (const std::string&, const xercesc::DOMElement*,
          flags = 0, container* = 0);
    type (const type&, flags = 0, container* = 0);

    virtual type*
    _clone (flags = 0, container* = 0) const;

  public:
    // Comment manipulation API.
    //
    const std::string&
    comment () const
    {
      return comment_;
    }

    void
    comment (const std::string& c)
    {
      comment_ = c;
    }

  private:
    std::string comment_;
  };

  // New serialization operators.
  //
  void
  operator<< (xercesc::DOMElement&, const type&);

  void
  operator<< (xercesc::DOMAttr&, const type&);

  void
  operator<< (xml_schema::list_stream&, const type&);
}
