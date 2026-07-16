// file      : examples/cxx/tree/custom/calendar/xml-schema-custom.hxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

// Do not include this file directly, use xml-schema.hxx instead. This
// file is included into generated xml-schema.hxx so we do not need to
// guard against multiple inclusions.
//

#include <boost/date_time/gregorian/gregorian.hpp> // boost::gregorian::date

namespace xml_schema
{
  class date: public simple_type,
              public boost::gregorian::date
  {
  public:
    // Parsing c-tors: element, attribute, and list item.
    //
    date (const xercesc::DOMElement&, flags = 0, container* = 0);
    date (const xercesc::DOMAttr&, flags = 0, container* = 0);
    date (const std::string&,
          const xercesc::DOMElement*,
          flags = 0,
          container* = 0);

    // Copy c-tor and _clone.
    //
    date (const date&, flags = 0, container* = 0);

    virtual date*
    _clone (flags = 0, container* = 0) const;
  };
}
