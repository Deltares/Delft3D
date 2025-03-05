// file      : examples/cxx/tree/custom/calendar/xml-schema-custom.cxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

// Include xml-schema.hxx instead of xml-schema-custom.hxx here.
//
#include "xml-schema.hxx"

#include <xsd/cxx/xml/string.hxx> // xsd::cxx::xml::transcode
#include <xsd/cxx/tree/text.hxx> // xsd::cxx::tree::text_content

using namespace boost;
using namespace boost::gregorian;

namespace xml_schema
{
  date::
  date (const xercesc::DOMElement& e, flags f, container* c)
      : simple_type (e, f, c),
        gregorian::date (
          from_simple_string (
            xsd::cxx::tree::text_content<char> (e)))
  {
  }

  date::
  date (const xercesc::DOMAttr& a, flags f, container* c)
      : simple_type (a, f, c),
        gregorian::date (
          from_simple_string (
            xsd::cxx::xml::transcode<char> (a.getValue ())))
  {
  }

  date::
  date (const std::string& s,
        const xercesc::DOMElement* e,
        flags f,
        container* c)
      : simple_type (s, e, f, c),
        gregorian::date (from_simple_string (s))
  {
  }

  date::
  date (const date& d, flags f, container* c)
      : simple_type (d, f, c),
        gregorian::date (d)
  {
  }

  date* date::
  _clone (flags f, container* c) const
  {
    return new date (*this, f, c);
  }
}
