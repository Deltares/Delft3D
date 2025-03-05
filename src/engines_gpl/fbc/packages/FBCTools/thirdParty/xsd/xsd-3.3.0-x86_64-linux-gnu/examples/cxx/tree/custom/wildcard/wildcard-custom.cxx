// file      : examples/cxx/tree/custom/wildcard/wildcard-custom.cxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#include <ostream>

// Include wildcard.hxx instead of wildcard-custom.hxx here.
//
#include "wildcard.hxx"

namespace wildcard
{
  data::
  data (const xml_schema::string& d)
      : data_base (d), scope_present_ (false)
  {
  }

  data::
  data (const xercesc::DOMElement& e,
        xml_schema::flags f,
        xml_schema::container* c)
      : data_base (e, f, c), scope_present_ (false)
  {
    // Check if we've got the scope attribute.
    //
    namespace xml = xsd::cxx::xml;
    xml::string name ("scope");

    if (e.hasAttribute (name.c_str ()))
    {
      scope (xml::transcode<char> (e.getAttribute (name.c_str ())));
    }
  }

  data::
  data (const data& d,
        xml_schema::flags f,
        xml_schema::container* c)
      : data_base (d, f, c),
        scope_present_ (d.scope_present_),
        scope_ (d.scope_)
  {
  }

  data* data::
  _clone (xml_schema::flags f, xml_schema::container* c) const
  {
    return new data (*this, f, c);
  }

  void
  operator<< (xercesc::DOMElement& e, const data& x)
  {
    // Use our base to serialize data and id.
    //
    const data_base& b (x);
    e << b;

    // Add the scope attribute if present.
    //
    if (x.scope_present ())
    {
      namespace xml = xsd::cxx::xml;
      xml::string name ("scope");
      xml::string value (x.scope ());

      e.setAttribute (name.c_str (), value.c_str ());
    }
  }

  std::ostream&
  operator<< (std::ostream& os, const data& x)
  {
    // Use our base to print date and id.
    //
    const data_base& b (x);
    os << b;

    if (x.scope_present ())
      os << std::endl << "scope: " << x.scope ();

    return os;
  }
}
