// file      : examples/cxx/tree/custom/contacts/contacts-custom.cxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#include <ostream>

// Include contacts.hxx instead of contacts-custom.hxx here.
//
#include "contacts.hxx"

namespace contacts
{
  // We implement the following constructs by simply forwarding
  // to our base.
  //
  contact::
  contact (const name_type& n,
           const email_type& e,
           const phone_type& p)
      : contact_base (n, e, p)
  {
  }

  contact::
  contact (const xercesc::DOMElement& e,
           xml_schema::flags f,
           xml_schema::container* c)
      : contact_base (e, f, c)
  {
  }

  contact::
  contact (const contact& x,
           xml_schema::flags f,
           xml_schema::container* c)
      : contact_base (x, f, c)
  {
  }

  contact* contact::
  _clone (xml_schema::flags f, xml_schema::container* c) const
  {
    return new contact (*this, f, c);
  }

  void contact::
  print (std::ostream& os) const
  {
    os << name () << " e| " << email () << " t| " << phone () << std::endl;
  }
}
