// file      : examples/cxx/tree/custom/contacts/contacts-custom.hxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

// Do not include this file directly, use contacts.hxx instead. This
// file is included into generated contacts.hxx so we do not need to
// guard against multiple inclusions.
//

#include <iosfwd> // std::ostream

namespace contacts
{
  class contact: public contact_base
  {
    // The following constructor signatures are copied from
    // contact_base except for the copy constructor and the
    // _clone function where we had to change the type from
    // contact_base to contact.
    //
  public:
    contact (const name_type&,
             const email_type&,
             const phone_type&);

    contact (const xercesc::DOMElement&,
             xml_schema::flags = 0,
             xml_schema::container* = 0);

    contact (const contact&,
             xml_schema::flags = 0,
             xml_schema::container* = 0);

    virtual contact*
    _clone (xml_schema::flags = 0,
            xml_schema::container* = 0) const;

    // Our customizations.
    //
  public:
    void
    print (std::ostream&) const;
  };
}
