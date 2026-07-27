// file      : examples/cxx/tree/custom/taxonomy/people-custom.hxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

// Do not include this file directly, use people.hxx instead. This
// file is included into generated people.hxx so we do not need to
// guard against multiple inclusions.
//

#include <iosfwd> // std::ostream

// Include people-fwd.hxx here so that we can refer to the generated
// types.
//
#include "people-fwd.hxx"

namespace people
{
  //
  //
  template <typename base>
  class person_impl: public base
  {
  public:
    person_impl (const xml_schema::string& name);

    person_impl (const xercesc::DOMElement&,
                 xml_schema::flags = 0,
                 xml_schema::container* = 0);

    person_impl (const person_impl&,
                 xml_schema::flags = 0,
                 xml_schema::container* = 0);

    virtual person_impl*
    _clone (xml_schema::flags = 0,
            xml_schema::container* = 0) const;

  public:
    virtual void
    print (std::ostream&) const;
  };


  //
  //
  template <typename base>
  class superman_impl: public base
  {
  public:
    superman_impl (const xml_schema::string& name, bool can_fly);

    superman_impl (const xercesc::DOMElement&,
                   xml_schema::flags = 0,
                   xml_schema::container* = 0);

    superman_impl (const superman_impl&,
                   xml_schema::flags = 0,
                   xml_schema::container* = 0);

    virtual superman_impl*
    _clone (xml_schema::flags = 0,
            xml_schema::container* = 0) const;

  public:
    virtual void
    print (std::ostream&) const;
  };


  //
  //
  template <typename base>
  class batman_impl: public base
  {
  public:
    batman_impl (const xml_schema::string& name,
                 bool can_fly,
                 unsigned int wing_span);

    batman_impl (const xercesc::DOMElement&,
                 xml_schema::flags = 0,
                 xml_schema::container* = 0);

    batman_impl (const batman_impl&,
                 xml_schema::flags = 0,
                 xml_schema::container* = 0);

    virtual batman_impl*
    _clone (xml_schema::flags = 0,
            xml_schema::container* = 0) const;

  public:
    virtual void
    print (std::ostream&) const;
  };
}
