// file      : examples/cxx/tree/custom/taxonomy/people-custom.cxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#include <ostream>

// Include people.hxx instead of people-custom.hxx here.
//
#include "people.hxx"

namespace people
{
  // person_impl
  //
  template <typename base>
  person_impl<base>::
  person_impl (const xml_schema::string& name)
      : base (name)
  {
  }

  template <typename base>
  person_impl<base>::
  person_impl (const xercesc::DOMElement& e,
               xml_schema::flags f,
               xml_schema::container* c)
      : base (e, f, c)
  {
  }

  template <typename base>
  person_impl<base>::
  person_impl (const person_impl& p,
               xml_schema::flags f,
               xml_schema::container* c)
      : base (p, f, c)
  {
  }

  template <typename base>
  person_impl<base>* person_impl<base>::
  _clone (xml_schema::flags f, xml_schema::container* c) const
  {
    return new person_impl (*this, f, c);
  }

  template <typename base>
  void person_impl<base>::
  print (std::ostream& os) const
  {
    os << this->name () << std::endl;
  }

  // Explicitly instantiate person_impl class template for person_base.
  //
  template class person_impl<person_base>;


  // superman_impl
  //
  template <typename base>
  superman_impl<base>::
  superman_impl (const xml_schema::string& name, bool can_fly)
      : base (name, can_fly)
  {
  }

  template <typename base>
  superman_impl<base>::
  superman_impl (const xercesc::DOMElement& e,
                 xml_schema::flags f,
                 xml_schema::container* c)
      : base (e, f, c)
  {
  }

  template <typename base>
  superman_impl<base>::
  superman_impl (const superman_impl& s,
                 xml_schema::flags f,
                 xml_schema::container* c)
      : base (s, f, c)
  {
  }

  template <typename base>
  superman_impl<base>* superman_impl<base>::
  _clone (xml_schema::flags f, xml_schema::container* c) const
  {
    return new superman_impl (*this, f, c);
  }

  template <typename base>
  void superman_impl<base>::
  print (std::ostream& os) const
  {
    if (this->can_fly ())
      os << "Flying superman ";
    else
      os << "Superman ";

    os << this->name () << std::endl;
  }

  // Explicitly instantiate superman_impl class template for superman_base.
  //
  template class superman_impl<superman_base>;


  // batman_impl
  //
  template <typename base>
  batman_impl<base>::
  batman_impl (const xml_schema::string& name,
               bool can_fly,
               unsigned int wing_span)
      : base (name, can_fly, wing_span)
  {
  }

  template <typename base>
  batman_impl<base>::
  batman_impl (const xercesc::DOMElement& e,
               xml_schema::flags f,
               xml_schema::container* c)
      : base (e, f, c)
  {
  }

  template <typename base>
  batman_impl<base>::
  batman_impl (const batman_impl& s,
               xml_schema::flags f,
               xml_schema::container* c)
      : base (s, f, c)
  {
  }

  template <typename base>
  batman_impl<base>* batman_impl<base>::
  _clone (xml_schema::flags f, xml_schema::container* c) const
  {
    return new batman_impl (*this, f, c);
  }

  template <typename base>
  void batman_impl<base>::
  print (std::ostream& os) const
  {
    os << "Batman " << this->name () << " with " <<
      this->wing_span () << "m wing span" << std::endl;
  }

  // Explicitly instantiate batman_impl class template for batman_base.
  //
  template class batman_impl<batman_base>;
}
