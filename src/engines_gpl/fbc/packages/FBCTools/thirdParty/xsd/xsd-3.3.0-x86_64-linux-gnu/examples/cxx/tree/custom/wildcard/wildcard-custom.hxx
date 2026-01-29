// file      : examples/cxx/tree/custom/wildcard/wildcard-custom.hxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

// Do not include this file directly, use wildcard.hxx instead. This
// file is included into generated wildcard.hxx so we do not need to
// guard against multiple inclusions.
//

namespace wildcard
{
  class data: public data_base
  {
    // Standard constructors.
    //
  public:
    data (const xml_schema::string&);

    data (const xercesc::DOMElement&,
          xml_schema::flags = 0,
          xml_schema::container* = 0);

    data (const data&,
          xml_schema::flags = 0,
          xml_schema::container* = 0);

    virtual data*
    _clone (xml_schema::flags = 0,
            xml_schema::container* = 0) const;

    // Our customizations.
    //
  public:
    bool
    scope_present () const
    {
      return scope_present_;
    }

    const xml_schema::string&
    scope () const
    {
      return scope_;
    }

    void
    scope (const xml_schema::string& s)
    {
      scope_present_ = true;
      scope_ = s;
    }

  private:
    bool scope_present_;
    xml_schema::string scope_;
  };

  // Serialization operator.
  //
  void
  operator<< (xercesc::DOMElement&, const data&);

  // std::ostream insertion operator.
  //
  std::ostream&
  operator<< (std::ostream&, const data&);
}
