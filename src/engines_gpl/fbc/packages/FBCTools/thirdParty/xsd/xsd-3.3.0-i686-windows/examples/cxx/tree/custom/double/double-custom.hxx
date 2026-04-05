// file      : examples/cxx/tree/custom/double/double-custom.hxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

// Do not include this file directly, use xml-schema.hxx instead. This
// file is included into generated xml-schema.hxx so we do not need to
// guard against multiple inclusions.
//

#include <xsd/cxx/xml/string.hxx> // xml::transcode
#include <xsd/cxx/tree/text.hxx>  // text_content

// Parsing.
//
namespace xsd
{
  namespace cxx
  {
    namespace tree
    {
      template<>
      struct traits<double, char, schema_type::double_>
      {
        static double
        create (const xercesc::DOMElement& e, flags f, type* c)
        {
          return create (text_content<char> (e), 0, f, c);
        }

        static double
        create (const xercesc::DOMAttr& a, flags f, type* c)
        {
          return create (xml::transcode<char> (a.getValue ()), 0, f, c);
        }

        static double
        create (const std::string& s,
                const xercesc::DOMElement*,
                flags,
                type*);
      };
    }
  }
}

// Serialization.
//
namespace XERCES_CPP_NAMESPACE
{
  void
  operator<< (xercesc::DOMElement& e, const xml_schema::as_double& d);

  void
  operator<< (xercesc::DOMAttr& a, const xml_schema::as_double& d);
}

namespace xsd
{
  namespace cxx
  {
    namespace tree
    {
      void
      operator<< (xml_schema::list_stream& ls,
                  const xml_schema::as_double& d);
    }
  }
}
