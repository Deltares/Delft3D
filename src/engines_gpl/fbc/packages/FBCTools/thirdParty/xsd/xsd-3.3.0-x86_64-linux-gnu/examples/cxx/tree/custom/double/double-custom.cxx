// file      : examples/cxx/tree/custom/double/double-custom.cxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

// Include xml-schema.hxx instead of double-custom.hxx here.
//
#include "xml-schema.hxx"

#include <limits>
#include <locale>
#include <sstream>

#include <xsd/cxx/ro-string.hxx>
#include <xsd/cxx/zc-istream.hxx>

using namespace std;

// Parsing.
//
namespace xsd
{
  namespace cxx
  {
    namespace tree
    {
      double traits<double, char, schema_type::double_>::
      create (const std::string& s,
              const xercesc::DOMElement*,
              flags,
              type*)
      {
        // This type cannot have whitespaces in its values. As result we
        // don't need to waste time collapsing whitespaces. All we need to
        // do is trim the string representation which can be done without
        // copying.
        //
        ro_string<char> tmp (s);
        trim (tmp);

        zc_istream<char> is (tmp);
        is.imbue (locale::classic ());

        double t;
        is >> t;

        return t;
      }
    }
  }
}

// Serialization.
//
namespace XERCES_CPP_NAMESPACE
{
  void
  operator<< (xercesc::DOMElement& e, const xml_schema::as_double& d)
  {
    ostringstream os;
    os.imbue (locale::classic ());

    os.precision (2);
    os << fixed << d.x;

    e << os.str ();
  }

  void
  operator<< (xercesc::DOMAttr& a, const xml_schema::as_double& d)
  {
    ostringstream os;
    os.imbue (locale::classic ());

    os.precision (2);
    os << fixed << d.x;

    a << os.str ();
  }
}

namespace xsd
{
  namespace cxx
  {
    namespace tree
    {
      void
      operator<< (xml_schema::list_stream& ls,
                  const xml_schema::as_double& d)
      {
        ls.os_.imbue (locale::classic ());
        ls.os_.precision (2);
        ls.os_ << fixed << d.x;
      }
    }
  }
}
