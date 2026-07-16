// file      : examples/cxx/tree/custom/comments/xml-schema-custom.cxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

// Include xml-schema.hxx instead of xml-schema-custom.hxx here.
//
#include "xml-schema.hxx"

#include <xercesc/dom/DOMComment.hpp>
#include <xercesc/dom/DOMDocument.hpp>

#include <xsd/cxx/xml/string.hxx> // xml::transcode, xml::string

namespace xml = xsd::cxx::xml;

namespace xml_schema
{
  type::
  type ()
      : type_base ()
  {
  }

  type::
  type (const xercesc::DOMElement& e, flags f, container* c)
      : type_base (e, f, c)
  {
    using namespace xercesc;

    // Here we are only handling a comment that is the first
    // node in the element's content.
    //
    const DOMNode* n (e.getFirstChild ());

    if (n != 0 && n->getNodeType () == DOMNode::COMMENT_NODE)
    {
      const DOMComment* c (static_cast<const DOMComment*> (n));
      comment_ = xml::transcode<char> (c->getData ());
    }
  }

  type::
  type (const xercesc::DOMAttr& a, flags f, container* c)
      : type_base (a, f, c)
  {
    // No comments for attributes.
    //
  }

  type::
  type (const std::string& s, const xercesc::DOMElement* e,
        flags f, container* c)
      : type_base (s, e, f, c)
  {
    // No comments for list items.
    //
  }

  type::
  type (const type& x, flags f, container* c)
      : type_base (x, f, c), comment_ (x.comment_)
  {
  }

  type* type::
  _clone (flags f, container* c) const
  {
    return new type (*this, f, c);
  }

  // Serialization operators.
  //
  void
  operator<< (xercesc::DOMElement& e, const type& x)
  {
    // Call our base first.
    //
    const type_base& b (x);
    e << b;

    // Add the comment if any.
    //
    const std::string s (x.comment ());

    if (!s.empty ())
    {
      using namespace xercesc;

      DOMDocument* doc (e.getOwnerDocument ());
      DOMComment* c (doc->createComment (xml::string (s).c_str ()));
      e.appendChild (c);
    }
  }

  void
  operator<< (xercesc::DOMAttr& a, const type& x)
  {
    // Call our base first.
    //
    const type_base& b (x);
    a << b;

    // No comments for attributes.
    //
  }

  void
  operator<< (xml_schema::list_stream& ls, const type& x)
  {
    // Call our base first.
    //
    const type_base& b (x);
    ls << b;

    // No comments for list items.
    //
  }
}
