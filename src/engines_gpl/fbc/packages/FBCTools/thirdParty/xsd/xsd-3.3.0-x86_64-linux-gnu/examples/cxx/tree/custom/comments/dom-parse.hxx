// file      : examples/cxx/tree/custom/comments/dom-parse.hxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#ifndef DOM_PARSE
#define DOM_PARSE

#include <string>
#include <iosfwd>

#include <xercesc/dom/DOMDocument.hpp>
#include <xsd/cxx/xml/dom/auto-ptr.hxx>

// Parse an XML document from the standard input stream with an
// optional resource id. Resource id is used in diagnostics as
// well as to locate schemas referenced from inside the document.
//
xsd::cxx::xml::dom::auto_ptr<xercesc::DOMDocument>
parse (std::istream& is,
       const std::string& id,
       bool validate);

#endif // DOM_PARSE
