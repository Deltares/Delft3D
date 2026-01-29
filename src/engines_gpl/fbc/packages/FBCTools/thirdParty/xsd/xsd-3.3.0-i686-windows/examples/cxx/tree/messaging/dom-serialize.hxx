// file      : examples/cxx/tree/messaging/dom-serialize.hxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#ifndef DOM_SERIALIZE
#define DOM_SERIALIZE

#include <string>
#include <iosfwd>

#include <xercesc/dom/DOMDocument.hpp>

// Serialize a DOM document to XML which is written to the standard
// output stream.
//
void
serialize (std::ostream& os,
           const xercesc::DOMDocument& doc,
           const std::string& encoding = "UTF-8");

#endif // DOM_SERIALIZE
