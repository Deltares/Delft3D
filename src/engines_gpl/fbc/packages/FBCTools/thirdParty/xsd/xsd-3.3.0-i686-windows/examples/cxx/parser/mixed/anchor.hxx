// file      : examples/cxx/parser/mixed/anchor.hxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#ifndef ANCHOR_HXX
#define ANCHOR_HXX

#include <string>

struct anchor
{
  anchor (const std::string& text, const std::string& uri)
      : uri_ (uri), text_ (text)
  {
  }

  const std::string&
  text () const
  {
    return text_;
  }

  const std::string&
  uri () const
  {
    return uri_;
  }

private:
  std::string uri_;
  std::string text_;
};

#endif // ANCHOR_HXX
