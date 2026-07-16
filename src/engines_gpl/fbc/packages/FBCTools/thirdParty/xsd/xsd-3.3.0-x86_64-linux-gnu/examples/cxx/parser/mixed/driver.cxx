// file      : examples/cxx/parser/mixed/driver.cxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#include <string>
#include <vector>
#include <iostream>

#include "anchor.hxx"
#include "text-pskel.hxx"

using namespace std;

struct anchor_pimpl: anchor_pskel, xml_schema::string_pimpl
{
  virtual void
  href (const std::string& uri)
  {
    uri_ = uri;
  }

  virtual anchor
  post_anchor ()
  {
    return anchor (post_string (), uri_);
  }

private:
  std::string uri_;
};


struct text_pimpl: text_pskel
{
  virtual void
  a (const anchor& a)
  {
    cout << a.text () << "[" << anchors_.size () << "]";
    anchors_.push_back (a);
  }

  virtual void
  _any_characters (const xml_schema::ro_string& s)
  {
    cout << s;
  }

  virtual void
  post_text ()
  {
    for (anchors::const_iterator i (anchors_.begin ());
         i != anchors_.end ();
         ++i)
    {
      cout << "[" << i - anchors_.begin () << "] " << i->uri () << endl;
    }
  }

private:
  typedef vector<anchor> anchors;
  anchors anchors_;
};


int
main (int argc, char* argv[])
{
  if (argc != 2)
  {
    cerr << "usage: " << argv[0] << " text.xml" << endl;
    return 1;
  }

  try
  {
    // Construct the parser.
    //
    xml_schema::string_pimpl string_p;
    anchor_pimpl anchor_p;
    text_pimpl text_p;

    anchor_p.href_parser (string_p);
    text_p.a_parser (anchor_p);

    xml_schema::document doc_p (text_p, "text");

    text_p.pre ();
    doc_p.parse (argv[1]);
    text_p.post_text ();
  }
  catch (const xml_schema::exception& e)
  {
    cerr << e << endl;
    return 1;
  }
  catch (const std::ios_base::failure&)
  {
    cerr << argv[1] << ": unable to open or read failure" << endl;
    return 1;
  }
}
