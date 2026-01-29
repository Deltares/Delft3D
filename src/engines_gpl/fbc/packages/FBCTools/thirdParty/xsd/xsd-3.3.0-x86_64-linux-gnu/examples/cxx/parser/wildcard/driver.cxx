// file      : examples/cxx/parser/wildcard/driver.cxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#include <string>
#include <memory>
#include <iostream>

#include "email-pskel.hxx"

using namespace std;
using xml_schema::ro_string;

class binary_pimpl: public email::binary_pskel,
                    public xml_schema::base64_binary_pimpl
{
public:
  virtual void
  name (const string& n)
  {
    cerr << "binary: " << n << endl;
  }

  virtual void
  mime (const string& t)
  {
    cerr << "type:   " << t << endl
         << endl;
  }

  virtual void
  post_binary ()
  {
    auto_ptr<xml_schema::buffer> buf (post_base64_binary ());

    cerr << "size:   " << buf->size () << endl
         << endl;
  }
};

class envelope_pimpl: public email::envelope_pskel
{
public:
  envelope_pimpl (xml_schema::unsigned_int_pskel& uint_p,
                  xml_schema::string_pskel& string_p,
                  email::binary_pskel& binary_p)
      : depth_ (0), cur_ (0),
        uint_p_ (uint_p), string_p_ (string_p), binary_p_ (binary_p)
  {
  }

  virtual void
  to (const string& addr)
  {
    cerr << "To:        " << addr << endl;
  }

  virtual void
  from (const string& addr)
  {
    cerr << "From:      " << addr << endl;
  }

  virtual void
  subject (const string& s)
  {
    cerr << "Subject:   " << s << endl;
  }

  // Wildcard handling. All wildcard events are routed to these
  // four functions. It is our job to dispatch them to the right
  // parsers.
  //
  virtual void
  _start_any_element (const ro_string& ns,
                      const ro_string& name,
                      const ro_string* type)
  {
    if (depth_++ > 0)
    {
      // Nested wildcard element.
      //
      if (cur_)
        cur_->_start_element (ns, name, type);
    }
    else
    {
      // Top-level element matched by the any wildcard.
      //
      if (ns == "http://www.codesynthesis.com/email")
      {
        if (name == "text")
        {
          cur_ = &string_p_;
          string_p_.pre ();
          string_p_._pre_impl ();
        }
        else if (name == "binary")
        {
          cur_ = &binary_p_;
          binary_p_.pre ();
          binary_p_._pre_impl ();
        }
      }

      if (cur_ == 0)
      {
        cerr << "Unknown wildcard content: " << ns << "#" << name << endl;
      }
    }
  }

  virtual void
  _end_any_element (const ro_string& ns, const ro_string& name)
  {
    if (--depth_ > 0)
    {
      if (cur_)
        cur_->_end_element (ns, name);
    }
    else
    {
      if (ns == "http://www.codesynthesis.com/email")
      {
        if (name == "text")
        {
          string_p_._post_impl ();
          string text (string_p_.post_string ());

          cerr << text << endl
               << endl;
        }
        else if (name == "binary")
        {
          binary_p_._post_impl ();
          binary_p_.post_binary ();
        }
      }

      cur_ = 0;
    }
  }

  virtual void
  _any_attribute (const ro_string& ns,
                  const ro_string& name,
                  const ro_string& value)
  {
    if (depth_ > 0)
    {
      // Nested wildcard attribute.
      //
      if (cur_)
        cur_->_attribute (ns, name, value);
    }
    else
    {
      // Top-level attribute matched by the anyAttribute wildcard.
      //
      if (ns == "http://www.codesynthesis.com/email" && name == "thread-id")
      {
        uint_p_.pre ();
        uint_p_._pre_impl ();
        uint_p_._characters (value);
        uint_p_._post_impl ();
        unsigned int tid (uint_p_.post_unsigned_int ());

        cerr << "Thread-id: " << tid << endl;
      }
    }
  }

  virtual void
  _any_characters (const ro_string& s)
  {
    if (depth_ > 0)
    {
      if (cur_)
        cur_->_characters (s);
    }
  }

private:
  size_t depth_;
  xml_schema::parser_base* cur_;

  // Parsers for the unsigned int, string and binary types.
  //
private:
  xml_schema::unsigned_int_pskel& uint_p_;
  xml_schema::string_pskel& string_p_;
  email::binary_pskel& binary_p_;
};


int
main (int argc, char* argv[])
{
  if (argc != 2)
  {
    cerr << "usage: " << argv[0] << " email.xml" << endl;
    return 1;
  }

  try
  {
    // Construct the parser.
    //
    xml_schema::unsigned_int_pimpl unsigned_int_p;
    xml_schema::string_pimpl string_p;
    binary_pimpl binary_p;
    envelope_pimpl envelope_p (unsigned_int_p, string_p, binary_p);

    binary_p.parsers (string_p,  // name
                      string_p); // mime

    envelope_p.parsers (string_p,  // to
                        string_p,  // from
                        string_p); // subject

    // Parse the XML instance document.
    //
    xml_schema::document doc_p (envelope_p,
                                "http://www.codesynthesis.com/email",
                                "message");
    envelope_p.pre ();
    doc_p.parse (argv[1]);
    envelope_p.post_envelope ();
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
