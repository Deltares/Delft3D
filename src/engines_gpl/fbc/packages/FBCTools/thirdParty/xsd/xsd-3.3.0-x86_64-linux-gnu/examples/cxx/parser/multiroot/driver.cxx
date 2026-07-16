// file      : examples/cxx/parser/multiroot/driver.cxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#include <memory>   // std::auto_ptr
#include <iostream>

#include "protocol.hxx"
#include "protocol-pimpl.hxx"

using std::cerr;
using std::endl;
using xml_schema::ro_string;

namespace protocol
{
  // Customize the xml_schema::document object to handle our protocol
  // vocabulary with multiple root elements.
  //
  class document: public xml_schema::document
  {
  public:
    document (balance_pskel& balance_p, withdraw_pskel& withdraw_p)
        : balance_p_ (balance_p), withdraw_p_ (withdraw_p)
    {
    }

    request*
    result ()
    {
      return result_.release ();
    }

  protected:
    // This function is called to obtain the root element type parser.
    // If the returned pointed is 0 then the whole document content
    // is ignored. The type argument is used to handle polymorphic
    // XML documents and is not used in this example (see the polyroot
    // example for more information on this argument).
    //
    virtual xml_schema::parser_base*
    start_root_element (const ro_string& ns,
                        const ro_string& name,
                        const ro_string* /* type */)
    {
      if (ns == "http://www.codesynthesis.com/protocol")
      {
        if (name == "balance")
        {
          balance_p_.pre ();

          return &balance_p_;
        }
        else if (name == "withdraw")
        {
          balance_p_.pre ();

          return &withdraw_p_;
        }
      }

      cerr << "ignoring unknown request: " << ns << "#" << name << endl;

      return 0;
    }

    // This function is called to indicate the completion of document
    // parsing. The parser argument contains the pointer returned by
    // start_root_element.
    //
    virtual void
    end_root_element (const ro_string& /* ns */,
                      const ro_string& /* name */,
                      xml_schema::parser_base* parser)
    {
      // We could have handled the result directly in this function
      // instead of storing it in the result_ variable.
      //
      if (parser == &balance_p_)
      {
        result_.reset (balance_p_.post_balance ());
      }
      else if (parser == &withdraw_p_)
      {
        result_.reset (withdraw_p_.post_withdraw ());
      }
      else
        result_.reset (0);
    }


  private:
    std::auto_ptr<request> result_;

    balance_pskel& balance_p_;
    withdraw_pskel& withdraw_p_;
  };
}


int
main (int argc, char* argv[])
{
  if (argc != 2)
  {
    cerr << "usage: " << argv[0] << " request.xml" << endl;
    return 1;
  }

  try
  {
    using namespace protocol;

    // Construct the parser.
    //
    xml_schema::unsigned_int_pimpl unsigned_int_p;

    balance_pimpl balance_p;
    withdraw_pimpl withdraw_p;

    balance_p.parsers (unsigned_int_p); // account

    withdraw_p.parsers (unsigned_int_p,  // account
                        unsigned_int_p); // amount

    // Parse the XML instance document.
    //
    document doc_p (balance_p, withdraw_p);

    // pre() and post() will be called as part of the start_root_element()
    // and end_root_element() calls.
    //
    doc_p.parse (argv[1]);
    std::auto_ptr<request> r (doc_p.result ());

    // Let's print what we've got.
    //
    if (balance* b = dynamic_cast<balance*> (r.get ()))
    {
      cerr << "balance request for acc# " << b->account () << endl;
    }
    else if (withdraw* w = dynamic_cast<withdraw*> (r.get ()))
    {
      cerr << "withdrawal request for acc# " << w->account () << ", "
           << "amount: " << w->amount () << endl;
    }
    else
    {
      cerr << "unknown request" << endl;
    }
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
