// file      : examples/cxx/parser/mixin/driver.cxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#include <memory>
#include <iostream>

#include "types.hxx"
#include "schema-pskel.hxx"

using namespace std;

struct base_pimpl: virtual base_pskel
{
  virtual void
  pre ()
  {
    base_.reset (new ::base);
  }

  virtual void
  a (bool v)
  {
    base_->a (v);
  }

  virtual base*
  post_base ()
  {
    return base_.release ();
  }

protected:
  auto_ptr<base> base_;
};

// Implement derived parser by mixing-in base's implementation.
//
struct derived_pimpl: derived_pskel, base_pimpl
{
  virtual void
  pre ()
  {
    // Override base's pre() with the new implementation that
    // instantiates derived instead of base.
    //
    base_.reset (new ::derived);
  }

  virtual void
  b (int v)
  {
    // We could also store a pointer to derived in derived_impl to
    // avoid casting.
    //
    static_cast< ::derived* > (base_.get ())->b (v);
  }

  virtual derived*
  post_derived ()
  {
    return static_cast<derived*> (base_.release ());
  }
};

int
main (int argc, char* argv[])
{
  if (argc != 2)
  {
    cerr << "usage: " << argv[0] << " instance.xml" << endl;
    return 1;
  }

  try
  {
    // Construct the parser.
    //
    xml_schema::boolean_pimpl bool_p;
    xml_schema::int_pimpl int_p;
    derived_pimpl derived_p;

    derived_p.parsers (bool_p, int_p);

    xml_schema::document doc_p (derived_p, "root");

    derived_p.pre ();
    doc_p.parse (argv[1]);
    auto_ptr<derived> d (derived_p.post_derived ());

    cerr << "a: " << boolalpha << d->a () << endl;
    cerr << "b: " << d->b () << endl;
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
