// file      : examples/cxx/tree/custom/contacts/driver.cxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#include <memory>   // std::auto_ptr
#include <iostream>

#include "contacts.hxx"

using std::cerr;
using std::endl;

int
main (int argc, char* argv[])
{
  if (argc != 2)
  {
    cerr << "usage: " << argv[0] << " contacts.xml" << endl;
    return 1;
  }

  try
  {
    using namespace contacts;

    std::auto_ptr<catalog> c (catalog_ (argv[1]));

    for (catalog::contact_const_iterator i (c->contact ().begin ());
         i != c->contact ().end (); ++i)
    {
      i->print (cerr);
    }
  }
  catch (const xml_schema::exception& e)
  {
    cerr << e << endl;
    return 1;
  }
}
