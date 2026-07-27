// file      : examples/cxx/tree/custom/taxonomy/driver.cxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#include <memory>   // std::auto_ptr
#include <iostream>

#include "people.hxx"

using std::cerr;
using std::endl;

int
main (int argc, char* argv[])
{
  if (argc != 2)
  {
    cerr << "usage: " << argv[0] << " people.xml" << endl;
    return 1;
  }

  try
  {
    using namespace people;

    std::auto_ptr<catalog> c (catalog_ (argv[1]));

    for (catalog::person_const_iterator i (c->person ().begin ());
         i != c->person ().end (); ++i)
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
