// file      : examples/cxx/tree/custom/double/driver.cxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#include <iostream>

#include "order.hxx"

using std::cerr;
using std::endl;

int
main ()
{
  try
  {
    // Order one Airbus A380.
    //
    order o;
    o.item ().push_back (item ("Airbus A380", 317000000.90));


    // Serialize.
    //
    order_ (std::cout, o);
  }
  catch (const xml_schema::exception& e)
  {
    cerr << e << endl;
    return 1;
  }
}
