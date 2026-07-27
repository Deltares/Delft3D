// file      : examples/cxx/tree/polymorphism/driver.cxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#include <memory>   // std::auto_ptr
#include <iostream>

#include "supermen.hxx"

using std::cerr;
using std::endl;
using std::auto_ptr;

int
main (int argc, char* argv[])
{
  if (argc != 2)
  {
    cerr << "usage: " << argv[0] << " supermen.xml" << endl;
    return 1;
  }

  try
  {
    auto_ptr<supermen> sm (supermen_ (argv[1]));

    supermen copy (*sm); // Dynamic types are preserved in copies.

    // Print what we've got.
    //
    for (supermen::person_const_iterator i (copy.person ().begin ());
         i != copy.person ().end ();
         ++i)
    {
      cerr << i->name ();

      if (const superman* s = dynamic_cast<const superman*> (&*i))
      {
        if (s->can_fly ())
          cerr << ", flying superman";
        else
          cerr << ", superman";
      }

      cerr << endl;
    }

    // Serialize back to XML.
    //
    xml_schema::namespace_infomap map;
    map[""].schema = "supermen.xsd";

    supermen_ (std::cout, copy, map);
  }
  catch (const xml_schema::exception& e)
  {
    cerr << e << endl;
    return 1;
  }
}
