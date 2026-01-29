// file      : examples/cxx/tree/library/driver.cxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#include <memory>   // std::auto_ptr
#include <iostream>

#include "library.hxx"

using std::cerr;
using std::endl;

int
main (int argc, char* argv[])
{
  if (argc != 2)
  {
    cerr << "usage: " << argv[0] << " library.xml" << endl;
    return 1;
  }

  try
  {
    using namespace library;

    // Read in the XML file and obtain its object model.
    //
    std::auto_ptr<catalog> c (catalog_ (argv[1]));


    // Let's print what we've got.
    //
    for (catalog::book_const_iterator bi (c->book ().begin ());
         bi != c->book ().end ();
         ++bi)
    {
      cerr << endl
           << "ID           : " << bi->id () << endl
           << "ISBN         : " << bi->isbn () << endl
           << "Title        : " << bi->title () << endl
           << "Genre        : " << bi->genre () << endl;

      for (book::author_const_iterator ai (bi->author ().begin ());
           ai != bi->author ().end ();
           ++ai)
      {
        cerr << "Author       : " << ai->name () << endl;
        cerr << "  Born       : " << ai->born () << endl;

        if (ai->died ())
          cerr << "  Died       : " << *ai->died () << endl;

        if (ai->recommends ())
          cerr << "  Recommends : " << (*ai->recommends ())->title () << endl;
      }

      cerr  << "Available    : " << std::boolalpha << bi->available () << endl;
    }


    // Now we are going to modify the object model and serialize it
    // back to XML.
    //

    catalog::book_sequence& books (c->book ());


    // Get rid of all unavailable books.
    //
    for (catalog::book_iterator bi (books.begin ()); bi != books.end ();)
    {
      if (!bi->available ())
        bi = books.erase (bi);
      else
        ++bi;
    }


    // Insert a new book.
    //
    book b (679776443,         // ISBN
            "Dead Souls",      // Title
            genre::philosophy, // Genre
            "DS");             // ID

    b.author ().push_back (author ("Nikolai Gogol",
                                   xml_schema::date (1809, 3, 31)));

    books.insert (books.begin (), b);


    // Because we removed all unavailable books, some IDREFs might be
    // broken. Let's fix this.
    //
    for (catalog::book_iterator bi (books.begin ()); bi != books.end (); ++bi)
    {
      for (book::author_iterator ai (bi->author ().begin ());
           ai != bi->author ().end ();
           ++ai)
      {
        author::recommends_optional& c (ai->recommends ());

        if (c.present ())
        {
          author::recommends_type& ref (c.get ());

          if (!ref)
            c.reset ();
        }
      }
    }


    // Prepare namespace mapping and schema location information.
    //
    xml_schema::namespace_infomap map;

    map["lib"].name = "http://www.codesynthesis.com/library";
    map["lib"].schema = "library.xsd";


    // Write it out.
    //
    catalog_ (std::cout, *c, map);
  }
  catch (const xml_schema::exception& e)
  {
    cerr << e << endl;
    return 1;
  }
}
