// file      : examples/cxx/parser/library/library-pimpl.cxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#include "library-pimpl.hxx"

namespace library
{
  // isbn_impl
  //
  isbn isbn_pimpl::
  post_isbn ()
  {
    return post_unsigned_int ();
  }

  // title_pimpl
  //
  void title_pimpl::
  _pre ()
  {
    title_.lang ("");
  }

  void title_pimpl::
  lang (const std::string& lang)
  {
    title_.lang (lang);
  }

  title title_pimpl::
  post_title ()
  {
    title_.assign (post_string ());
    return title_;
  }

  // genre_pimpl
  //
  genre genre_pimpl::
  post_genre ()
  {
    genre r (romance);
    std::string v (post_string ());

    if (v == "romance") r = romance; else
    if (v == "fiction") r = fiction; else
    if (v == "horror") r = horror; else
    if (v == "history") r = history; else
    if (v == "philosophy") r = philosophy;

    return r;
  }

  // person_pimpl
  //
  void person_pimpl::
  _pre ()
  {
    person_.died ("");
  }

  void person_pimpl::
  name (const std::string& name)
  {
    person_.name (name);
  }

  void person_pimpl::
  born (const std::string& born)
  {
    person_.born (born);
  }

  void person_pimpl::
  died (const std::string& died)
  {
    person_.died (died);
  }

  person person_pimpl::
  post_person ()
  {
    return person_;
  }

  // author_pimpl
  //
  void author_pimpl::
  _pre ()
  {
    person_pimpl::_pre ();
    author_.recommends ("");
  }

  void author_pimpl::
  recommends (const std::string& recommends)
  {
    author_.recommends (recommends);
  }

  author author_pimpl::
  post_author ()
  {
    person p (post_person ());

    author_.name (p.name ());
    author_.born (p.born ());
    author_.died (p.died ());

    return author_;
  }

  // book_pimpl
  //
  void book_pimpl::
  _pre ()
  {
    book_.author ().clear ();
  }

  void book_pimpl::
  isbn (library::isbn isbn)
  {
    book_.isbn (isbn);
  }

  void book_pimpl::
  title (const library::title& title)
  {
    book_.title (title);
  }

  void book_pimpl::
  genre (library::genre genre)
  {
    book_.genre (genre);
  }

  void book_pimpl::
  author (const library::author& author)
  {
    book_.author ().push_back (author);
  }

  void book_pimpl::
  available (bool available)
  {
    book_.available (available);
  }

  void book_pimpl::
  id (const std::string& id)
  {
    book_.id (id);
  }

  book book_pimpl::
  post_book ()
  {
    return book_;
  }

  // catalog_pimpl
  //
  void catalog_pimpl::
  _pre ()
  {
    catalog_.clear ();
  }

  void catalog_pimpl::
  book (const library::book& book)
  {
    catalog_.push_back (book);
  }

  catalog catalog_pimpl::
  post_catalog ()
  {
    return catalog_;
  }
}

