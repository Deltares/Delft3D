// file      : examples/cxx/parser/library/library.hxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#ifndef LIBRARY_HXX
#define LIBRARY_HXX

#include <string>
#include <vector>

namespace library
{
  //
  //
  typedef unsigned int isbn;


  //
  //
  struct title: std::string
  {
    // lang
    //
    const std::string&
    lang () const
    {
      return lang_;
    }

    void
    lang (const std::string& lang)
    {
      lang_ = lang;
    }

  private:
    std::string lang_;
  };


  //
  //
  enum genre
  {
    romance,
    fiction,
    horror,
    history,
    philosophy
  };


  //
  //
  struct person
  {
    // name
    //
    const std::string&
    name () const
    {
      return name_;
    }

    void
    name (const std::string& name)
    {
      name_ = name;
    }

    // born
    //
    const std::string&
    born () const
    {
      return born_;
    }

    void
    born (const std::string& born)
    {
      born_ = born;
    }


    // died
    //
    const std::string&
    died () const
    {
      return died_;
    }

    void
    died (const std::string& died)
    {
      died_ = died;
    }

  private:
    std::string name_;
    std::string born_;
    std::string died_;
  };


  //
  //
  struct author: person
  {
    // recommends
    //
    const std::string&
    recommends () const
    {
      return recommends_;
    }

    void
    recommends (const std::string& recommends)
    {
      recommends_ = recommends;
    }

  private:
    std::string recommends_;
  };


  //
  //
  struct book
  {
    // isbn
    //
    library::isbn
    isbn () const
    {
      return isbn_;
    }

    void
    isbn (const library::isbn& isbn)
    {
      isbn_ = isbn;
    }


    // title
    //
    library::title
    title () const
    {
      return title_;
    }

    void
    title (const library::title& title)
    {
      title_ = title;
    }


    // genre
    //
    library::genre
    genre () const
    {
      return genre_;
    }

    void
    genre (const library::genre& genre)
    {
      genre_ = genre;
    }


    // author
    //
    typedef std::vector<library::author> authors;

    const authors&
    author () const
    {
      return author_;
    }

    authors&
    author ()
    {
      return author_;
    }


    // available
    //
    bool
    available () const
    {
      return available_;
    }

    void
    available (bool available)
    {
      available_ = available;
    }


    // id
    //
    const std::string&
    id () const
    {
      return id_;
    }

    void
    id (const std::string& id)
    {
      id_ = id;
    }

  private:
    library::isbn  isbn_;
    library::title title_;
    library::genre genre_;

    authors author_;

    bool available_;
    std::string id_;
  };


  //
  //
  typedef std::vector<book> catalog;
}

#endif // LIBRARY_HXX
