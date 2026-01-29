// file      : examples/cxx/parser/polyroot/supermen-pimpl.hxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#ifndef SUPERMEN_PIMPL_HXX
#define SUPERMEN_PIMPL_HXX

#include "supermen-pskel.hxx"

class person_pimpl: public virtual person_pskel
{
public:
  virtual void
  pre ();

  virtual void
  name (const std::string&);

  virtual void
  post_person ();
};

class superman_pimpl: public virtual superman_pskel,
                      public person_pimpl
{
public:
  virtual void
  pre ();

  virtual void
  can_fly (bool);

  // By default, post_superman() calls post_person(). In case of
  // polymorphic parsing we want the opposite: post_person() calls
  // post_superman().
  //
  virtual void
  post_person ();

  virtual void
  post_superman ();
};

class batman_pimpl: public virtual batman_pskel,
                    public superman_pimpl
{
public:
  virtual void
  pre ();

  virtual void
  wing_span (unsigned int);

  // By default, post_batman() calls post_superman(). In case of
  // polymorphic parsing we want the opposite: post_superman()
  // calls post_batman().
  //
  virtual void
  post_superman ();

  virtual void
  post_batman ();
};

#endif // SUPERMEN_PIMPL_HXX
