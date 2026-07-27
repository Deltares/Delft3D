// file      : examples/cxx/parser/mixin/types.hxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#ifndef TYPES_HXX
#define TYPES_HXX

struct base
{
  bool
  a () const
  {
    return a_;
  }

  void
  a (bool v)
  {
    a_ = v;
  }

private:
  bool a_;
};

struct derived: base
{
  int
  b () const
  {
    return b_;
  }

  void
  b (int v)
  {
    b_ = v;
  }

private:
  int b_;
};

#endif // TYPES_HXX
