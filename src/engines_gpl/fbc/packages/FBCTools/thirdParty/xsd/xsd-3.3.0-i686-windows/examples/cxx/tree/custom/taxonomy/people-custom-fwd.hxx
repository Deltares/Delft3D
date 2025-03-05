// file      : examples/cxx/tree/custom/taxonomy/people-custom-fwd.hxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

// Do not include this file directly, use people-fwd.hxx instead. This
// file is included into generated people-fwd.hxx so we do not need to
// guard against multiple inclusions.
//

namespace people
{
  template <typename base>
  class person_impl;

  template <typename base>
  class superman_impl;

  template <typename base>
  class batman_impl;
}
