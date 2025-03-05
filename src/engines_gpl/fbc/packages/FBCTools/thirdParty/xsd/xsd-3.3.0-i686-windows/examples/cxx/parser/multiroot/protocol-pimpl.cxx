// file      : examples/cxx/parser/multiroot/protocol-pimpl.cxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#include "protocol-pimpl.hxx"

namespace protocol
{
  // request_pimpl
  //
  void request_pimpl::
  account (unsigned int account)
  {
    account_ = account;
  }

  request* request_pimpl::
  post_request ()
  {
    // This parser is never used directly.
    //
    return 0;
  }

  // balance_pimpl
  //
  balance* balance_pimpl::
  post_balance ()
  {
    return new balance (account_);
  }

  // withdraw_pimpl
  //
  void withdraw_pimpl::
  amount (unsigned int amount)
  {
    amount_ = amount;
  }

  withdraw* withdraw_pimpl::
  post_withdraw ()
  {
    return new withdraw (account_, amount_);
  }
}

