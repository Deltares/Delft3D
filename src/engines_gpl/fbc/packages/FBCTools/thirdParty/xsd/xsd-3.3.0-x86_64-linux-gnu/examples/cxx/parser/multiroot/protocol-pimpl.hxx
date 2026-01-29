// file      : examples/cxx/parser/multiroot/protocol-pimpl.hxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#ifndef PROTOCOL_PIMPL_HXX
#define PROTOCOL_PIMPL_HXX

#include "protocol.hxx"
#include "protocol-pskel.hxx"

namespace protocol
{
  class request_pimpl: public virtual request_pskel
  {
  public:
    virtual void
    account (unsigned int);

    virtual request*
    post_request ();

  protected:
    unsigned int account_;
  };

  class balance_pimpl: public virtual balance_pskel,
                       public request_pimpl
  {
  public:
    virtual balance*
    post_balance ();
  };

  class withdraw_pimpl: public virtual withdraw_pskel,
                        public request_pimpl
  {
  public:
    virtual void
    amount (unsigned int);

    virtual withdraw*
    post_withdraw ();

  private:
    unsigned int amount_;
  };
}

#endif // PROTOCOL_PIMPL_HXX
