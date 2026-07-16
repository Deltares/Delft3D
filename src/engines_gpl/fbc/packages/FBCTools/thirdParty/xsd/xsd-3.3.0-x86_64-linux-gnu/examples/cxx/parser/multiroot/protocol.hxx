// file      : examples/cxx/parser/multiroot/protocol.hxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#ifndef PROTOCOL_HXX
#define PROTOCOL_HXX

namespace protocol
{
  class request
  {
  public:
    virtual
    ~request ()
    {
    }

    unsigned int
    account () const
    {
      return account_;
    }

  protected:
    request (unsigned int account)
        : account_ (account)
    {
    }

  private:
    unsigned int account_;
  };

  class balance: public request
  {
  public:
    balance (unsigned int account)
        : request (account)
    {
    }
  };

  class withdraw: public request
  {
  public:
    withdraw (unsigned int account, unsigned int amount)
        : request (account), amount_ (amount)
    {
    }

    unsigned int
    amount () const
    {
      return amount_;
    }

  private:
    unsigned int amount_;
  };
}

#endif // PROTOCOL_HXX
