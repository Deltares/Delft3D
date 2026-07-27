// file      : examples/cxx/tree/embedded/grammar-input-stream.cxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#include <cassert>
#include "grammar-input-stream.hxx"

grammar_input_stream::
grammar_input_stream (const XMLByte* data, std::size_t size)
      : data_ (data),
        size_ (size),
        pos_ (0),
        vpos_ (0),
        cseq_ (0),
        add_zero_ (false)
{
}

#if _XERCES_VERSION >= 30000
XMLFilePos grammar_input_stream::
curPos () const
{
  return static_cast<XMLFilePos> (vpos_);
}
#else
unsigned int grammar_input_stream::
curPos () const
{
  return static_cast<unsigned int> (vpos_);
}
#endif

#if _XERCES_VERSION >= 30000
XMLSize_t grammar_input_stream::
readBytes (XMLByte* const buf, const XMLSize_t size)
#else
unsigned int grammar_input_stream::
readBytes (XMLByte* const buf, const unsigned int size)
#endif
{
  std::size_t i (0);

  // Add a zero from the alternating sequence if it didn't
  // fit on the previous read.
  //
  if (add_zero_)
  {
    buf[i++] = 0;
    add_zero_ = false;
  }

  // If have an unfinished sequential sequence, output it now.
  //
  if (cseq_ != 0 && !alt_)
  {
    for (; cseq_ != 0 && i < size; --cseq_)
      buf[i++] = 0;
  }

  for (; i < size && pos_ < size_;)
  {
    XMLByte b = buf[i++] = data_[pos_++];

    // See if we are in a compression sequence.
    //
    if (cseq_ != 0)
    {
      if (i < size)
        buf[i++] = 0;
      else
        add_zero_ = true; // Add it on the next read.

      cseq_--;
      continue;
    }

    // If we are not in a compression sequence and this byte is
    // not zero then we are done.
    //
    if (b != 0)
      continue;

    // We have a zero.
    //
    assert (pos_ < size_); // There has to be another byte.
    unsigned char v (static_cast<unsigned char> (data_[pos_++]));
    alt_ = (v & 128) != 0;
    cseq_ = v & 127;

    // If it is a sequential sequence, output as many zeros as
    // we can.
    //
    if (!alt_)
    {
      for (; cseq_ != 0 && i < size; --cseq_)
        buf[i++] = 0;
    }
  }

  vpos_ += i;

#if _XERCES_VERSION >= 30000
  return static_cast<XMLSize_t> (i);
#else
  return static_cast<unsigned int> (i);
#endif
}

#if _XERCES_VERSION >= 30000
const XMLCh* grammar_input_stream::
getContentType () const
{
  return 0;
}
#endif
