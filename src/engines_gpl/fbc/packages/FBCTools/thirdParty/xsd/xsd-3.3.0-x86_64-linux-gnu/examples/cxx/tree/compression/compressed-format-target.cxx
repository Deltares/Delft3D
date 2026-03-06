// file      : examples/cxx/tree/compression/compressed-format-target.cxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#include <ostream>
#include <cstring> // std::memcpy

#include "compressed-format-target.hxx"

using namespace std;

//
// compression_failure
//

const char* compression_failure::
what () const throw ()
{
  return "compression failure";
}

//
// compressed_format_target
//

compressed_format_target::
compressed_format_target (ostream& os, compression_type t)
    : os_ (os), closed_ (false), n_ (0)
  {
    zs_.zalloc = Z_NULL;
    zs_.zfree  = Z_NULL;
    zs_.opaque = Z_NULL;

    int window = 0;

    switch (t)
    {
    case raw:
      {
        window = -15;
        break;
      }
    case zlib:
      {
        window = 15;
        break;
      }
    case gzip:
      {
        window = 16 + 15;
        break;
      }
    }

    int r (deflateInit2 (&zs_,
                         Z_DEFAULT_COMPRESSION,
                         Z_DEFLATED,
                         window,
                         8,
                         Z_DEFAULT_STRATEGY));
    if (r != Z_OK)
      throw compression_failure (r);
  }

compressed_format_target::
~compressed_format_target ()
{
  try
  {
    // Close the free the compression stream.
    //
    if (!closed_)
      close ();
  }
  catch (...)
  {
  }

  deflateEnd (&zs_);
}

void compressed_format_target::
writeChars (const XMLByte* const buf,
#if _XERCES_VERSION >= 30000
            const XMLSize_t size,
#else
            const unsigned int size,
#endif
            xercesc::XMLFormatter* const)
{
  // Flush the buffer if the block is too large or if we don't have
  // any space left.
  //
  if ((size >= buf_size_ / 8 || n_ + size > buf_size_) && n_ != 0)
  {
    write (in_, n_);
    n_ = 0;
  }

  if (size < buf_size_ / 8)
  {
    memcpy (in_ + n_, reinterpret_cast<const char*> (buf), size);
    n_ += size;
  }
  else
    write (reinterpret_cast<const char*> (buf), size);
}


void compressed_format_target::
flush ()
{
  if (n_ != 0)
  {
    write (in_, n_);
    n_ = 0;
  }

  if (!os_.fail ())
    os_.flush ();
}

void compressed_format_target::
close ()
{
  write (in_, n_, true);
  n_ = 0;

  if (!os_.fail ())
    os_.flush ();

  closed_ = true;
}

void compressed_format_target::
write (const char* buf, size_t size, bool flush)
{
  zs_.next_in = reinterpret_cast<Bytef*> (const_cast<char*> (buf));
  zs_.avail_in = static_cast<uInt> (size);

  do
  {
    zs_.next_out = reinterpret_cast<Bytef*> (out_);
    zs_.avail_out = buf_size_;

    int r (deflate (&zs_, flush ? Z_FINISH : Z_NO_FLUSH));

    if (r != Z_OK && r != Z_BUF_ERROR && r != Z_STREAM_END)
      throw compression_failure (r);

    size_t n (buf_size_ - zs_.avail_out);

    if (!os_.fail () && n > 0)
      os_.write (out_, static_cast<streamsize> (n));

  } while (zs_.avail_out == 0);
}
