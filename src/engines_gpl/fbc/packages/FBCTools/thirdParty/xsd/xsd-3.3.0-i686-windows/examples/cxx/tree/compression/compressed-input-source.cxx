// file      : examples/cxx/tree/compression/compressed-input-source.cxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#include <istream>

#include <xsd/cxx/xml/string.hxx>

#include "compressed-input-source.hxx"

using namespace std;
using namespace xercesc;
namespace xml = xsd::cxx::xml;

//
// decompression_failure
//

const char* decompression_failure::
what () const throw ()
{
  return "decompression failure";
}

//
// compressed_input_source
//

compressed_input_source::
compressed_input_source (istream& is, compression_type t)
    : is_ (&is), type_ (t)
{
}

compressed_input_source::
compressed_input_source (istream& is,
                         compression_type t,
                         const string& sysid)
    : InputSource (xml::string (sysid).c_str ()), is_ (&is), type_ (t)
{
}

compressed_input_source::
compressed_input_source (istream& is,
                         compression_type t,
                         const string& sysid,
                         const string& pubid)
    : xercesc::InputSource (xml::string (sysid).c_str (),
                            xml::string (pubid).c_str ()),
      is_ (&is),
      type_ (t)
{
}

BinInputStream* compressed_input_source::
makeStream () const
{
  if (is_ == 0)
    throw copy ();

  istream& is (*is_);
  is_ = 0;
  return new compressed_input_stream (
    is, static_cast<compressed_input_stream::compression_type> (type_));
}

//
// compressed_input_stream
//

compressed_input_stream::
compressed_input_stream (istream& is, compression_type t)
    : is_ (is), end_ (false), pos_ (0)
{
  zs_.zalloc = Z_NULL;
  zs_.zfree  = Z_NULL;
  zs_.opaque = Z_NULL;
  zs_.next_in = Z_NULL;
  zs_.avail_in = 0;

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

  int r (inflateInit2 (&zs_, window));

  if (r != Z_OK)
    throw decompression_failure (r);
}

compressed_input_stream::
~compressed_input_stream ()
{
  inflateEnd (&zs_);
}

#if _XERCES_VERSION >= 30000
XMLFilePos compressed_input_stream::
curPos () const
{
  return static_cast<XMLFilePos> (pos_);
}
#else
unsigned int compressed_input_stream::
curPos () const
{
  return static_cast<unsigned int> (pos_);
}
#endif

#if _XERCES_VERSION >= 30000
XMLSize_t compressed_input_stream::
readBytes (XMLByte* const buf, const XMLSize_t size)
#else
unsigned int compressed_input_stream::
readBytes (XMLByte* const buf, const unsigned int size)
#endif
{
  if (end_)
    return 0;

  // Keep calling inflate() until we fill up the buffer or reach the
  // end of stream. If we run out of input data, call the underlying
  // stream for more.
  //
  zs_.next_out = reinterpret_cast<Bytef*> (buf);
  zs_.avail_out = static_cast<uInt> (size);

  int r;

  do
  {
    if (zs_.avail_in == 0)
    {
      zs_.avail_in = static_cast<uInt> (read ());
      zs_.next_in = reinterpret_cast<Bytef*> (const_cast<char*> (in_));

      if (zs_.avail_in == 0)
        throw decompression_failure (Z_DATA_ERROR);
    }

    r = inflate (&zs_, Z_NO_FLUSH);

    if (r != Z_OK && r != Z_STREAM_END)
      throw decompression_failure (r);

  } while (r != Z_STREAM_END && zs_.avail_out != 0);

  if (r == Z_STREAM_END)
    end_ = true;

  size_t n (size - zs_.avail_out);
  pos_ += n;

#if _XERCES_VERSION >= 30000
  return static_cast<XMLSize_t> (n);
#else
  return static_cast<unsigned int> (n);
#endif
}

#if _XERCES_VERSION >= 30000
const XMLCh* compressed_input_stream::
getContentType () const
{
  return 0;
}
#endif

size_t compressed_input_stream::
read ()
{
  // Some implementations don't clear gcount if you call read() on a
  // stream that is in the eof state.
  //
  if (is_.eof ())
    return 0;

  // Unset the exception failbit while we are working with the stream.
  //
  ios_base::iostate old (is_.exceptions ());
  is_.exceptions (old & ~ios_base::failbit);

  is_.read (in_, static_cast<streamsize> (buf_size_));

  // Clear the fail bit if it was caused by eof and restore the original
  // exception state. If there are any pending errors then the exception
  // will be thrown now.
  //
  if (is_.fail () && is_.eof ())
    is_.clear (is_.rdstate () & ~ios_base::failbit);

  is_.exceptions (old);

  // Make sure that if we failed, we won't be called again.
  //
  return !is_.fail () ? static_cast<size_t> (is_.gcount ()) : 0;
}
