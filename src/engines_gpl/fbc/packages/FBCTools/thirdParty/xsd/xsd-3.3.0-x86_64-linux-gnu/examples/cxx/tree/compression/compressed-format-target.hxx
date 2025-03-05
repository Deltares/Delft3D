// file      : examples/cxx/tree/compression/compressed-format-target.hxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#ifndef COMPRESSED_FORMAT_TARGET_HXX
#define COMPRESSED_FORMAT_TARGET_HXX

#include <zlib.h>

#include <iosfwd>
#include <cstddef>   // std::size_t
#include <exception>

#include <xercesc/framework/XMLFormatter.hpp>

struct compression_failure: std::exception
{
  explicit
  compression_failure (int code)
      : code_ (code)
  {
  }

  int
  code () const
  {
    return code_;
  }

  const char*
  message () const
  {
    return zError (code_);
  }

  virtual const char*
  what () const throw ();

private:
  int code_;
};

// Xerces-C++ XMLFormatTarget interface implementation with on-the-fly,
// zlib-based compression.
//
class compressed_format_target: public xercesc::XMLFormatTarget
{
public:
  enum compression_type
  {
    raw,
    zlib,
    gzip
  };

  compressed_format_target (std::ostream&, compression_type);

  virtual
  ~compressed_format_target ();

  virtual void
  writeChars (const XMLByte* const buf,
#if _XERCES_VERSION >= 30000
              const XMLSize_t size,
#else
              const unsigned int size,
#endif
              xercesc::XMLFormatter* const);

  virtual void
  flush ();

  // Close the compressed stream by writing out the zlib or gzip trailer.
  // This function is automatically called from the destructor but you
  // may want to call it explicitly to be able to catch any exceptions
  // that it might throw.
  //
  void
  close ();

private:
  void
  write (const char* buf, std::size_t size, bool flush = false);

private:
  std::ostream& os_;
  z_stream zs_;
  bool closed_;

  static const std::size_t buf_size_ = 65536;
  char in_[buf_size_];
  char out_[buf_size_];
  size_t n_;
};

#endif // COMPRESSED_FORMAT_TARGET_HXX
