// file      : examples/cxx/tree/compression/compressed-input-source.hxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#ifndef COMPRESSED_INPUT_SOURCE_HXX
#define COMPRESSED_INPUT_SOURCE_HXX

#include <zlib.h>

#include <iosfwd>
#include <string>
#include <cstddef>   // std::size_t
#include <exception>

#include <xercesc/sax/InputSource.hpp>
#include <xercesc/util/BinInputStream.hpp>

struct decompression_failure: std::exception
{
  explicit
  decompression_failure (int code)
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

// Xerces-C++ InputSource interface implementation with on-the-fly, zlib-
// based decompression.
//
class compressed_input_source: public xercesc::InputSource
{
public:
  enum compression_type
  {
    raw,
    zlib,
    gzip
  };

  compressed_input_source (std::istream&, compression_type);

  compressed_input_source (std::istream&,
                           compression_type,
                           const std::string& system_id);

  compressed_input_source (std::istream&,
                           compression_type,
                           const std::string& system_id,
                           const std::string& public_id);

  struct copy {};

  // Throws the copy exception if this function is called more than once.
  //
  virtual xercesc::BinInputStream*
  makeStream () const;

private:
  mutable std::istream* is_;
  compression_type type_;
};

// Xerces-C++ BinInputStream interface implementation with on-the-fly, zlib-
// based decompression.
//
class compressed_input_stream: public xercesc::BinInputStream
{
public:
  enum compression_type
  {
    raw,
    zlib,
    gzip
  };

  compressed_input_stream (std::istream&, compression_type);

  virtual
  ~compressed_input_stream ();

#if _XERCES_VERSION >= 30000
  virtual XMLFilePos
  curPos () const;

  virtual XMLSize_t
  readBytes (XMLByte* const buf, const XMLSize_t size);

  virtual const XMLCh*
  getContentType () const;

#else

  virtual unsigned int
  readBytes (XMLByte* const buf, const unsigned int size);

  virtual unsigned int
  curPos () const;
#endif

private:
  std::size_t
  read ();

private:
  std::istream& is_;
  z_stream zs_;
  bool end_;

  static const std::size_t buf_size_ = 65536;
  char in_[buf_size_];
  std::size_t pos_; // Current decompressed stream position.
};

#endif // COMPRESSED_INPUT_SOURCE_HXX
