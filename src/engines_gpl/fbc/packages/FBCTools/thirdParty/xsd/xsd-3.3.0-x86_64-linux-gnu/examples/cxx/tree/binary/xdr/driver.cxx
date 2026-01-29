// file      : examples/cxx/tree/binary/xdr/driver.cxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#include <memory>   // std::auto_ptr
#include <cstring>  // std::memcpy
#include <cstddef>  // std::size_t
#include <iostream>

#include <rpc/types.h>
#include <rpc/xdr.h>

#include "library.hxx"

using std::cerr;
using std::endl;
using std::size_t;

// XDR output functions. Their implementations are provided after main().
//
struct underflow_info
{
  xml_schema::buffer* buf;
  size_t pos;
};

extern "C" int
overflow (void* user_data, char* buf, int n);

extern "C" int
underflow (void* user_data, char* buf, int n);

// The xdrrec_create function (used below) has slightly different
// prototypes on different platforms. To make this example portable
// we will need to cast the actual function to the following common
// prototype.
//
extern "C"
typedef  void (*xdrrec_create_p) (
  XDR*,
  unsigned int write_size,
  unsigned int read_size,
  void* user_data,
  int (*read) (void* user_data, char* buf, int n),
  int (*write) (void* user_data, char* buf, int n));

int
main (int argc, char* argv[])
{
  if (argc != 2)
  {
    cerr << "usage: " << argv[0] << " library.xml" << endl;
    return 1;
  }

  try
  {
    using namespace library;

    xdrrec_create_p xdrrec_create_ =
      reinterpret_cast<xdrrec_create_p> (::xdrrec_create);

    // Read in the file.
    //
    std::auto_ptr<catalog> c (catalog_ (argv[1]));

    cerr << *c << endl;

    // Save to an XDR stream.
    //
    XDR xdr;
    xml_schema::buffer buf;

    xdrrec_create_ (&xdr, 0, 0, reinterpret_cast<char*> (&buf), 0, &overflow);
    xdr.x_op = XDR_ENCODE;

    xml_schema::ostream<XDR> oxdr (xdr);

    oxdr << *c;

    xdrrec_endofrecord (&xdr, true); // Flush the data.
    xdr_destroy (&xdr);

    // The binary representation is now in the memory buffer 'buf'.
    // To get to the raw data use buf.data() and buf.size().
    //
    cerr << endl
         << "binary representation size: " << buf.size () << endl;

    // Load from an XDR stream.
    //
    underflow_info ui;
    ui.buf = &buf;
    ui.pos = 0;

    xdrrec_create_ (&xdr, 0, 0, reinterpret_cast<char*> (&ui), &underflow, 0);
    xdr.x_op = XDR_DECODE;

    xdrrec_skiprecord (&xdr);

    xml_schema::istream<XDR> ixdr (xdr);

    std::auto_ptr<catalog> copy (new catalog (ixdr));

    xdr_destroy (&xdr);

    cerr << *copy << endl;
  }
  catch (const xml_schema::exception& e)
  {
    cerr << e << endl;
    return 1;
  }
}

extern "C" int
overflow (void* p, char* buf, int n_)
{
  xml_schema::buffer* dst (reinterpret_cast<xml_schema::buffer*> (p));
  size_t n (static_cast<size_t> (n_));

  size_t size (dst->size ());
  size_t capacity (dst->capacity ());

  // Implement exponential growth.
  //
  if (size + n > capacity && size + n < capacity * 2)
    dst->capacity (capacity * 2);

  dst->size (size + n);
  std::memcpy (dst->data () + size, buf, n);

  return n;
}

extern "C" int
underflow (void* p, char* buf, int n_)
{
  underflow_info* ui (reinterpret_cast<underflow_info*> (p));
  size_t n (static_cast<size_t> (n_));

  size_t size (ui->buf->size () - ui->pos);
  n = size > n ? n : size;

  std::memcpy (buf, ui->buf->data () + ui->pos, n);
  ui->pos += n;

  return n;
}
