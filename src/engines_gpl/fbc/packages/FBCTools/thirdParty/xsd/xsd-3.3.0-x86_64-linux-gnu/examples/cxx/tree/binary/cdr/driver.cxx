// file      : examples/cxx/tree/binary/cdr/driver.cxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#include <memory>   // std::auto_ptr
#include <cstring>  // std::memcpy
#include <iostream>

#include <ace/Log_Msg.h>   // ACE_HEX_DUMP
#include <ace/CDR_Stream.h>

// The following two headers define XSD-specific insertion/extraction
// operations for ACE CDR streams. You can use the content of these
// headers as a guide to implementing insertion/extraction to/from
// your own data representation streams:
//
// xsd/cxx/tree/ace-cdr-stream-insertion.hxx
// xsd/cxx/tree/ace-cdr-stream-extraction.hxx

#include "library.hxx"

using std::cerr;
using std::endl;

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

    // Read in the file.
    //
    std::auto_ptr<catalog> c (catalog_ (argv[1]));

    cerr << *c << endl;

    // Save to a CDR stream.
    //
    ACE_OutputCDR ace_ocdr;
    xml_schema::ostream<ACE_OutputCDR> ocdr (ace_ocdr);

    ocdr << *c;

    // Print the binary representation and at the same time save
    // it into a continuous buffer.
    //
    cerr << endl
         << "binary representation size: " << ace_ocdr.total_length () << endl;

    xml_schema::buffer buf (ace_ocdr.total_length ());
    char* data (buf.data ());

    for (const ACE_Message_Block* mb = ace_ocdr.begin ();
         mb != 0;
         mb = mb->cont ())
    {
      std::memcpy (data, mb->rd_ptr (), mb->length ());
      data += mb->length ();

      ACE_HEX_DUMP ((LM_DEBUG, mb->rd_ptr (), mb->length ()));
    }

    // Load from a CDR stream. Note that ACE_InputCDR expects the
    // buffer to be properly aligned. Since our buffer is dynamically
    // allocated, its alignment should be good enough.
    //
    ACE_InputCDR ace_icdr (buf.data (), buf.size ());
    xml_schema::istream<ACE_InputCDR> icdr (ace_icdr);

    std::auto_ptr<catalog> copy (new catalog (icdr));

    cerr << *copy << endl;
  }
  catch (const xml_schema::exception& e)
  {
    cerr << e << endl;
    return 1;
  }

  return 0; // ACE makes our main() an ordinary function.
}
