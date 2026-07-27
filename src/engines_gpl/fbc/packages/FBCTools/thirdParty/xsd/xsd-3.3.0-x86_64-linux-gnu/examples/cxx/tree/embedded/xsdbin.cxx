// file      : examples/cxx/tree/embedded/xsdbin.cxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

// This program loads the XML Schema file(s) and converts them to
// the Xerces-C++ binary schema format which can then be embedded
// into C++ programs and used to validate XML documents. The output
// is written as a C++ source file containing the array with the
// binary data.
//

#include <string>
#include <memory>   // std::auto_ptr
#include <cstddef>  // std::size_t
#include <fstream>
#include <iostream>

#include <xercesc/util/XMLUni.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/util/XercesVersion.hpp>

#include <xercesc/internal/BinMemOutputStream.hpp>
#include <xercesc/validators/common/Grammar.hpp>

#include <xercesc/sax/ErrorHandler.hpp>
#include <xercesc/sax/SAXParseException.hpp>
#include <xercesc/sax2/SAX2XMLReader.hpp>
#include <xercesc/sax2/XMLReaderFactory.hpp>

#if _XERCES_VERSION >= 30000
#  include <xercesc/framework/XMLGrammarPoolImpl.hpp>
#else
#  include <xercesc/internal/XMLGrammarPoolImpl.hpp>
#endif

using namespace std;
using namespace xercesc;

class error_handler: public ErrorHandler
{
public:
  error_handler ()
      : failed_ (false)
  {
  }

  bool
  failed () const
  {
    return failed_;
  }

  enum severity {s_warning, s_error, s_fatal};

  virtual void
  warning (const SAXParseException&);

  virtual void
  error (const SAXParseException&);

  virtual void
  fatalError (const SAXParseException&);

  virtual void
  resetErrors ()
  {
    failed_ = false;
  }

  void
  handle (const SAXParseException&, severity);

private:
  bool failed_;
};

void
cxx_escape (string&);

int
main (int argc, char* argv[])
{
  const char* hxx_suffix = "-schema.hxx";
  const char* cxx_suffix = "-schema.cxx";

  string name;
  string base;
  string outdir;

  class usage {};

  int argi (1);
  bool help (false);
  bool multi_import (true);
  bool verbose (false);

  try
  {
    for (; argi < argc; ++argi)
    {
      string a (argv[argi]);

      if (a == "--help")
      {
        help = true;
        throw usage ();
      }
      else if (a == "--verbose")
      {
        verbose = true;
      }
      else if (a == "--hxx-suffix")
      {
        if (++argi >= argc)
          throw usage ();

        hxx_suffix = argv[argi];
      }
      else if (a == "--cxx-suffix")
      {
        if (++argi >= argc)
          throw usage ();

        cxx_suffix = argv[argi];
      }
      else if (a == "--output-dir")
      {
        if (++argi >= argc)
          throw usage ();

        outdir = argv[argi];
      }
      else if (a == "--array-name")
      {
        if (++argi >= argc)
          throw usage ();

        name = argv[argi];
      }
      else if (a == "--disable-multi-import")
      {
        multi_import = false;
      }
      else
        break;
    }

    if (argi >= argc)
    {
      cerr << "no input file specified" << endl;
      throw usage ();
    }

    base = argv[argi];
  }
  catch (usage const&)
  {
    cerr << "Usage: " << argv[0] << " [options] <files>" << endl
         << "Options:" << endl
         << "  --help                 Print usage information and exit." << endl
         << "  --verbose              Print progress information." << endl
         << "  --output-dir <dir>     Write generated files to <dir>." << endl
         << "  --hxx-suffix <sfx>     Header file suffix instead of '-schema.hxx'." << endl
         << "  --cxx-suffix <sfx>     Source file suffix instead of '-schema.cxx'." << endl
         << "  --array-name <name>    Binary data array name." << endl
         << "  --disable-multi-import Disable multiple import support." << endl
         << endl;

    return help ? 0 : 1;
  }

  XMLPlatformUtils::Initialize ();

  {
    MemoryManager* mm (XMLPlatformUtils::fgMemoryManager);

    auto_ptr<XMLGrammarPool> gp (new XMLGrammarPoolImpl (mm));

    // Load the schemas into grammar pool.
    //
    {
      auto_ptr<SAX2XMLReader> parser (
        XMLReaderFactory::createXMLReader (mm, gp.get ()));

      parser->setFeature (XMLUni::fgSAX2CoreNameSpaces, true);
      parser->setFeature (XMLUni::fgSAX2CoreNameSpacePrefixes, true);
      parser->setFeature (XMLUni::fgSAX2CoreValidation, true);
      parser->setFeature (XMLUni::fgXercesSchema, true);
      parser->setFeature (XMLUni::fgXercesSchemaFullChecking, true);
      parser->setFeature (XMLUni::fgXercesValidationErrorAsFatal, true);

      // Xerces-C++ 3.1.0 is the first version with working multi import
      // support.
      //
#if _XERCES_VERSION >= 30100
      parser->setFeature (XMLUni::fgXercesHandleMultipleImports, multi_import);
#endif

      error_handler eh;
      parser->setErrorHandler (&eh);

      for (; argi < argc; ++argi)
      {
        if (verbose)
          cerr << "loading " << argv[argi] << endl;

        if (!parser->loadGrammar (argv[argi], Grammar::SchemaGrammarType, true))
        {
          cerr << argv[argi] << ": error: unable to load" << endl;
          return 1;
        }

        if (eh.failed ())
          return 1;
      }
    }

    // Get the binary representation.
    //
    BinMemOutputStream data;

    try
    {
      gp->serializeGrammars (&data);
    }
    catch (const XSerializationException& e)
    {
      char* msg (XMLString::transcode (e.getMessage ()));
      cerr << "error: " << msg << endl;
      XMLString::release (&msg);
      return 1;
    }

    size_t n (static_cast<size_t> (data.curPos ()));
    const unsigned char* buf (
      static_cast<const unsigned char*> (data.getRawBuffer ()));

    if (verbose)
      cerr << "uncomressed data size " << n << " bytes" << endl;

    // Compress zeros.
    //
    size_t cn (0);
    unsigned char* cbuf = new unsigned char[n];

    size_t cseq (0);  // Number of bytes left in a compression sequence.
    bool alt (false); // Alternating or sequential sequence.

    for (size_t i (0); i < n;)
    {
      unsigned char v (buf[i++]);

      // See if we are in a compression sequence.
      //
      if (cseq != 0)
      {
        // See if this byte needs to be copied.
        //
        if (alt && cseq % 2 == 0)
          cbuf[cn++] = v;

        cseq--;
        continue;
      }

      // If we are not in a compression sequence and this byte is
      // not zero then simply copy it.
      //
      if (v != 0)
      {
        cbuf[cn++] = v;
        continue;
      }

      // We have a zero.
      //
      cbuf[cn++] = 0;

      // See if we can start a new compression sequence.
      //
      if (i < n)
      {
        if (buf[i] == 0)
        {
          // Sequential sequence. See how far it runs.
          //
          alt = false;

          for (cseq = 1; cseq < 127 && cseq + i < n; cseq++)
            if (buf[cseq + i] != 0)
              break;
        }
        else if (i + 1 < n && buf[i + 1] == 0)
        {
          // Alternating sequence. See how far it runs.
          //
          alt = true;

          for (cseq = 1; cseq < 127 && cseq * 2 + i + 1 < n; cseq++)
          {
            if (buf[cseq * 2 + i + 1] != 0)
              break;

            // For longer sequences prefer sequential to alternating.
            //
            if (cseq > 2 &&
                buf[cseq * 2 + i] == 0 &&
                buf[(cseq - 1) * 2 + i] == 0 &&
                buf[(cseq - 2) * 2 + i] == 0)
            {
              cseq -= 2;
              break;
            }
          }

          cseq *= 2;
        }
      }

      if (cseq != 0)
      {
        cbuf[cn++] = static_cast<unsigned char> (
          alt ? (128  | cseq / 2) : cseq);
      }
      else
        cbuf[cn++] = 0;
    }

    if (verbose)
      cerr << "comressed data size " << cn << " bytes" << endl;

    buf = cbuf;
    n = cn;

    // Figure out the file names.
    //
    string::size_type p (base.rfind ('/')), p1 (base.rfind ('\\'));

    if (p1 != string::npos && p1 > p)
      p = p1;

    if (p != string::npos)
      base = string (base, p + 1);

    p = base.rfind ('.');

    if (p != string::npos)
      base.resize (p);

    string hxx (base + hxx_suffix);
    string cxx (base + cxx_suffix);

    if (!outdir.empty ())
    {
#if defined (WIN32) || defined (__WIN32__)
      hxx = outdir + '\\' + hxx;
      cxx = outdir + '\\' + cxx;
#else
      hxx = outdir + '/' + hxx;
      cxx = outdir + '/' + cxx;
#endif
    }

    if (name.empty ())
    {
      name = base + "_schema";
      cxx_escape (name);
    }

    // Write header.
    //
    {
      ofstream os (hxx.c_str ());

      if (!os.is_open ())
      {
        cerr << hxx << ": error: unable to open" << endl;
        return 1;
      }

      os << "// Automatically generated. Do not edit." << endl
         << "//" << endl
         << endl
         << "#include <xercesc/util/XercesDefs.hpp>" << endl
         << endl
         << "extern const XMLByte " << name << "[" << n << "UL];" << endl;
    }

    {
      ofstream os (cxx.c_str ());

      if (!os.is_open ())
      {
        cerr << cxx << ": error: unable to open" << endl;
        return 1;
      }

      os << "// Automatically generated. Do not edit." << endl
         << "//" << endl
         << endl
         << "#include <xercesc/util/XercesDefs.hpp>" << endl
         << "#include <xercesc/util/XercesVersion.hpp>" << endl
         << endl
         << "#if XERCES_GRAMMAR_SERIALIZATION_LEVEL != " <<
        XERCES_GRAMMAR_SERIALIZATION_LEVEL << endl
         << "#  error incompatible Xerces-C++ version detected" << endl
         << "#endif" << endl
         << endl
         << "extern const XMLByte " << name << "[" << n << "UL] =" << endl
         << "{";

      for (size_t i (0); i < n; ++i)
      {
        if (i != 0)
          os << ',';

        os << (i % 12 == 0 ? "\n  " : " ") << "0x";
        os.width (2);
        os.fill ('0');
        os << hex << static_cast<unsigned short> (buf[i]);
      }

      os << endl
         << "};" << endl
         << endl;
    }

    delete[] cbuf;
  }

  XMLPlatformUtils::Terminate ();
}

void
cxx_escape (string& s)
{
  for (string::size_type i (0); i < s.size (); ++i)
  {
    char& c (s[i]);

    if (i == 0)
    {
      if (!((c >= 'a' && c <= 'z') ||
            (c >= 'A' && c <= 'Z') ||
            c == '_'))
        c = '_';
    }
    else
    {
      if (!((c >= 'a' && c <= 'z') ||
            (c >= 'A' && c <= 'Z') ||
            (c >= '0' && c <= '9') ||
            c == '_'))
        c = '_';
    }
  }
}

void error_handler::
warning (const SAXParseException& e)
{
  handle (e, s_warning);
}

void error_handler::
error (const SAXParseException& e)
{
  failed_ = true;
  handle (e, s_error);
}

void error_handler::
fatalError (const SAXParseException& e)
{
  failed_ = true;
  handle (e, s_fatal);
}

void error_handler::
handle (const SAXParseException& e, severity s)
{
  const XMLCh* xid (e.getPublicId ());

  if (xid == 0)
    xid = e.getSystemId ();

  char* id (XMLString::transcode (xid));
  char* msg (XMLString::transcode (e.getMessage ()));

  cerr << id << ":";

#if _XERCES_VERSION >= 30000
  cerr << e.getLineNumber () << ":" << e.getColumnNumber () << " ";
#else
  XMLSSize_t l (e.getLineNumber ());
  XMLSSize_t c (e.getColumnNumber ());
  cerr << (l == -1 ? 0 : l) << ":" << (c == -1 ? 0 : c) << " ";
#endif

  cerr << (s == s_warning ? "warning: " : "error: ") << msg << endl;

  XMLString::release (&id);
  XMLString::release (&msg);
}
