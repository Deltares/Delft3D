// file      : examples/cxx/parser/performance/driver.cxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#include <string>
#include <memory>  // std::auto_ptr
#include <cstddef> // std::size_t
#include <fstream>
#include <sstream>
#include <iostream>

#include "time.hxx"
#include "test-pskel.hxx"

#ifdef _XERCES_VERSION
#  include <xercesc/sax2/SAX2XMLReader.hpp>
#  include <xercesc/sax2/XMLReaderFactory.hpp>
#  include <xercesc/framework/MemBufInputSource.hpp>
#  include <xercesc/validators/common/Grammar.hpp>
#  include <xercesc/util/PlatformUtils.hpp>
#  include <xercesc/util/XMLUni.hpp>

#  include <xsd/cxx/xml/sax/bits/error-handler-proxy.hxx>
#  include <xsd/cxx/parser/error-handler.hxx>

#endif

// No-op parser implementation.
//
namespace test
{
  struct enum_pimpl: enum_pskel, xml_schema::string_pimpl
  {
    virtual void
    post_enum ()
    {
    }
  };

  struct record_pimpl: record_pskel
  {
    virtual void
    int_ (unsigned int)
    {
    }

    virtual void
    double_ (double)
    {
    }

    virtual void
    name (const std::string&)
    {
    }

    virtual void
    string (const std::string&)
    {
    }

    virtual void
    choice1 (const std::string&)
    {
    }

    virtual void
    choice2 (const std::string&)
    {
    }

    virtual void
    choice3 (const std::string&)
    {
    }

    virtual void
    choice4 (const std::string&)
    {
    }

    virtual void
    apple (bool)
    {
    }

    virtual void
    orange (unsigned long long)
    {
    }
  };

  struct root_pimpl: root_pskel
  {
  };
}

using namespace std;

int
main (int argc, char* argv[])
{
  if (argc < 2)
  {
    cerr << "usage: " << argv[0] << " [-v] [-i <count>] test.xml" << endl
         << "\t -v turn on validation (default is off)" << endl
         << "\t -i number of iterations to perform (default is 1000)" << endl;
    return 1;
  }

  bool validate (false);
  unsigned long iter (1000);
  const char* file (0);

  // Parse command line arguments.
  //
  for (int i (1); i < argc; ++i)
  {
    string arg (argv[i]);

    if (arg == "-v")
    {
      validate = true;
    }
    else if (arg == "-i")
    {
      if (++i == argc)
      {
        cerr << "argument expected for the -i option" << endl;
        return 1;
      }

      iter = 0;
      istringstream is (argv[i]);
      is >> iter;

      if (iter == 0)
      {
        cerr << "invalid argument for the -i option" << endl;
        return 1;
      }
    }
    else
    {
      file = argv[i];
      break;
    }
  }

  if (file == 0)
  {
    cerr << "no input file specified" << endl;
    return 1;
  }

  try
  {
    // Instantiate and connect parsers.
    //
    xml_schema::unsigned_int_pimpl unsigned_int_p;
    xml_schema::double_pimpl double_p;
    xml_schema::ncname_pimpl ncname_p;
    xml_schema::string_pimpl string_p;
    xml_schema::boolean_pimpl boolean_p;
    xml_schema::unsigned_long_pimpl unsigned_long_p;

    test::enum_pimpl enum_p;
    test::record_pimpl record_p;
    test::root_pimpl root_p;

    record_p.parsers (unsigned_int_p,
                      double_p,
                      ncname_p,
                      string_p,
                      string_p,
                      string_p,
                      string_p,
                      string_p,
                      enum_p,
                      boolean_p,
                      unsigned_long_p);

    root_p.parsers (record_p);

    // Read the fine into in-memory buffer.
    //
    ifstream ifs;
    ifs.exceptions (ios_base::failbit);
    ifs.open (file, ios::in | ios::ate);

    size_t size (ifs.tellg ());
    ifs.seekg (0, ios::beg);

    char* buf = new char[size];
    ifs.read (buf, size);
    ifs.close ();

    cerr << "document size:  " << size << " bytes" << endl
         << "iterations:     " << iter << endl;

    os::time time (0);
    xml_schema::document doc (root_p, "test", "root");

#ifdef _XERCES_VERSION

    // Xerces-C++ as the underlying XML parser.
    //
    using namespace xercesc;

    namespace xml = xsd::cxx::xml;
    namespace parser = xsd::cxx::parser;

    XMLPlatformUtils::Initialize ();

    {
      MemBufInputSource is (
        reinterpret_cast<XMLByte*> (buf), size, file, false);
      is.setCopyBufToStream (false);

      auto_ptr<SAX2XMLReader> parser (XMLReaderFactory::createXMLReader ());

      parser->setFeature (XMLUni::fgSAX2CoreNameSpaces, true);
      parser->setFeature (XMLUni::fgSAX2CoreNameSpacePrefixes, true);
      parser->setFeature (XMLUni::fgXercesValidationErrorAsFatal, true);

      if (validate)
      {
        parser->setFeature (XMLUni::fgSAX2CoreValidation, true);
        parser->setFeature (XMLUni::fgXercesSchema, true);
        parser->setFeature (XMLUni::fgXercesSchemaFullChecking, false);

        // Xerces-C++ 3.1.0 is the first version with working multi import
        // support.
        //
#if _XERCES_VERSION >= 30100
        parser->setFeature (XMLUni::fgXercesHandleMultipleImports, true);
#endif

        // Initialize the schema cache. To detect schema errors we will
        // need an error handler.
        //
        parser::error_handler<char> eh;
        xml::sax::bits::error_handler_proxy<char> ehp (eh);
        parser->setErrorHandler (&ehp);

        if (!parser->loadGrammar ("test.xsd", Grammar::SchemaGrammarType, true))
        {
          // In Xerces-C++ grammar loading failure results in just a warning.
          // Make it a fatal error.
          //
          eh.handle ("test.xsd", 0, 0,
                     parser::error_handler<char>::severity::fatal,
                     "unable to load schema");
        }

        eh.throw_if_failed ();
        parser->setFeature (XMLUni::fgXercesUseCachedGrammarInParse, true);

#if _XERCES_VERSION >= 30000
        parser->setFeature (XMLUni::fgXercesLoadSchema, false);
#endif
      }
      else
      {
        parser->setFeature (XMLUni::fgSAX2CoreValidation, false);
        parser->setFeature (XMLUni::fgXercesSchema, false);
        parser->setFeature (XMLUni::fgXercesSchemaFullChecking, false);
      }

      os::time start;

      for (unsigned long i (0); i < iter; ++i)
      {
        root_p.pre ();
        doc.parse (is, *parser);
        root_p.post_root ();
      }

      os::time end;
      time = end - start;
    }

    XMLPlatformUtils::Terminate ();

#else

    // Expat as the underlying XML parser.
    //
    XML_Parser xml_parser (XML_ParserCreateNS (0, ' '));
    string public_id (file);

    os::time start;

    for (unsigned long i (0); i < iter; ++i)
    {
      // Using the low-level Expat-specific API to parse the memory
      // buffer.
      //
      root_p.pre ();
      doc.parse_begin (xml_parser, public_id);

      XML_Parse (xml_parser, buf, size, 1);

      doc.parse_end ();
      root_p.post_root ();

      XML_ParserReset (xml_parser, 0);
    }

    os::time end;
    time = end - start;

    XML_ParserFree (xml_parser);

#endif

    delete[] buf;

    cerr << "time:           " << time << " sec" << endl;

    double ms (time.sec () * 1000000ULL + time.nsec () / 1000ULL);

    // Calculate throughput in documents/sec.
    //
    double tpd ((iter / ms) * 1000000);
    cerr << "throughput:     " << tpd << " documents/sec" << endl;

    // Calculate throughput in MBytes/sec.
    //
    double tpb (((size * iter) / ms) * 1000000/(1024*1024));
    cerr << "throughput:     " << tpb << " MBytes/sec" << endl;
  }
  catch (const xml_schema::exception& e)
  {
    cerr << e << endl;
    return 1;
  }
  catch (std::ios_base::failure const&)
  {
    cerr << "io failure" << endl;
    return 1;
  }
}
