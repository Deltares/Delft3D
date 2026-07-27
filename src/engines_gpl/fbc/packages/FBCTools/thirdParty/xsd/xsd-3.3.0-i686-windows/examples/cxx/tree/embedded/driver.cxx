// file      : examples/cxx/tree/embedded/driver.cxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#include <memory>   // std::auto_ptr
#include <fstream>
#include <iostream>

#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLUniDefs.hpp> // chLatin_*
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/validators/common/Grammar.hpp> // xercesc::Grammar
#include <xercesc/framework/Wrapper4InputSource.hpp>

#if _XERCES_VERSION >= 30000
#  include <xercesc/framework/XMLGrammarPoolImpl.hpp>
#else
#  include <xercesc/internal/XMLGrammarPoolImpl.hpp>
#endif

#include <xsd/cxx/xml/string.hxx>
#include <xsd/cxx/xml/dom/auto-ptr.hxx>
#include <xsd/cxx/xml/dom/bits/error-handler-proxy.hxx>
#include <xsd/cxx/xml/sax/std-input-source.hxx>

#include <xsd/cxx/tree/error-handler.hxx>

#include "library.hxx"
#include "library-schema.hxx"
#include "grammar-input-stream.hxx"

using namespace std;

int
main (int argc, char* argv[])
{
  if (argc != 2)
  {
    cerr << "usage: " << argv[0] << " library.xml" << endl;
    return 1;
  }

  int r (0);

  // We need to initialize the Xerces-C++ runtime because we
  // are doing the XML-to-DOM parsing ourselves.
  //
  xercesc::XMLPlatformUtils::Initialize ();

  try
  {
    using namespace xercesc;
    namespace xml = xsd::cxx::xml;
    namespace tree = xsd::cxx::tree;

    // Create and load the grammar pool.
    //
    MemoryManager* mm (XMLPlatformUtils::fgMemoryManager);

    auto_ptr<XMLGrammarPool> gp (new XMLGrammarPoolImpl (mm));

    try
    {
      grammar_input_stream is (library_schema, sizeof (library_schema));
      gp->deserializeGrammars(&is);
    }
    catch(const XSerializationException& e)
    {
      cerr << "unable to load schema: " <<
        xml::transcode<char> (e.getMessage ()) << endl;
      return 1;
    }

    // Lock the grammar pool. This is necessary if we plan to use the
    // same grammar pool in multiple threads (this way we can reuse the
    // same grammar in multiple parsers). Locking the pool disallows any
    // modifications to the pool, such as an attempt by one of the threads
    // to cache additional schemas.
    //
    gp->lockPool ();

    // Get an implementation of the Load-Store (LS) interface.
    //
    const XMLCh ls_id [] = {chLatin_L, chLatin_S, chNull};

    DOMImplementation* impl (
      DOMImplementationRegistry::getDOMImplementation (ls_id));

#if _XERCES_VERSION >= 30000

    // Xerces-C++ 3.0.0 and later.
    //
    xml::dom::auto_ptr<DOMLSParser> parser (
      impl->createLSParser (
        DOMImplementationLS::MODE_SYNCHRONOUS, 0, mm, gp.get ()));

    DOMConfiguration* conf (parser->getDomConfig ());

    // Discard comment nodes in the document.
    //
    conf->setParameter (XMLUni::fgDOMComments, false);

    // Enable datatype normalization.
    //
    conf->setParameter (XMLUni::fgDOMDatatypeNormalization, true);

    // Do not create EntityReference nodes in the DOM tree. No
    // EntityReference nodes will be created, only the nodes
    // corresponding to their fully expanded substitution text
    // will be created.
    //
    conf->setParameter (XMLUni::fgDOMEntities, false);

    // Perform namespace processing.
    //
    conf->setParameter (XMLUni::fgDOMNamespaces, true);

    // Do not include ignorable whitespace in the DOM tree.
    //
    conf->setParameter (XMLUni::fgDOMElementContentWhitespace, false);

    // Enable validation.
    //
    conf->setParameter (XMLUni::fgDOMValidate, true);
    conf->setParameter (XMLUni::fgXercesSchema, true);
    conf->setParameter (XMLUni::fgXercesSchemaFullChecking, false);

    // Xerces-C++ 3.1.0 is the first version with working multi import
    // support.
    //
#if _XERCES_VERSION >= 30100
    conf->setParameter (XMLUni::fgXercesHandleMultipleImports, true);
#endif

    // Use the loaded grammar during parsing.
    //
    conf->setParameter (XMLUni::fgXercesUseCachedGrammarInParse, true);

    // Disable loading schemas via other means (e.g., schemaLocation).
    //
    conf->setParameter (XMLUni::fgXercesLoadSchema, false);

    // We will release the DOM document ourselves.
    //
    conf->setParameter (XMLUni::fgXercesUserAdoptsDOMDocument, true);

    // Set error handler.
    //
    tree::error_handler<char> eh;
    xml::dom::bits::error_handler_proxy<char> ehp (eh);
    conf->setParameter (XMLUni::fgDOMErrorHandler, &ehp);

#else // _XERCES_VERSION >= 30000

    // Same as above but for Xerces-C++ 2 series.
    //
    xml::dom::auto_ptr<DOMBuilder> parser (
      impl->createDOMBuilder(
        DOMImplementationLS::MODE_SYNCHRONOUS, 0, mm, gp.get ()));


    parser->setFeature (XMLUni::fgDOMComments, false);
    parser->setFeature (XMLUni::fgDOMDatatypeNormalization, true);
    parser->setFeature (XMLUni::fgDOMEntities, false);
    parser->setFeature (XMLUni::fgDOMNamespaces, true);
    parser->setFeature (XMLUni::fgDOMWhitespaceInElementContent, false);
    parser->setFeature (XMLUni::fgDOMValidation, true);
    parser->setFeature (XMLUni::fgXercesSchema, true);
    parser->setFeature (XMLUni::fgXercesSchemaFullChecking, false);
    parser->setFeature (XMLUni::fgXercesUseCachedGrammarInParse, true);
    parser->setFeature (XMLUni::fgXercesUserAdoptsDOMDocument, true);

    tree::error_handler<char> eh;
    xml::dom::bits::error_handler_proxy<char> ehp (eh);
    parser->setErrorHandler (&ehp);

#endif // _XERCES_VERSION >= 30000

    // Parse XML documents.
    //
    for (unsigned long i (0); i < 10; ++i)
    {
      ifstream ifs;
      ifs.exceptions (ifstream::badbit | ifstream::failbit);
      ifs.open (argv[1]);

      // Wrap the standard input stream.
      //
      xml::sax::std_input_source isrc (ifs, argv[1]);
      Wrapper4InputSource wrap (&isrc, false);

      // Parse XML to DOM.
      //
#if _XERCES_VERSION >= 30000
      xml_schema::dom::auto_ptr<DOMDocument> doc (parser->parse (&wrap));
#else
      xml_schema::dom::auto_ptr<DOMDocument> doc (parser->parse (wrap));
#endif

      eh.throw_if_failed<xml_schema::parsing> ();

      // Parse DOM to the object model.
      //
      auto_ptr<library::catalog> c (library::catalog_ (*doc));

      cerr << "catalog with " << c->book ().size () << " books" << endl;
    }
  }
  catch (const xml_schema::exception& e)
  {
    cerr << e << endl;
    r = 1;
  }
  catch (const std::ios_base::failure&)
  {
    cerr << argv[1] << ": unable to open or read failure" << endl;
    r = 1;
  }

  xercesc::XMLPlatformUtils::Terminate ();
  return r;
}
