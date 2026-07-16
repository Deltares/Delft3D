// file      : examples/cxx/tree/performance/serialization.cxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#include <memory>  // std::auto_ptr
#include <cstddef> // std::size_t
#include <sstream>
#include <iostream>

#include <xercesc/dom/DOM.hpp>
#if _XERCES_VERSION >= 30000
#  include <xercesc/dom/DOMLSOutput.hpp>
#  include <xercesc/dom/DOMLSSerializer.hpp>
#else
#  include <xercesc/dom/DOMWriter.hpp>
#endif
#include <xercesc/dom/DOMImplementation.hpp>
#include <xercesc/dom/DOMImplementationRegistry.hpp>

#include <xercesc/framework/MemBufFormatTarget.hpp>

#include <xercesc/util/XMLUniDefs.hpp>

#include <xsd/cxx/xml/dom/bits/error-handler-proxy.hxx>
#include <xsd/cxx/tree/error-handler.hxx>

#include "time.hxx"
#include "test.hxx"

using namespace std;

bool
serialization (const char* file, unsigned long iter)
{
  try
  {
    using namespace xercesc;

    cerr << "serialization:" << endl;

    // Get the object model using the standard parsing function.
    //
    auto_ptr<test::root> r (
      test::root_ (file,
                   xml_schema::flags::dont_initialize |
                   xml_schema::flags::dont_validate));

    // Serialize it to the in-memory buffer. This makes sure the buffer
    // pre-allocates enough memory.
    //
    xml_schema::namespace_infomap map;
    map["t"].name = "test";
    map["t"].schema = "test.xsd";

    MemBufFormatTarget ft (10240);
    test::root_ (ft, *r, map, "UTF-8",
                 xml_schema::flags::dont_initialize |
                 xml_schema::flags::dont_pretty_print |
                 xml_schema::flags::no_xml_declaration);

    size_t size (ft.getLen ());
    cerr << "  document size:  " << size << " bytes" << endl
         << "  iterations:     " << iter << endl;

    // Create XML serializer that we are going to use in all iterations.
    //
    const XMLCh ls_id[] =
      {xercesc::chLatin_L, xercesc::chLatin_S, xercesc::chNull};

    DOMImplementation* impl (
      DOMImplementationRegistry::getDOMImplementation (ls_id));

    // Use the error handler implementation provided by the XSD runtime.
    //
    xsd::cxx::tree::error_handler<char> eh;
    xsd::cxx::xml::dom::bits::error_handler_proxy<char> ehp (eh);

#if _XERCES_VERSION >= 30000
    xml_schema::dom::auto_ptr<DOMLSSerializer> writer (
      impl->createLSSerializer ());

    DOMConfiguration* conf (writer->getDomConfig ());

    conf->setParameter (XMLUni::fgDOMErrorHandler, &ehp);
    conf->setParameter (XMLUni::fgDOMXMLDeclaration, false);

    xml_schema::dom::auto_ptr<DOMLSOutput> out (impl->createLSOutput ());

    out->setByteStream (&ft);
#else
    // Same as above but for Xerces-C++ 2 series.
    //
    xml_schema::dom::auto_ptr<DOMWriter> writer (impl->createDOMWriter ());

    writer->setErrorHandler (&ehp);
    writer->setFeature (XMLUni::fgDOMXMLDeclaration, false);
#endif

    // Serialization loop.
    //
    os::time start;

    for (unsigned long i (0); i < iter; ++i)
    {
      // First serialize the object model to DOM.
      //
      xml_schema::dom::auto_ptr<DOMDocument> doc (test::root_ (*r, map));

      ft.reset ();

      // Then serialize DOM to XML reusing the serializer we created above.
      //
#if _XERCES_VERSION >= 30000
      writer->write (doc.get (), out.get ());
#else
      writer->writeNode (&ft, *doc);
#endif

      eh.throw_if_failed<xml_schema::serialization> ();
    }

    os::time end;
    os::time time (end - start);

    cerr << "  time:           " << time << " sec" << endl;

    double ms (time.sec () * 1000000ULL + time.nsec () / 1000ULL);

    // Calculate throughput in documents/sec.
    //
    double tpd ((iter / ms) * 1000000);
    cerr << "  throughput:     " << tpd << " documents/sec" << endl;

    // Calculate throughput in MBytes/sec.
    //
    double tpb (((size * iter) / ms) * 1000000/(1024*1024));
    cerr << "  throughput:     " << tpb << " MBytes/sec" << endl;
  }
  catch (const xml_schema::exception& e)
  {
    cerr << e << endl;
    return false;
  }
  catch (std::ios_base::failure const&)
  {
    cerr << "io failure" << endl;
    return false;
  }

  return true;
}
