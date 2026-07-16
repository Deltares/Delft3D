// file      : examples/cxx/tree/wildcard/driver.cxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#include <string>
#include <memory>   // std::auto_ptr
#include <cstring>  // std::memcpy
#include <iostream>

#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/PlatformUtils.hpp>

#include "email.hxx"

// The following string class keeps us sane when working with Xerces.
// Include it after the generated header in order to get only char or
// wchar_t version depending on how you compiled your schemas.
//
#include <xsd/cxx/xml/string.hxx>

using std::cerr;
using std::endl;
using std::string;

int
main (int argc, char* argv[])
{
  if (argc != 2)
  {
    cerr << "usage: " << argv[0] << " email.xml" << endl;
    return 1;
  }

  using namespace xercesc;

  int r (0);

  // The Xerces-C++ DOM objects that will be used to store the
  // content matched by wildcards "out-live" the call to the
  // parsing function. Therefore we need to initialize the
  // Xerces-C++ runtime ourselves.
  //
  XMLPlatformUtils::Initialize ();

  try
  {
    using namespace email;
    namespace xml = xsd::cxx::xml;

    // Read in the message.
    //
    std::auto_ptr<envelope> msg (
      message (argv[1], xml_schema::flags::dont_initialize));

    // Print what we've got.
    //
    cerr << "To:      " << msg->to () << endl
         << "From:    " << msg->from () << endl
         << "Subject: " << msg->subject () << endl;

    envelope::any_sequence& body (msg->any ());

    for (envelope::any_iterator i (body.begin ()); i != body.end (); ++i)
    {
      DOMElement& e (*i);
      string name (xml::transcode<char> (e.getLocalName ()));

      if (name == "text")
      {
        // Create object representation for the text element.
        //
        xml_schema::string text (e);

        cerr << text << endl
             << endl;
      }
      else if (name == "binary")
      {
        // Create object representation for the binary element.
        //
        binary bin (e);

        cerr << "binary: " << bin.name () << " type: " << bin.mime () << endl
             << endl;
      }
      else
      {
        cerr << "unknown body type: " << name << endl;
      }
    }

    // Create a reply message.
    //
    envelope reply (msg->from (), msg->to (), "Re: " + msg->subject ());

    // Copy the thread-id attribute from the original message if any.
    //
    envelope::any_attribute_set& as (msg->any_attribute ());
    envelope::any_attribute_iterator ti (
      as.find ("http://www.codesynthesis.com/email", "thread-id"));

    if (ti != as.end ())
      reply.any_attribute ().insert (*ti);

    // Add a text body.
    //
    DOMDocument& doc (reply.dom_document ());
    envelope::any_sequence& rbody (reply.any ());

    xml_schema::string text ("Hi!\n\n"
                             "Indeed nice pictures. Check out mine.\n\n"
                             "Jane");

    DOMElement* e (
      doc.createElementNS (
        xml::string ("http://www.codesynthesis.com/email").c_str (),
        xml::string ("eml:text").c_str ()));

    *e << text;
    rbody.push_back (e);

    // Add a (fake) image.
    //
    binary pic ("pic.jpg", "image/jpeg");
    pic.size (3);
    std::memcpy (pic.data (), "123", 3);

    e = doc.createElementNS (
      xml::string ("http://www.codesynthesis.com/email").c_str (),
      xml::string ("eml:binary").c_str ());

    *e << pic;
    rbody.push_back (e);


    // Prepare namespace mapping and schema location information for
    // serialization.
    //
    xml_schema::namespace_infomap map;

    map["eml"].name = "http://www.codesynthesis.com/email";
    map["eml"].schema = "email.xsd";

    // Write it out.
    //
    message (std::cout,
             reply,
             map,
             "UTF-8",
             xml_schema::flags::dont_initialize);
  }
  catch (const xml_schema::exception& e)
  {
    cerr << e << endl;
    r = 1;
  }

  XMLPlatformUtils::Terminate ();
  return r;
}
