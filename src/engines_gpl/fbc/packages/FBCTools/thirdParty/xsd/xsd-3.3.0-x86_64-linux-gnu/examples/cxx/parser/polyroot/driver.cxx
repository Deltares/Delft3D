// file      : examples/cxx/parser/polyroot/driver.cxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : not copyrighted - public domain

#include <map>
#include <string>
#include <iostream>

#include "supermen-pimpl.hxx"

using std::cerr;
using std::endl;
using xml_schema::ro_string;

// Customize the xml_schema::document object to handle polymorphic
// root element. For more information see the multiroot example.
//
class document: public xml_schema::document
{
public:
  document (const xml_schema::parser_map& parser_map)
      : xml_schema::document (true), // Indicate polymorphic parsing.
        parser_map_ (parser_map)
  {
  }

protected:
  // This function is called to obtain the root element type parser.
  // If the returned pointer is 0 then the whole document content
  // is ignored. The type argument contains the XML Schema type
  // if xsi:type attribute was specified for this element and 0
  // otherwise.
  //
  virtual xml_schema::parser_base*
  start_root_element (const ro_string& ns,
                      const ro_string& name,
                      const ro_string* type)
  {
    if (name != "person" || !ns.empty ())
      return 0;

    xml_schema::parser_base* base;

    // Search the parser map.
    //
    if (type == 0)
    {
      // No xsi:type. Static type should be used.
      //
      ro_string st (person_pskel::_static_type ());
      base = parser_map_.find (st);
    }
    else
    {
      base = parser_map_.find (*type);
    }

    if (base != 0)
    {
      parser_used_ = dynamic_cast<person_pskel*> (base);
      parser_used_->pre ();
    }
    else
      parser_used_ = 0; // No parser for this type.

    return parser_used_;
  }

  // This function is called to indicate the completion of document
  // parsing. The parser argument contains the pointer returned by
  // start_root_element.
  //
  virtual void
  end_root_element (const ro_string& /* ns */,
                    const ro_string& /* name */,
                    xml_schema::parser_base* /* parser */)
  {
    // Instead of caching the current parser in parser_used_, we
    // could also dynamic_cast the parser argument to the person_pskel
    // type.
    //
    if (parser_used_)
      parser_used_->post_person ();
  }


private:
  const xml_schema::parser_map& parser_map_;
  person_pskel* parser_used_;
};

int
main (int argc, char* argv[])
{
  if (argc != 2)
  {
    cerr << "usage: " << argv[0] << " instance.xml" << endl;
    return 1;
  }

  try
  {
    // Construct the parser.
    //
    xml_schema::string_pimpl string_p;
    xml_schema::boolean_pimpl boolean_p;
    xml_schema::unsigned_int_pimpl unsigned_int_p;

    person_pimpl person_p;
    superman_pimpl superman_p;
    batman_pimpl batman_p;

    person_p.parsers (string_p);
    superman_p.parsers (string_p, boolean_p);
    batman_p.parsers (string_p, boolean_p, unsigned_int_p);

    // Parse the XML document.
    //
    xml_schema::parser_map_impl person_map;

    person_map.insert (person_p);
    person_map.insert (superman_p);
    person_map.insert (batman_p);

    document doc_p (person_map);

    doc_p.parse (argv[1]);
  }
  catch (const xml_schema::exception& e)
  {
    cerr << e << endl;
    return 1;
  }
  catch (const std::ios_base::failure&)
  {
    cerr << argv[1] << ": unable to open or read failure" << endl;
    return 1;
  }
}
