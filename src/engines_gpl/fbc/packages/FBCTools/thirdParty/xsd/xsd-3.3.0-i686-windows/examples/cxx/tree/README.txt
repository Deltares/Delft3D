This directory contains a number of examples that show how to use
the C++/Tree mapping. The following list gives an overview of
each example. See the README files in example directories for
more information on each example.

hello
  A simple "Hello, world!" example that shows how to parse XML
  documents.

library
  Shows hot to handle more complex data structures, use the
  ID/IDREF cross-referencing mechanism, use the xsd:enumeration
  to C++ enum mapping, modify the object model, and serialize
  the modified object model back to XML.

polymorphism
  Shows how to use XML Schema polymorphism features such as the
  xsi:type attribute and substitution groups.

xpath
  Shows how to use the C++/Tree mapping together with XPath.

wildcard
  Shows how to use the optional wildcard mapping to parse, access,
  modify, and serialize the XML data matched by XML Schema wildcards
  (any and anyAttribute).

mixed
  Shows how to access the underlying DOM nodes to handle raw, "type-
  less content" such as mixed content models, anyType/anySimpleType,
  and any/anyAttribute.

multiroot
  Shows how to handle XML vocabularies with multiple root elements.
  See also the messaging example.

messaging
  Shows how to handle XML vocabularies with multiple root elements
  using the element type and element map features of the C++/Tree
  mapping.

caching
  Shows how to parse several XML documents while reusing the
  underlying XML parser and caching the schemas used for validation.

embedded
  Shows how to embed the binary representation of the schema grammar
  into an application and then use it with the C++/Tree mapping to 
  parse and validate XML documents.

performance
  Measures the performance of parsing and serialization. This example
  also shows how to structure your code to achieve the maximum
  performance for these two operations.

custom/
  A collection of examples that show how to customize the C++/Tree
  mapping by using custom C++ classes instead of or in addition to
  the generated ones. See the accompanying README file for an
  overview of each example in this directory.

streaming
  Shows how to perform stream-oriented, partially in-memory XML
  processing using the C++/Tree mapping. With the partially in-memory
  parsing and serialization only a part of the object model is in
  memory at any given time. With this approach we can process parts
  of the document as they become available as well as handle documents
  that are too large to fit into memory.

compression
  Shows how to compress an XML document during serialization and decompress 
  it during parsing using the zlib library.

binary/
  A collection of examples that show how to serialize the object model
  into a number of predefined and custom binary formats.

dbxml
  Shows how to use the C++/Tree mapping on top of the Berkeley DB
  XML embedded XML database.
