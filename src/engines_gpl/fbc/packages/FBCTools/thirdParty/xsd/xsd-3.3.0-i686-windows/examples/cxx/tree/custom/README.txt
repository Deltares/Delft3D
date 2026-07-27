This directory contains a number of examples that show how to customize the
C++/Tree mapping. The following list gives an overview of each example:

calendar
  Shows how to customize XML Schema built-in types by mapping xsd:date
  built-in type to the date class from the Boost date_time library.

comments
  This example shows how to customize the anyType XML Schema built-in
  type to implement preservation of comments stored in XML documents.
  Because anyType is a base type for every generated type, you can use
  this technique to implement custom functionality that spans the
  entire type system.

contacts
  Shows how to map a user-defined XML Schema type to a custom C++ class.
  This example presents the simple case where the customized type is not
  used as a base in the same schema.

double
  Shows how to customize parsing and serialization code for the xsd:double
  XML Schema built-in type. It can be used as a guide on how to customize
  built-in XML Schema types that are mapped to fundamental C++ types.

mixed
  Shows how to use type customization to parse and serialize mixed content.

taxonomy
  Shows how to map user-defined XML Schema types to custom C++ classes.
  This example presents the complex case where the customized types are
  inherited from in the same schema.

wildcard
  Shows how to use type customization to parse and serialize a specific
  attribute that is matched by a wildcard (anyAttribute).

For more information on the C++/Tree mapping customization see the
C++/Tree Mapping Customization Guide[1].

[1] http://wiki.codesynthesis.com/Tree/Customization_guide
