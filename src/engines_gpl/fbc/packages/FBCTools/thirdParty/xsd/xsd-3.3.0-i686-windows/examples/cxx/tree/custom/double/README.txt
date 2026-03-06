This example shows how to customize parsing and serialization code for the
xsd:double XML Schema built-in type using the type customization mechanism
provided by the C++/Tree Mapping. For more information on type customization
see the C++/Tree Mapping Customization Guide, particularly sections 1 and 4:

http://wiki.codesynthesis.com/Tree/Customization_guide

In this example our schema uses xsd:double to represent a price. There are
two potential problems with this choice of a price type. First, xsd:double
can be serialized in the scientific notation which would be an unusual way
of representing a price. Second, we would like to limit the number of 
fraction digits in our prices to 2. Furthermore, we would like to always
have two fraction digits, even if one or both of them are zeros, for
example: 12.99, 12.90, 12.00.

In case we can modify the schema, a better approach would be to define the
price type as a restriction of the xsd:decimal type (always fixed notation)
and specify the fractionDigits facet to limit the number of fraction digits
to 2. However, there is no way in XML Schema to specify that there should
always be exactly 2 fraction digits. Therefore, it may still be desirable
to customize this price type to get the required serialization behavior.

Finally, it is worth noting that the behavior achieved in this example via
type customization can also be achieved by compiling your code with the
following macros defined:

XSD_TREE_DOUBLE_FIXED
XSD_TREE_DOUBLE_PRECISION 2

However, the type customization approach while requiring more work is
cleaner since it does not rely on global macro definitions.

This example consists of the following files:

order.xsd
  XML Schema definition for a simple order vocabulary.

double-custom.hxx
double-custom.cxx
  Custom parsing and serialization code for the xsd:double types. The
  double-custom.hxx file is included at the end of the xml-schema.hxx
  file described below.

xml-schema.hxx
  C++ types for XML Schema built-in types. This header file is generated
  by the XSD compiler using the --generate-xml-schema option. The
  --custom-type option is used to customize the xsd:double type. The 
  --hxx-epilogue option is used to include the double-custom.hxx file
  at the end of this file.

order.hxx
order.cxx
  C++ types generated from order.xsd. The --extern-xml-schema option
  is used to include xml-schema.hxx into order.hxx.

driver.cxx
  Test driver for the example. It creates a sample order and then
  writes it to XML to test the custom xsd:double serialization code.

To run the example simply execute:

$ ./driver
