root := ../../../..

include $(root)/build/cxx/rules.make
include $(root)/build/xsd/tree-rules.make


override XSDFLAGS += --generate-inline --generate-serialization \
--extern-xml-schema xml-schema.xsd


# Build.
#
driver: driver.o order.o double-custom.o

driver.o: driver.cxx order.ixx order.hxx xml-schema.hxx double-custom.hxx
order.o: order.cxx order.ixx order.hxx xml-schema.hxx double-custom.hxx
double-custom.o: xml-schema.hxx double-custom.hxx

order.cxx order.ixx order.hxx: order.xsd

# Generate header for XML Schema namespace.
#
XML_SCHEMA_XSDFLAGS := --generate-xml-schema --generate-serialization \
--custom-type double=double \
--hxx-epilogue '\#include "double-custom.hxx"'

xml-schema.hxx:
	$(XSD) cxx-tree $(XML_SCHEMA_XSDFLAGS) xml-schema.xsd


# Test
#
.PHONY: test
test: driver
	./driver


# Clean.
#
.PHONY: clean
clean:
	rm -f xml-schema.hxx double-custom.o order.o order.?xx driver.o driver
