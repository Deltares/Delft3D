root := ../../../..

include $(root)/build/cxx/rules.make
include $(root)/build/xsd/tree-rules.make


override XSDFLAGS += --generate-inline --generate-serialization \
--extern-xml-schema xml-schema.xsd


# Build.
#
driver: driver.o people.o xml-schema-custom.o dom-parse.o

driver.o: driver.cxx dom-parse.hxx people.ixx people.hxx xml-schema.hxx xml-schema-custom.hxx
people.o: people.cxx people.ixx people.hxx xml-schema.hxx xml-schema-custom.hxx
xml-schema-custom.o: xml-schema.hxx xml-schema-custom.hxx
dom-parse.o: dom-parse.cxx dom-parse.hxx

people.cxx people.ixx people.hxx: people.xsd

# Generate header for XML Schema namespace.
#
XML_SCHEMA_XSDFLAGS := --generate-xml-schema --generate-serialization \
--custom-type anyType=/type_base \
--hxx-epilogue '\#include "xml-schema-custom.hxx"'

xml-schema.hxx:
	$(XSD) cxx-tree $(XML_SCHEMA_XSDFLAGS) xml-schema.xsd


# Test
#
.PHONY: test
test: driver people.xml
	./driver people.xml


# Clean.
#
.PHONY: clean
clean:
	rm -f xml-schema.hxx xml-schema-custom.o people.o people.?xx dom-parse.o driver.o driver
