root := ../../../..

include $(root)/build/cxx/rules.make
include $(root)/build/xsd/tree-rules.make


override XSDFLAGS += --generate-inline --extern-xml-schema xml-schema.xsd

override LIBS += -lboost_date_time


# Build.
#
driver: driver.o calendar.o xml-schema-custom.o

driver.o: driver.cxx calendar.ixx calendar.hxx xml-schema.hxx xml-schema-custom.hxx
calendar.o: calendar.cxx calendar.ixx calendar.hxx xml-schema.hxx xml-schema-custom.hxx
xml-schema-custom.o: xml-schema.hxx xml-schema-custom.hxx

calendar.cxx calendar.ixx calendar.hxx: calendar.xsd

# Generate header for XML Schema namespace.
#
XML_SCHEMA_XSDFLAGS := --generate-xml-schema --custom-type date \
--hxx-epilogue '\#include "xml-schema-custom.hxx"'

xml-schema.hxx:
	$(XSD) cxx-tree $(XML_SCHEMA_XSDFLAGS) xml-schema.xsd


# Test
#
.PHONY: test
test: driver calendar.xml
	./driver calendar.xml


# Clean.
#
.PHONY: clean
clean:
	rm -f xml-schema.hxx xml-schema-custom.o calendar.o calendar.?xx driver.o driver
