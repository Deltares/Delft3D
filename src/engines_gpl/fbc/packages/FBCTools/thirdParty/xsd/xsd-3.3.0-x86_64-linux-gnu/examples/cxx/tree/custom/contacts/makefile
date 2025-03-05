root := ../../../..

include $(root)/build/cxx/rules.make
include $(root)/build/xsd/tree-rules.make


override XSDFLAGS += --generate-inline --custom-type contact=/contact_base \
--hxx-epilogue '\#include "contacts-custom.hxx"'


# Build.
#
driver: driver.o contacts.o contacts-custom.o

contacts.o: contacts.cxx contacts.ixx contacts.hxx contacts-custom.hxx
driver.o: driver.cxx contacts.ixx contacts.hxx contacts-custom.hxx
contacts-custom.o: contacts-custom.cxx contacts.ixx contacts.hxx contacts-custom.hxx

contacts.cxx contacts.hxx contacts.ixx: contacts.xsd


# Test
#
.PHONY: test
test: driver contacts.xml
	./driver contacts.xml


# Clean.
#
.PHONY: clean
clean:
	rm -f contacts-custom.o contacts.o contacts.?xx driver.o driver
