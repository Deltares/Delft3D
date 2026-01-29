root := ../../../..

include $(root)/build/cxx/rules.make
include $(root)/build/xsd/tree-rules.make


override XSDFLAGS += --generate-inline --generate-forward \
--generate-polymorphic \
--polymorphic-type person \
--custom-type "person=person_impl<person_base>/person_base" \
--custom-type "superman=superman_impl<superman_base>/superman_base" \
--custom-type "batman=batman_impl<batman_base>/batman_base" \
--fwd-prologue '\#include "people-custom-fwd.hxx"' \
--hxx-prologue '\#include "people-custom.hxx"'


# Build.
#
driver: driver.o people.o people-custom.o

people.o: people.cxx people.ixx people.hxx people-custom.hxx people-custom-fwd.hxx
driver.o: driver.cxx people.ixx people.hxx people-custom.hxx people-custom-fwd.hxx
people-custom.o: people-custom.cxx people.ixx people.hxx people-custom.hxx people-custom-fwd.hxx

people.cxx people.ixx people.hxx people-fwd.hxx: people.xsd


# Test
#
.PHONY: test
test: driver people.xml
	./driver people.xml


# Clean.
#
.PHONY: clean
clean:
	rm -f people-custom.o people.o people-fwd.hxx people.?xx driver.o driver
