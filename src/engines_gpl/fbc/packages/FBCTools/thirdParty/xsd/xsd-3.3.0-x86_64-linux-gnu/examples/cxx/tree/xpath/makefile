root := ../../..

include $(root)/build/cxx/rules.make
include $(root)/build/xsd/tree-rules.make

override LIBS += -lxqilla

# Build.
#
driver: driver.o people.o dom-parse.o

people.o: people.cxx people.hxx
driver.o: driver.cxx people.hxx dom-parse.hxx
people.o: dom-parse.cxx dom-parse.hxx

people.cxx people.hxx: people.xsd


# Test
#
.PHONY: test
test: driver people.xml
	./driver people.xml


# Clean.
#
.PHONY: clean
clean:
	rm -f people.o people.?xx driver.o dom-parse.o driver
