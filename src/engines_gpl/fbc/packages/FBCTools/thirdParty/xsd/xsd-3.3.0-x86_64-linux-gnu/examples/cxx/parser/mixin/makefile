root := ../../..

include $(root)/build/cxx/rules.make
include $(root)/build/xsd/parser-rules.make

override XSDFLAGS += --type-map schema.map


# Build.
#
driver: driver.o schema-pskel.o

driver.o: driver.cxx schema-pskel.hxx types.hxx
schema-pskel.o: schema-pskel.cxx schema-pskel.hxx types.hxx

schema-pskel.hxx schema-pskel.cxx: schema.xsd schema.map

# Test.
#
.PHONY: test
test: driver instance.xml
	./driver instance.xml


# Clean.
#
.PHONY: clean
clean:
	rm -f schema-pskel.?xx schema-pskel.o driver.o driver

