root := ../../..

include $(root)/build/cxx/rules.make
include $(root)/build/xsd/tree-rules.make


override XSDFLAGS += --generate-inline --generate-ostream --generate-serialization


# Build.
#
driver: driver.o library.o

library.o: library.cxx library.hxx library.ixx
driver.o: driver.cxx library.hxx library.ixx

library.cxx library.hxx library.ixx: library.xsd


# Test
#
.PHONY: test
test: driver library.xml
	./driver library.xml


# Clean.
#
.PHONY: clean
clean:
	rm -f library.o library.?xx driver.o driver

