root := ../../..

include $(root)/build/cxx/rules.make
include $(root)/build/xsd/parser-rules.make

override XSDFLAGS += --generate-inline --type-map library.map

# Build
#
driver: driver.o library-pskel.o library-pimpl.o

driver.o: driver.cxx library-pimpl.hxx library-pskel.hxx library.hxx
library-pimpl.o: library-pimpl.cxx library-pimpl.hxx library-pskel.hxx library.hxx
library-pskel.o: library-pskel.cxx library-pskel.hxx library.hxx

library-pskel.hxx library-pskel.ixx library-pskel.cxx: library.xsd library.map


# Test.
#
.PHONY: test
test: driver library.xml
	./driver library.xml


# Clean.
#
.PHONY: clean
clean:
	rm -f library-pskel.?xx library-pimpl.o library-pskel.o driver.o driver
