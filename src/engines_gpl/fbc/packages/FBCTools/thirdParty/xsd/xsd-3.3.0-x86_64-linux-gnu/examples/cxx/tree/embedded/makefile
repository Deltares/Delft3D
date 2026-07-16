root := ../../..

include $(root)/build/cxx/rules.make
include $(root)/build/xsd/tree-rules.make


# Build.
#
driver: driver.o library.o library-schema.o grammar-input-stream.o
xsdbin: xsdbin.o

library.o: library.cxx library.hxx
driver.o: driver.cxx library.hxx library-schema.hxx
library-schema.o: library-schema.cxx library-schema.hxx
grammar-input-stream.o: grammar-input-stream.cxx grammar-input-stream.hxx

library.cxx library.hxx: library.xsd
library-schema.cxx library-schema.hxx: library.xsd

.PRECIOUS: %-schema.cxx %-schema.hxx
%-schema.cxx %-schema.hxx: %.xsd xsdbin
	./xsdbin $<

# Test
#
.PHONY: test
test: driver library.xml
	./driver library.xml


# Clean.
#
.PHONY: clean
clean:
	rm -f library.o library.?xx library-schema.?xx driver.o \
library-schema.o grammar-input-stream.o xsdbin.o xsdbin driver
