root := ../../..

include $(root)/build/cxx/rules.make
include $(root)/build/xsd/tree-rules.make


override XSDFLAGS += --generate-ostream --generate-serialization

override LIBS += -lz

# Build.
#
driver: driver.o library.o compressed-format-target.o compressed-input-source.o

library.o: library.cxx library.hxx
driver.o: driver.cxx library.hxx compressed-format-target.hxx \
compressed-input-source.hxx
compressed-format-target.o: compressed-format-target.cxx
compressed-input-source.o: compressed-input-source.cxx

library.cxx library.hxx: library.xsd


# Test
#
.PHONY: test
test: driver library.xml.gz
	./driver library.xml.gz


# Clean.
#
.PHONY: clean
clean:
	rm -f library.o library.?xx driver.o compressed-format-target.o \
compressed-input-source.o driver out.xml.gz

