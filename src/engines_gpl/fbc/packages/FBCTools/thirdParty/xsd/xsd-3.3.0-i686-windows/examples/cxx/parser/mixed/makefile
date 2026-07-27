root := ../../..

include $(root)/build/cxx/rules.make
include $(root)/build/xsd/parser-rules.make

override XSDFLAGS += --type-map text.map

# Build.
#
driver: driver.o text-pskel.o

driver.o: driver.cxx text-pskel.hxx anchor.hxx
text-pskel.o: text-pskel.cxx text-pskel.hxx anchor.hxx

text-pskel.hxx text-pskel.cxx: text.xsd text.map


# Test.
#
.PHONY: test
test: driver text.xml
	./driver text.xml


# Clean.
#
.PHONY: clean
clean:
	rm -f text-pskel.?xx text-pskel.o driver.o driver

