root := ../../..

include $(root)/build/cxx/rules.make
include $(root)/build/xsd/tree-rules.make

override XSDFLAGS += --generate-polymorphic --generate-serialization --root-element-last


# Build.
#
driver: driver.o supermen.o

supermen.o: supermen.cxx supermen.hxx
driver.o: driver.cxx supermen.hxx

supermen.cxx supermen.hxx: supermen.xsd


# Test.
#
.PHONY: test
test: driver supermen.xml
	./driver supermen.xml


# Clean.
#
.PHONY: clean
clean:
	rm -f supermen.o supermen.?xx driver.o driver
