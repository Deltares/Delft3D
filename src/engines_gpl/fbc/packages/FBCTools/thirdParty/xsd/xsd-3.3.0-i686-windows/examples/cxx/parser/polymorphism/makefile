root := ../../..

include $(root)/build/cxx/rules.make
include $(root)/build/xsd/parser-rules.make

override XSDFLAGS += --generate-polymorphic

# Build
#
driver: driver.o supermen-pskel.o supermen-pimpl.o

driver.o: driver.cxx supermen-pimpl.hxx supermen-pskel.hxx
supermen-pimpl.o: supermen-pimpl.cxx supermen-pimpl.hxx supermen-pskel.hxx
supermen-pskel.o: supermen-pskel.cxx supermen-pskel.hxx

supermen-pskel.hxx supermen-pskel.cxx: supermen.xsd


# Test.
#
.PHONY: test
test: driver supermen.xml
	./driver supermen.xml


# Clean.
#
.PHONY: clean
clean:
	rm -f supermen-pskel.?xx supermen-pimpl.o supermen-pskel.o driver.o driver
