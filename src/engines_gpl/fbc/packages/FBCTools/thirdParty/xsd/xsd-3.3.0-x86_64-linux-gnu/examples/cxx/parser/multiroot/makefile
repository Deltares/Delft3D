root := ../../..

include $(root)/build/cxx/rules.make
include $(root)/build/xsd/parser-rules.make

override XSDFLAGS += --type-map protocol.map

# Build
#
driver: driver.o protocol-pskel.o protocol-pimpl.o

driver.o: driver.cxx protocol-pimpl.hxx protocol-pskel.hxx protocol.hxx
protocol-pimpl.o: protocol-pimpl.cxx protocol-pimpl.hxx protocol-pskel.hxx protocol.hxx
protocol-pskel.o: protocol-pskel.cxx protocol-pskel.hxx protocol.hxx

protocol-pskel.hxx protocol-pskel.cxx: protocol.xsd protocol.map


# Test.
#
.PHONY: test
test: driver balance.xml withdraw.xml deposit.xml
	./driver balance.xml
	./driver withdraw.xml
	./driver deposit.xml


# Clean.
#
.PHONY: clean
clean:
	rm -f protocol-pskel.?xx protocol-pimpl.o protocol-pskel.o driver.o driver
