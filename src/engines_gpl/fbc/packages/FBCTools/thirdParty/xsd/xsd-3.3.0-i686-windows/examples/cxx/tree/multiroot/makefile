root := ../../..

include $(root)/build/cxx/rules.make
include $(root)/build/xsd/tree-rules.make

override XSDFLAGS += --root-element-all


# Build.
#
driver: driver.o dom-parse.o protocol.o

protocol.o: protocol.cxx protocol.hxx
dom-parse.o: dom-parse.cxx dom-parse.hxx
driver.o: driver.cxx dom-parse.hxx protocol.hxx

protocol.cxx protocol.hxx: protocol.xsd


# Test
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
	rm -f protocol.o protocol.?xx dom-parse.o driver.o driver
