root := ../../..

include $(root)/build/cxx/rules.make
include $(root)/build/xsd/tree-rules.make


override XSDFLAGS += --generate-inline --generate-serialization \
--generate-wildcard --root-element message


# Build.
#
driver: driver.o email.o

email.o: email.cxx email.hxx email.ixx
driver.o: driver.cxx email.hxx email.ixx

email.cxx email.hxx email.ixx: email.xsd


# Test
#
.PHONY: test
test: driver email.xml
	./driver email.xml


# Clean.
#
.PHONY: clean
clean:
	rm -f email.o email.?xx driver.o driver
