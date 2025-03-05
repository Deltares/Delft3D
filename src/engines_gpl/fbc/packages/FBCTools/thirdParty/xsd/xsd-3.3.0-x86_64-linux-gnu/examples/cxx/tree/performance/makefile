root := ../../..

include $(root)/build/cxx/rules.make
include $(root)/build/xsd/tree-rules.make


override XSDFLAGS += --generate-inline --generate-serialization


# Build.
#
driver: driver.o parsing.o serialization.o test.o time.o

driver.o: driver.cxx
test.o: test.cxx test.hxx test.ixx
parsing.o: parsing.cxx test.hxx test.ixx time.hxx
serialization.o: serialization.cxx test.hxx test.ixx time.hxx
time.o: time.cxx time.hxx

test.cxx test.hxx test.ixx: test.xsd


# Test
#
.PHONY: test
test: driver test-50k.xml
	./driver test-50k.xml


# Clean.
#
.PHONY: clean
clean:
	rm -f test.o test.?xx driver.o parsing.o serialization.o time.o driver
