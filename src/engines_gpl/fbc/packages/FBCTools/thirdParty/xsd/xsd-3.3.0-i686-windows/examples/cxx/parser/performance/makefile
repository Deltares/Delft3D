root := ../../..

include $(root)/build/cxx/rules.make
include $(root)/build/xsd/parser-rules.make

override XSDFLAGS += --generate-inline

# Build.
#
driver: driver.o test-pskel.o time.o

driver.o: driver.cxx test-pskel.hxx test-pskel.ixx time.hxx
test-pskel.o: test-pskel.cxx test-pskel.hxx test-pskel.ixx
time.o: time.cxx time.hxx

test-pskel.hxx test-pskel.ixx test-pskel.cxx: test.xsd


# Test.
#
.PHONY: test
test: driver test-50k.xml
	./driver test-50k.xml


# Clean.
#
.PHONY: clean
clean:
	rm -f test-pskel.?xx test-pskel.o driver.o time.o driver
