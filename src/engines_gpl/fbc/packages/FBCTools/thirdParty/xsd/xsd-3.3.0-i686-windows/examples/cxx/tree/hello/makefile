root := ../../..

include $(root)/build/cxx/rules.make
include $(root)/build/xsd/tree-rules.make


# Build.
#
driver: driver.o hello.o

hello.o: hello.cxx hello.hxx
driver.o: driver.cxx hello.hxx

hello.cxx hello.hxx: hello.xsd


# Test.
#
.PHONY: test
test: driver hello.xml
	./driver hello.xml


# Clean.
#
.PHONY: clean
clean:
	rm -f hello.o hello.?xx driver.o driver

