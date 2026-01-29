root := ../../..

include $(root)/build/cxx/rules.make
include $(root)/build/xsd/parser-rules.make


# Build.
#
driver: driver.o hello-pskel.o

driver.o: driver.cxx hello-pskel.hxx
hello-pskel.o: hello-pskel.cxx hello-pskel.hxx

hello-pskel.hxx hello-pskel.cxx: hello.xsd


# Test.
#
.PHONY: test
test: driver hello.xml
	./driver hello.xml


# Clean.
#
.PHONY: clean
clean:
	rm -f hello-pskel.?xx hello-pskel.o driver.o driver
