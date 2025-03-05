root := ../../..

include $(root)/build/cxx/rules.make
include $(root)/build/xsd/tree-rules.make


# Build.
#
driver: driver.o text.o

text.o: text.cxx text.hxx
driver.o: driver.cxx text.hxx

text.cxx text.hxx: text.xsd


# Test.
#
.PHONY: test
test: driver text.xml
	./driver text.xml


# Clean.
#
.PHONY: clean
clean:
	rm -f text.o text.?xx driver.o driver
