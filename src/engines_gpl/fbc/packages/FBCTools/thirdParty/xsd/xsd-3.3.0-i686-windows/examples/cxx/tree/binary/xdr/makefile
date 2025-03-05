root := ../../../..

include $(root)/build/cxx/rules.make
include $(root)/build/xsd/tree-rules.make


override XSDFLAGS += --generate-ostream --generate-insertion XDR \
--generate-extraction XDR


override LIBS += -lnsl


# Build.
#
driver: driver.o library.o

library.o: library.cxx library.hxx
driver.o: driver.cxx library.hxx

library.cxx library.hxx: library.xsd


# Test
#
.PHONY: test
test: driver library.xml
	./driver library.xml


# Clean.
#
.PHONY: clean
clean:
	rm -f library.o library.?xx driver.o driver
