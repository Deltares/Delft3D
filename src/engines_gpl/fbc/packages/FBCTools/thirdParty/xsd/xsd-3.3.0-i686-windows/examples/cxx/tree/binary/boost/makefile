root := ../../../..

include $(root)/build/cxx/rules.make
include $(root)/build/xsd/tree-rules.make


override XSDFLAGS += --generate-ostream             \
--hxx-prologue-file library-prologue.hxx            \
--generate-insertion boost::archive::text_oarchive  \
--generate-extraction boost::archive::text_iarchive


override LIBS += -lboost_serialization


# Build.
#
driver: driver.o library.o

library.o: library.cxx library.hxx boost-archive-insertion.hxx boost-archive-extraction.hxx
driver.o: driver.cxx library.hxx boost-archive-insertion.hxx boost-archive-extraction.hxx

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
