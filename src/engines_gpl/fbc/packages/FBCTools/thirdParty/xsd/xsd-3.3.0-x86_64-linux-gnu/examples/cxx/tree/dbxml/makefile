root := ../../..

include $(root)/build/cxx/rules.make
include $(root)/build/xsd/tree-rules.make


override XSDFLAGS += --generate-ostream --generate-serialization
override CPPFLAGS += -DDBXML_DOM
override LIBS += -ldbxml -lxqilla -ldb_cxx -ldb


# Build.
#
driver: driver.o library.o

library.o: library.cxx library.hxx
driver.o: driver.cxx library.hxx

library.cxx library.hxx: library.xsd


# Test
#
.PHONY: test
test: driver
	./driver


# Clean.
#
.PHONY: clean
clean:
	rm -f library.o library.?xx driver.o driver new.dbxml
