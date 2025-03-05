root := ../../..

include $(root)/build/cxx/rules.make
include $(root)/build/xsd/parser-rules.make

override XSDFLAGS += --generate-print-impl --generate-test-driver

# If you are basing your own code on this example and make changes
# to the sample implementation and/or test driver, make sure you
# remove this option or your changes will be lost forever.
#
override XSDFLAGS += --force-overwrite

# Build
#
library-driver: library-driver.o library-pskel.o library-pimpl.o

library-driver.o: library-driver.cxx library-pimpl.hxx library-pskel.hxx
library-pimpl.o: library-pimpl.cxx library-pimpl.hxx library-pskel.hxx
library-pskel.o: library-pskel.cxx library-pskel.hxx

library-pskel.hxx  \
library-pskel.cxx  \
library-pimpl.hxx  \
library-pimpl.cxx  \
library-driver.cxx : library.xsd

# Change the pattern rule from parser-rules.make to include the sample
# implementation and test driver.
#
.PRECIOUS: %-pskel.hxx %-pskel.cxx %-pimpl.hxx %-pimpl.cxx %-driver.cxx

%-pskel.hxx %-pskel.cxx %-pimpl.hxx %-pimpl.cxx %-driver.cxx: %.xsd
	$(XSD) cxx-parser $(XSDFLAGS) $<


# Test.
#
.PHONY: test
test: library-driver library.xml
	./library-driver library.xml


# Clean.
#
.PHONY: clean
clean:
	rm -f library-driver.?xx library-pimpl.?xx library-pskel.?xx
	rm -f library-driver.o library-pimpl.o library-pskel.o library-driver
