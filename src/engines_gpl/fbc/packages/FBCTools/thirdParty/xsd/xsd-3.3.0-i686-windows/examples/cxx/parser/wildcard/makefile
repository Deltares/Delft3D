root := ../../..

include $(root)/build/cxx/rules.make
include $(root)/build/xsd/parser-rules.make


# Build.
#
driver: driver.o email-pskel.o

driver.o: driver.cxx email-pskel.hxx
email-pskel.o: email-pskel.cxx email-pskel.hxx

email-pskel.hxx email-pskel.cxx: email.xsd


# Test.
#
.PHONY: test
test: driver email.xml
	./driver email.xml


# Clean.
#
.PHONY: clean
clean:
	rm -f email-pskel.?xx email-pskel.o driver.o driver
