# file      : examples/build/xsd/parser-rules.make
# author    : Boris Kolpackov <boris@codesynthesis.com>
# copyright : Copyright (c) 2006-2010 Code Synthesis Tools CC
# license   : GNU GPL v2 + exceptions; see accompanying LICENSE file

include $(root)/build/xsd/common.make

XML_PARSER := xerces

ifeq ($(XML_PARSER),xerces)
override LIBS := -lxerces-c $(LIBS)
else
override LIBS := -lexpat $(LIBS)
endif

override XSDFLAGS += --xml-parser $(XML_PARSER)

# Rules.
#
.PRECIOUS: %-pskel.hxx %-pskel.ixx %-pskel.cxx

%-pskel.hxx %-pskel.ixx %-pskel.cxx: %.xsd
	$(XSD) cxx-parser $(XSDFLAGS) $<
