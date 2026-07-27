# file      : examples/build/xsd/tree-rules.make
# author    : Boris Kolpackov <boris@codesynthesis.com>
# copyright : Copyright (c) 2006-2010 Code Synthesis Tools CC
# license   : GNU GPL v2 + exceptions; see accompanying LICENSE file

include $(root)/build/xsd/common.make

override LIBS := -lxerces-c $(LIBS)

# Rules.
#
.PRECIOUS: %.cxx %.hxx %.ixx

%.cxx %.hxx %.ixx: %.xsd
	$(XSD) cxx-tree $(XSDFLAGS) $<
