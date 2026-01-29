# file      : examples/build/cxx/rules.make
# author    : Boris Kolpackov <boris@codesynthesis.com>
# copyright : Copyright (c) 2006-2010 Code Synthesis Tools CC
# license   : GNU GPL v2 + exceptions; see accompanying LICENSE file

include $(root)/build/cxx/compilers.make


# GNU g++
#
ifeq ($(cxx_id),gnu)
  CXXFLAGS := -W -Wall -O3
endif


# Intel C++
#
ifeq ($(cxx_id),intel)
  CXXFLAGS := -w1 -O2
endif


# Sun C++
#
ifeq ($(cxx_id),sun)
  CXXFLAGS := -O
endif


# HP aCC3
#
ifeq ($(cxx_id),hp3)

  # By default Xerces-C++ is built with -mt for aCC3.
  # 849 : symbol to long, truncated to 4000
  # 1039: incompatible vtable layout
  #
  CXXFLAGS := -AA -O -mt +W849 +W1039
endif


# HP aCC6
#
ifeq ($(cxx_id),hp6)

  # By default Xerces-C++ is built with -mt for aCC6.
  # 2334 no suitable copy constructor (e.g., for std::auto_ptr)
  #
  CXXFLAGS := -Aa -O -mt +W2334
endif


# IBM XL C++
#
ifeq ($(cxx_id),ibm)
  CXXFLAGS := -qrtti -O
endif


# Rules.
#
%.o: %.cxx
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $< -o $@

%: %.o
	$(CXX) $(CXXFLAGS) $(LDFLAGS) -o $@ $^ $(LIBS)
