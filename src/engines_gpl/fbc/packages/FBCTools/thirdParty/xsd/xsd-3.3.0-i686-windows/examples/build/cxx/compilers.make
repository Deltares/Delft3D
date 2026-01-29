# file      : examples/build/cxx/compilers.make
# author    : Boris Kolpackov <boris@codesynthesis.com>
# copyright : Copyright (c) 2006-2010 Code Synthesis Tools CC
# license   : GNU GPL v2 + exceptions; see accompanying LICENSE file

CXX := g++

cxx_sign := $(shell t=`$(CXX) -V 2>&1`; if test $$? -eq 0; then echo $$t; fi)

ifeq ($(cxx_sign),)
cxx_sign := $(shell t=`$(CXX) --version 2>&1`; if test $$? -eq 0; then echo $$t; fi)
endif

# IBM XL C++ V7.0 returns error code when called with the -qversion option. This
# complicates our life quite a bit.
#
ifeq ($(cxx_sign),)
cxx_sign := $(shell t=`$(CXX) -qversion 2>/dev/null`; echo $$t)
ifneq ($(shell echo '$(cxx_sign)' | sed -e 's/^.*IBM XL C\/C.. .*$$//'),)
cxx_sign :=
endif
endif

cxx_id :=

ifneq ($(cxx_sign),)

# GNU g++ (g++)
#
ifeq ($(cxx_id),)
ifeq ($(shell echo '$(cxx_sign)' | sed -e 's/^[^ ]* (GCC) .*$$//'),)
cxx_id := gnu
endif
endif

# g++ 4.3 removed GCC for some reason so check for g++ also.
ifeq ($(cxx_id),)
ifeq ($(shell echo '$(cxx_sign)' | sed -e 's/^g++.*$$//'),)
cxx_id := gnu
endif
endif

# Sun C++ (CC)
#
ifeq ($(cxx_id),)
ifeq ($(shell echo '$(cxx_sign)' | sed -e 's/^[^ ]* Sun C.. .*$$//'),)
cxx_id := sun
endif
endif


# HP C++ (aCC)
#
# aCC3 and aCC6 are two very different compilers so we will call them
# hp3 and hp6.
#

# 3
ifeq ($(cxx_id),)
ifeq ($(shell echo '$(cxx_sign)' | sed -e 's/^[^ ]* HP ANSI C.. .* A\.03\..*$$//'),)
cxx_id := hp3
endif
endif

# 6
ifeq ($(cxx_id),)
ifeq ($(shell echo '$(cxx_sign)' | sed -e 's/^[^ ]* HP aC..\/ANSI C .* A\.06\..*$$//'),)
cxx_id := hp6
endif
endif

ifeq ($(cxx_id),)
ifeq ($(shell echo '$(cxx_sign)' | sed -e 's/^[^ ]* HP C\/aC.. .* A\.06\..*$$//'),)
cxx_id := hp6
endif
endif

# Intel C++ (icpc)
#

# 9.x
ifeq ($(cxx_id),)
ifeq ($(shell echo '$(cxx_sign)' | sed -e 's/^Intel(R) C.. .*$$//'),)
cxx_id := intel
endif
endif

# 8.x
ifeq ($(cxx_id),)
ifeq ($(shell echo '$(cxx_sign)' | sed -e 's/^8\..$$//'),)
cxx_id := intel
endif
endif


# IBM XL C++
#
ifeq ($(cxx_id),)
ifeq ($(shell echo '$(cxx_sign)' | sed -e 's/^.*IBM XL C\/C.. .*$$//'),)
cxx_id := ibm
endif
endif


# Unknown
#
ifeq ($(cxx_id),)
$(warning unknown C++ compiler signature '$(cxx_sign)', continuing anyway)
endif


else
$(warning unable to obtain compiler signature for '$(CXX)', continuing anyway)
endif

#$(warning $(cxx_sign))
#$(warning $(cxx_id))
