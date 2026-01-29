dirs :=

ifeq ($(WITH_BOOST),1)
dirs += boost
endif


ifeq ($(WITH_ACE),1)
dirs += cdr
endif

ifeq ($(WITH_XDR),1)
dirs += xdr
endif


.PHONY: all $(dirs)

all: $(dirs)

$(dirs):
	@$(MAKE) -C $@ $(MAKECMDGOALS)

makefile: ;
% :: $(dirs) ;
