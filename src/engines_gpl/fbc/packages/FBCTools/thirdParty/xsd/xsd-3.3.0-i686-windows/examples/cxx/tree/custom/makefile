dirs := comments contacts double mixed taxonomy wildcard

ifeq ($(WITH_BOOST),1)
dirs += calendar
endif

.PHONY: all $(dirs)

all: $(dirs)

$(dirs):
	@$(MAKE) -C $@ $(MAKECMDGOALS)

makefile: ;
% :: $(dirs) ;
