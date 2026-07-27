dirs := cxx

.PHONY: all $(dirs)

all: $(dirs)

$(dirs):
	$(MAKE) -C $@ $(MAKECMDGOALS)

makefile: ;
% :: $(dirs) ;
