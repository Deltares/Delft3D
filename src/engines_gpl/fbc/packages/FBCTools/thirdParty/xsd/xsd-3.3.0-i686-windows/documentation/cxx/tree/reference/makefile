.PHONY: all
all: libxsd.doxytag

headers :=                                     \
../../../../libxsd/xsd/cxx/tree/buffer.hxx     \
../../../../libxsd/xsd/cxx/tree/types.hxx      \
../../../../libxsd/xsd/cxx/tree/date-time.hxx  \
../../../../libxsd/xsd/cxx/tree/elements.hxx   \
../../../../libxsd/xsd/cxx/tree/exceptions.hxx

libxsd.doxytag: libxsd.doxygen footer.html $(headers)
	doxygen $<

.PHONY: clean
clean:
	rm -f libxsd.doxytag
	rm -rf html

