include ../../support/include.mk
.PHONY: all clean compile_plugin

all: compile_plugin
	cp plugin_src/ebin/* plugin/

compile_plugin: 
	$(MAKE) -C plugin_src

clean:
	-rm -rf plugin/*
	echo README > plugin/README
	$(MAKE) -C plugin_src $@

install:
	$(INSTALL) -d ${DESTDIR}/usr/local/bnow/apps/sdk
	cp -r app.xml ${DESTDIR}/usr/local/bnow/apps/sdk/
	cp -r plugin ${DESTDIR}/usr/local/bnow/apps/sdk/
	cp -r web ${DESTDIR}/usr/local/bnow/apps/sdk/
