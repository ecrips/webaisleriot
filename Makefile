FILES=*.js *.scm *.png *.html

VERSION=$(shell ./mkversion)

all: cache.manifest apple-touch-icon.png
	@echo $(VERSION)

dist: webaisleriot-$(VERSION).tar.gz

cache.manifest: ${FILES} Makefile
	echo CACHE MANIFEST > cache.new
	md5sum ${FILES} | sed "s/\(.*\) \(.*\)/\2\n# \1/" >> cache.new
	mv cache.new cache.manifest

apple-touch-icon.png: icon.svg
	inkscape -e $@ $<

%.tar.gz: $(FILES) *.svg Makefile
	tar -czf $@ $+
