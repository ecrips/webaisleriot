FILES=$(filter-out sw.js,$(wildcard *.js)) $(wildcard *.scm *.png *.html)

VERSION=$(shell ./mkversion)

all: sw.js cache.manifest apple-touch-icon.png
	@echo $(VERSION)

sw.js: ${FILES} Makefile sw.js.in
	ls ${FILES} | sed "s/.*/  '.\/&',/" > assets.tmp
	sed -e "s/@VERSION@/$(VERSION)/" -e "/@ASSET_LIST@/r assets.tmp" -e "/@ASSET_LIST@/d" sw.js.in > sw.js
	rm assets.tmp

cache.manifest: ${FILES} Makefile
	echo CACHE MANIFEST > cache.new
	md5sum ${FILES} | sed "s/\(.*\) \(.*\)/\2\n# \1/" >> cache.new
	mv cache.new cache.manifest

apple-touch-icon.png: icon.svg
	inkscape -o $@ $<

clean:
	rm -f sw.js assets.tmp cache.manifest
