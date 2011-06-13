FILES=*.js *.scm *.png *.html

all: cache.manifest apple-touch-icon.png

cache.manifest: ${FILES} Makefile
	echo CACHE MANIFEST > cache.new
	echo "# Serial number " `sed "/Serial number/s/.* \([0-9]*\)/0\1 + 1/p;d" cache.manifest | bc` >> cache.new
	ls ${FILES} >> cache.new
	mv cache.new cache.manifest

apple-touch-icon.png: icon.svg
	inkscape -e $@ $<
