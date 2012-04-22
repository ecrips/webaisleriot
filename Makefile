FILES=*.js *.scm *.png *.html

all: cache.manifest apple-touch-icon.png

cache.manifest: ${FILES} Makefile
	echo CACHE MANIFEST > cache.new
	md5sum ${FILES} | sed "s/\(.*\) \(.*\)/\2 # \1/" >> cache.new
	mv cache.new cache.manifest

apple-touch-icon.png: icon.svg
	inkscape -e $@ $<
