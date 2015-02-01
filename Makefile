%.byte %.native: %.ml
	corebuild -pkg cohttp.async,yojson $@

all: refresher.byte
