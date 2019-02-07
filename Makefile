## Makefile
default: edit
edit: edit.native

## test: test.native

%.native:
	corebuild -tag thread -use-ocamlfind -pkg core -pkg lwt -pkg lambda-term -quiet src/$@
	mv $@ $*


clean:
	-rm -rf _build edit

.PHONY: default
