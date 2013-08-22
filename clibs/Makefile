include ../../config.mk

runtime_%.ml: %.c
	perl parseC.pl $*.c > $@

%.c:
	tar --strip-components=2 -zxf ../../$(OCAMLTAR) ocaml-$(OCAMLVERSION)/byterun/$*.c

other_unix.ml: otherlibs/unix
other_nums.ml: otherlibs/num
other_str.ml: otherlibs/str
other_threads.ml: otherlibs/systhreads
other_graphics.ml: otherlibs/graph
other_dbm.ml: otherlibs/dbm
other_bigarray.ml: otherlibs/bigarray

otherlibs/%: ../../$(OCAMLTAR)
	tar --strip-components=1 -zxf $< ocaml-$(OCAMLVERSION)/otherlibs/$*

other_%.ml:
	perl parseML.pl $(notdir $<) $* $</*.ml > $@

clean:
	rm -rf *.cm* *.o

distclean:
	rm -rf runtime_* other_* *.c otherlibs

.PHONY: clean distclean
