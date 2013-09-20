TOP=..

# for $(OCAMLVERSION)
-include $(TOP)/Makefile.config

##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC1= \
 other_bigarray.ml \
 other_num.ml \
 other_str.ml \
 other_unix.ml \
 other_systhreads.ml \

# other_graphics.ml \
# other_dbm.ml \

SRC2= \
 runtime_alloc.ml \
 runtime_array.ml \
 runtime_backtrace.ml \
 runtime_callback.ml \
 runtime_compare.ml \
 runtime_dynlink.ml \
 runtime_extern.ml \
 runtime_finalise.ml \
 runtime_floats.ml \
 runtime_gc_ctrl.ml \
 runtime_hash.ml \
 runtime_intern.ml \
 runtime_ints.ml \
 runtime_io.ml \
 runtime_lexing.ml \
 runtime_md5.ml \
 runtime_meta.ml \
 runtime_obj.ml \
 runtime_parsing.ml \
 runtime_signals.ml \
 runtime_stacks.ml \
 runtime_str.ml \
 runtime_sys.ml \
 runtime_terminfo.ml \
 runtime_weak.ml \

# for Ffi
INCLUDEDIRS=$(TOP)/interpreter $(TOP)/commons

# to prevent the automatic deletion done by make
.SECONDARY: $(SRC2)

SRC=$(SRC1) $(SRC2) extra.ml

##############################################################################
# Generic variables
##############################################################################

-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################

# need that otherwise get some 'tar  ... all.c'
.PHONY:: all all.opt depend .depend

all:: $(TARGET).cma
all.opt:: $(TARGET).cmxa

# need -linkall, otherwise the Ffi.init_list modifs are not done
# because the module appears unreferenced and so is not linked
$(TARGET).cma: $(OBJS)
	$(OCAMLC) -linkall -a -o $(TARGET).cma $(OBJS)

$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -linkall -a -o $(TARGET).cmxa $(OPTOBJS)

##############################################################################
# Adhoc rules
##############################################################################

# byterun stdlib
runtime_%.ml: %.c runtime
	perl parseC.pl $*.c > $@

%.c:
	tar --strip-components=2 -zxf ../$(OCAMLTAR) ocaml-$(OCAMLVERSION)/byterun/$*.c

# otherlibs
other_%.ml: big/% %
	perl parseML.pl $(notdir $<) $* $</*.ml > $@
	rm -rf big/$*

big/%: ../$(OCAMLTAR)
	rm -rf big/$*
	tar --strip-components=1 -zxf $< ocaml-$(OCAMLVERSION)/otherlibs/$*
	mkdir -p big
	mv otherlibs/$* big/

distclean::
	rm -rf runtime_* other_* *.c otherlibs big/*

