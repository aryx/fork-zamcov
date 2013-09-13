TOP=..

# for ocaml version
-include $(TOP)/Makefile.config

##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC1= \
 other_bigarray.ml \
 other_nums.ml \
 other_str.ml \
 other_unix.ml \

# other_threads.ml \
# other_graphics.ml \

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

# other_dbm.ml \

# for Ffi
INCLUDEDIRS=$(TOP)/interpreter

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
.PHONY:: all all.opt

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

runtime_%.ml: %.c
	perl parseC.pl $*.c > $@

%.c:
	tar --strip-components=2 -zxf ../$(OCAMLTAR) ocaml-$(OCAMLVERSION)/byterun/$*.c

other_unix.ml: otherlibs/unix
other_nums.ml: otherlibs/num
other_str.ml: otherlibs/str
other_threads.ml: otherlibs/systhreads
other_graphics.ml: otherlibs/graph
other_dbm.ml: otherlibs/dbm
other_bigarray.ml: otherlibs/bigarray

otherlibs/%: ../$(OCAMLTAR)
	tar --strip-components=1 -zxf $< ocaml-$(OCAMLVERSION)/otherlibs/$*

other_%.ml:
	perl parseML.pl $(notdir $<) $* $</*.ml > $@

distclean::
	rm -rf runtime_* other_* *.c otherlibs

