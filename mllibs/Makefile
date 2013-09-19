TOP=..
##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC1= \
 lib_bigarray.ml \
 lib_dbm.ml \
 lib_nums.ml \
 lib_str.ml \
 lib_unix.ml \
 lib_threads.ml \

# lib_graphics.ml \

SRC2= \
 run_alloc.ml \
 run_array.ml \
 run_backtrace.ml \
 run_callback.ml \
 run_compare.ml \
 run_dynlink.ml \
 run_extern.ml \
 run_finalise.ml \
 run_floats.ml \
 run_gc_ctrl.ml \
 run_hash.ml \
 run_intern.ml \
 run_ints.ml \
 run_io.ml \
 run_lexing.ml \
 run_md5.ml \
 run_meta.ml \
 run_obj.ml \
 run_parsing.ml \
 run_signals.ml \
 run_stacks.ml \
 run_str.ml \
 run_sys.ml \
 run_terminfo.ml \
 run_weak.ml \

SRC=$(SRC1) $(SRC2)

# for Ffi
INCLUDEDIRS=$(TOP)/interpreter $(TOP)/commons

##############################################################################
# Generic variables
##############################################################################

-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################
all:: $(TARGET).cma
all.opt:: $(TARGET).cmxa

# need -linkall, otherwise the Ffi.init_list modifs are not done
# because the module appears unreferenced and so is not linked
$(TARGET).cma: $(OBJS)
	$(OCAMLC) -linkall -a -o $(TARGET).cma $(OBJS)

$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -linkall -a -o $(TARGET).cmxa $(OPTOBJS)
