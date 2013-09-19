TOP=..
##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC= instructions.ml value.ml \
     vm.ml \
     utils.ml conv_obj_value.ml debug_events.ml \
     interpreter.ml \
     ffi.ml
#less: split more? bytecode/, objects/?,  interpreter/, extensions/

OCAMLCOMPILERDIR=$(shell ocamlc -where)/compiler-libs

INCLUDEDIRS=$(TOP)/commons $(OCAMLCOMPILERDIR)

##############################################################################
# Generic variables
##############################################################################

-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################
all:: $(TARGET).cma
all.opt:: $(TARGET).cmxa

$(TARGET).cma: $(OBJS)
	$(OCAMLC) -a -o $@ $^

$(TARGET).cmxa: $(OBJS:.cmo=.cmx) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -a -o $@ $^
