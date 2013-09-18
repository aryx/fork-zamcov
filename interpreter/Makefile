TOP=..
##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC= instructions.ml value.ml \
     vm.ml \
     utils.ml conv_obj_value.ml \
     interpreter.ml \
     ffi.ml

INCLUDEDIRS=$(TOP)/commons

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
