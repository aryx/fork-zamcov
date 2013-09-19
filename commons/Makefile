TOP=..
##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC= common.ml common2.ml ocaml.ml

INCLUDEDIRS=

COBJS=realpath.o

# because we need C code
OCAMLMKLIB=ocamlmklib
CC=gcc

##############################################################################
# Generic variables
##############################################################################

-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################
all:: $(TARGET).cma
all.opt:: $(TARGET).cmxa

# need to use ocamlmklib, otherwise with ocamlc or ocamlopt get some
# pbs with realpath.c at some point
$(TARGET).cma: $(OBJS) $(COBJS)
	$(OCAMLMKLIB) -o lib $^

$(TARGET).cmxa: $(OBJS:.cmo=.cmx) $(LIBS:.cma=.cmxa) $(COBJS)
	$(OCAMLMKLIB) -o lib $^

realpath.o: realpath.c
	$(OCAMLC) -c $^ -o $@
