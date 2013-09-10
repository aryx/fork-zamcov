#############################################################################
# Configuration section
#############################################################################

-include Makefile.config

##############################################################################
# Variables
##############################################################################
TOP=$(shell pwd)

SRC=

TARGET=zamcov

#------------------------------------------------------------------------------
# Program related variables
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
#package dependencies
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# Main variables
#------------------------------------------------------------------------------
SYSLIBS=

LIBS=

MAKESUBDIRS=

INCLUDEDIRS=$(MAKESUBDIRS)

##############################################################################
# Generic
##############################################################################
-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################

PLUGINS=Debug

all:zamcov-run zamcov-run.byte zamcov-extract

zamcov-extract: instructions.cmo bytecode_loader.cmo mainExtract.cmo
	$(OCAMLC) -I common -o "$@" $^

INTERPRETER_DEP=common instructions value vm utils ffi interpreter
interpreter.cmxa: $(addsuffix .cmx,$(INTERPRETER_DEP))
interpreter.cma: $(addsuffix .cmo,$(INTERPRETER_DEP))

zamcov-run: interpreter.cmxa $(addsuffix .cmx,bytecode_loader \
            $(addprefix clibs/other_,$(CLIBS)) \
	    $(addprefix clibs/runtime_,$(NATIVE_RUNTIME)) \
	    $(addprefix mllibs/lib_,$(CLIBS)) \
	    $(addprefix mllibs/run_,$(BYTECODE_RUNTIME)) \
	    plugin $(addprefix plugin,$(PLUGINS)) mainRun)
	$(OCAMLOPT) $(THREAD) $(addsuffix .cmxa,$(CLIBS)) -o "$@" $^

zamcov-run.byte: interpreter.cma $(addsuffix .cmo,bytecode_loader \
                 $(addprefix clibs/other_,$(CLIBS)) \
		 $(addprefix clibs/runtime_,$(BYTECODE_RUNTIME)) \
		 $(addprefix mllibs/lib_,$(CLIBS)) \
		 $(addprefix mllibs/run_,$(BYTECODE_RUNTIME)) \
		 plugin $(addprefix plugin,$(PLUGINS)) mainRun)
	$(OCAMLC) $(THREAD) -g $(addsuffix .cma,$(CLIBS)) -o "$@" $^


clibs/%.ml:
	cd clibs && make $*.ml
%.cmi:%.mli
	$(OCAMLC) -c "$<"
%.cmxa:
	$(OCAMLOPT) -a -o "$@" $^
%.cma:
	$(OCAMLC) -a -g -o "$@" $^
%.cmx:%.ml
	$(OCAMLOPT) -c "$<"
%.cmo:%.ml
	$(OCAMLC) -g -c "$<"

depend:
	ocamldep *.ml > .depend

clean:
	cd clibs && make clean
	cd mllibs && make clean
	rm -f *.a *.cm* *.bc *.nc *.o *.annot

distclean: clean
	rm -f zamcov-run.byte zamcov-run zamcov-extract
	cd clibs && make distclean

-include .depend

##############################################################################
# Install
##############################################################################

##############################################################################
# Package rules
##############################################################################

##############################################################################
# Developer rules
##############################################################################

.PHONY:: graph
# tags prolog  db layers visual   tests test