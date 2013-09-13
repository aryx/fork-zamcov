#############################################################################
# Configuration section
#############################################################################

-include Makefile.config

##############################################################################
# Variables
##############################################################################
TOP=$(shell pwd)

SRC=common.ml \
 bytecode_loader.ml \
 instructions.ml value.ml vm.ml utils.ml ffi.ml
 interpreter.ml \
 pluginDebug.ml

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

MAKESUBDIRS= clibs mllibs

INCLUDEDIRS=$(MAKESUBDIRS)

##############################################################################
# Generic
##############################################################################
-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################

.PHONY:: all all.opt opt top clean distclean

all::
	$(MAKE) rec 
	$(MAKE) $(TARGET) 

opt:
	$(MAKE) rec.opt 
	$(MAKE) $(OPTPROGS) 
all.opt: opt
top: $(TARGET).top


rec:
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all || exit 1; done 

rec.opt:
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all.opt || exit 1; done 

zamcov:

zamcov.opt:

clean::
	rm -f zamcov zamcov.opt zamcov.top
clean::
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i clean; done 


depend::
	ocamldep *.ml > .depend
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i depend; done


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

# graph  tags prolog  db layers visual   tests test