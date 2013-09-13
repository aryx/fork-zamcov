#############################################################################
# Configuration section
#############################################################################

-include Makefile.config

##############################################################################
# Variables
##############################################################################
TOP=$(shell pwd)

SRC=common.ml \
 instructions.ml value.ml vm.ml utils.ml \
 bytecode_loader.ml \
 interpreter.ml \
 ffi.ml \
 plugin.ml pluginDebug.ml

TARGET=zamcov

#------------------------------------------------------------------------------
# Program related variables
#------------------------------------------------------------------------------
PROGS=zamcov

#------------------------------------------------------------------------------
#package dependencies
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# Main variables
#------------------------------------------------------------------------------
SYSLIBS=nums.cma bigarray.cma str.cma unix.cma
#SYSLIBS+=$(OCAMLCOMPILERCMA)

LIBS=

MAKESUBDIRS=
#clibs mllibs

INCLUDEDIRS=$(MAKESUBDIRS)

##############################################################################
# Generic
##############################################################################
-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################

.PHONY:: all all.opt opt top clean distclean

all:: Makefile.config
	$(MAKE) rec 
	$(MAKE) $(TARGET) $(PROGS)
opt:
	$(MAKE) rec.opt 
	$(MAKE) $(PROGS:=.opt)
all.opt: opt
top: $(TARGET).top


rec:
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all || exit 1; done 

rec.opt:
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all.opt || exit 1; done 

zamcov: $(LIBS) $(OBJS) mainRun.cmo
	$(OCAMLC) $(BYTECODE_STATIC) -o $@ $(SYSLIBS) $^
zamcov.opt: $(LIBS:.cma=.cmxa) $(OBJS:.cmo=.cmx) mainRun.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa)  $^
zamcov.top: $(LIBS) $(OBJS) 
	$(OCAMLMKTOP) -o $@ $(SYSLIBS) threads.cma $^


clean::
	rm -f zamcov zamcov.opt zamcov.top

clean::
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i clean; done 

depend::
	ocamldep *.ml > .depend
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i depend; done

Makefile.config:    
	@echo "Makefile.config is missing. Have you run ./configure?"
	@exit 1

distclean:: clean
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i $@; done
	rm -f .depend
#	rm -f Makefile.config

##############################################################################
# TODO: old
##############################################################################

#PLUGINS=Debug
#
#all:zamcov-run zamcov-run.byte zamcov-extract
#
#zamcov-extract: instructions.cmo bytecode_loader.cmo mainExtract.cmo
#	$(OCAMLC) -I common -o "$@" $^
#
#INTERPRETER_DEP=common instructions value vm utils ffi interpreter
#interpreter.cmxa: $(addsuffix .cmx,$(INTERPRETER_DEP))
#interpreter.cma: $(addsuffix .cmo,$(INTERPRETER_DEP))
#
#zamcov-run: interpreter.cmxa $(addsuffix .cmx,bytecode_loader \
#            $(addprefix clibs/other_,$(CLIBS)) \
#	    $(addprefix clibs/runtime_,$(NATIVE_RUNTIME)) \
#	    $(addprefix mllibs/lib_,$(CLIBS)) \
#	    $(addprefix mllibs/run_,$(BYTECODE_RUNTIME)) \
#	    plugin $(addprefix plugin,$(PLUGINS)) mainRun)
#	$(OCAMLOPT) $(THREAD) $(addsuffix .cmxa,$(CLIBS)) -o "$@" $^
#
#zamcov-run.byte: interpreter.cma $(addsuffix .cmo,bytecode_loader \
#                 $(addprefix clibs/other_,$(CLIBS)) \
#		 $(addprefix clibs/runtime_,$(BYTECODE_RUNTIME)) \
#		 $(addprefix mllibs/lib_,$(CLIBS)) \
#		 $(addprefix mllibs/run_,$(BYTECODE_RUNTIME)) \
#		 plugin $(addprefix plugin,$(PLUGINS)) mainRun)
#	$(OCAMLC) $(THREAD) -g $(addsuffix .cma,$(CLIBS)) -o "$@" $^
#
#
#clibs/%.ml:
#	cd clibs && make $*.ml
#%.cmi:%.mli
#	$(OCAMLC) -c "$<"
#%.cmxa:
#	$(OCAMLOPT) -a -o "$@" $^
#%.cma:
#	$(OCAMLC) -a -g -o "$@" $^
#%.cmx:%.ml
#	$(OCAMLOPT) -c "$<"
#%.cmo:%.ml
#	$(OCAMLC) -g -c "$<"

##############################################################################
# Install
##############################################################################

##############################################################################
# Package rules
##############################################################################

##############################################################################
# Website rules
##############################################################################

##############################################################################
# Developer rules
##############################################################################

.PHONY:: graph

PFFF=~/pfff
#  tags prolog  db layers visual   tests test
graph:
	$(PFFF)/codegraph -lang cmt -build .

##############################################################################
# Pad specific rules
##############################################################################
