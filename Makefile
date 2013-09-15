#############################################################################
# Configuration section
#############################################################################

-include Makefile.config

##############################################################################
# Variables
##############################################################################
TOP=$(shell pwd)

SRC= \
 bytecode_loader.ml \
 cmo_loader.ml \
 plugin.ml pluginDebug.ml

TARGET=zamcov

#------------------------------------------------------------------------------
# Program related variables
#------------------------------------------------------------------------------
PROGS=zamcov zamcov_test

#------------------------------------------------------------------------------
#package dependencies
#------------------------------------------------------------------------------
OCAMLCOMPILERDIR=$(shell ocamlc -where)/compiler-libs
OCAMLCOMPILERCMA=ocamlcommon.cma

#------------------------------------------------------------------------------
# Main variables
#------------------------------------------------------------------------------
SYSLIBS=nums.cma bigarray.cma str.cma unix.cma

SYSLIBS+=$(OCAMLCOMPILERCMA)


# The order of clibs and mllibs below is very important
# mllibs must come last so that it's Fffi.init_list statements
# come last and so overwrite the naive wrappers in clibs
# (e.g. caml_register_named_value must not be redefined in mllibs
# so that we do not call the default one provided by ocaml that
# works on the Obj format, not the Value format)
LIBS= commons/lib.cma \
  interpreter/lib.cma \
  clibs/lib.cma mllibs/lib.cma 

MAKESUBDIRS=commons interpreter mllibs clibs

INCLUDEDIRS=$(MAKESUBDIRS) $(OCAMLCOMPILERDIR)

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

zamcov: $(LIBS) $(OBJS) main_zamcov.cmo
	$(OCAMLC) $(BYTECODE_STATIC) -o $@ $(SYSLIBS) $^
zamcov.opt: $(LIBS:.cma=.cmxa) $(OBJS:.cmo=.cmx) main_zamcov.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa)  $^
zamcov.top: $(LIBS) $(OBJS) 
	$(OCAMLMKTOP) -o $@ $(SYSLIBS) threads.cma $^


clean::
	rm -f zamcov zamcov.opt zamcov.top

clean::
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i clean; done 

depend::
	ocamldep *.ml *.mli > .depend
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i depend; done

Makefile.config:    
	@echo "Makefile.config is missing. Have you run ./configure?"
	@exit 1

distclean:: clean
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i $@; done
	rm -f .depend
#	rm -f Makefile.config

#------------------------------------------------------------------------------
# zamcov_test
#------------------------------------------------------------------------------

zamcov_test: $(LIBS) $(OBJS) main_test.cmo
	$(OCAMLC) $(BYTECODE_STATIC) -o $@ $(SYSLIBS) $^
zamcov_test.opt: $(LIBS:.cma=.cmxa) $(OBJS:.cmo=.cmx) main_test.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa)  $^
clean::
	rm -f zamcov_test zamcov_test.opt

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
#  tags prolog  db layers    tests test

graph:
	$(PFFF)/codegraph -lang cmt -build .
visual:
	$(PFFF)/codemap -no_legend .

##############################################################################
# Pad specific rules
##############################################################################
