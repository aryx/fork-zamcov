PROGS=foo use_nat use_oo use_array
all: $(PROGS)
#use_commons

OCAMLC=ocamlc -g -annot
ZAM=../zamcov

foo: foo.ml
	$(OCAMLC) -custom -o foo foo.ml

use_nat: use_nat.ml
	$(OCAMLC) -o use_nat use_nat.ml nums.cma

use_oo: use_oo.ml
	$(OCAMLC) -o $@ $^

use_array: use_array.ml
	$(OCAMLC) -o $@ $^

test:
	$(ZAM) foo

test2:
	../zamcov_test foo

clean:
	rm -f *.cm*
	rm -f $(PROGS)

.PHONY: use_commons

use_commons: use_commons.ml
	$(OCAMLC) -thread -linkall -custom unix.cma str.cma nums.cma bigarray.cma threads.cma -I ~/pfff/commons -o use_commons ~/pfff/commons/lib.cma  use_commons.ml
