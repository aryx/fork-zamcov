all: foo use_nat oo
#use_commons

OCAMLC=ocamlc -g
ZAM=../zamcov

foo: foo.ml
	$(OCAMLC) -custom -o foo foo.ml

use_nat: use_nat.ml
	$(OCAMLC) -o use_nat use_nat.ml nums.cma

oo: oo.ml
	$(OCAMLC) -o $@ $^

test:
	$(ZAM) foo

test2:
	../zamcov_test foo

clean:
	rm -f *.cm*
	rm -f foo use_nat

.PHONY: use_commons

use_commons: use_commons.ml
	$(OCAMLC) -thread -linkall -custom unix.cma str.cma nums.cma bigarray.cma threads.cma -I ~/pfff/commons -o use_commons ~/pfff/commons/lib.cma  use_commons.ml
