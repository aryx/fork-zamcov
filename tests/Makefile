all: foo use_nat

OCAMLC=ocamlc -g
ZAM=../zamcov

foo: foo.ml
	$(OCAMLC) -custom -o foo foo.ml

use_nat: use_nat.ml
	$(OCAMLC) -o use_nat use_nat.ml nums.cma

test:
	$(ZAM) foo

test2:
	../zamcov_test foo

clean:
	rm -f *.cm*
	rm -f foo use_nat
