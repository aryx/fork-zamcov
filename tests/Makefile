all: foo use_nat

ZAM=../zamcov-run

foo: foo.ml
	ocamlc -o foo foo.ml

use_nat: use_nat.ml
	ocamlc -o use_nat use_nat.ml nums.cma


test:
	$(ZAM) foo

clean:
	rm -f *.cm*
	rm -f foo use_nat
