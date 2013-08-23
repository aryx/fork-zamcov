all: foo

ZAM=../zamcov-run

foo: foo.ml
	ocamlc -o foo foo.ml



test:
	$(ZAM) foo
