# where to install Zamcov
PREFIX=/usr/local

# O'Caml version used
OCAMLVERSION=4.00.1

# location of the O'Caml sources (relative path)
OCAMLTAR=ocaml-$(OCAMLVERSION).tar.gz

# O'Caml compilers to use
OCAMLOPT=ocamlopt -bin-annot -annot
OCAMLC=ocamlc -bin-annot -annot

# thread flavor: "" for none, -thread, or -vmthread
THREAD=-thread

# list of libraries to link in zamcov-run interpreter
CLIBS=unix nums str threads graphics bigarray

# list of runtime elements to link in zamcov-run interpreter
NATIVE_RUNTIME= \
 alloc array backtrace callback compare extern finalise floats gc_ctrl hash \
 intern ints io lexing md5 obj parsing signals str sys terminfo weak

# list of runtime elements to link in zamcov-run.byte interpreter
BYTECODE_RUNTIME=$(NATIVE_RUNTIME) dynlink meta stacks
