# Copyright (c) Microsoft Corporation.  All rights reserved.

# Detect operating system, architecture, and number of processors
ifeq (${OS},Windows_NT)
  ARCH=x86
  NPROCS:=$(subst NumberOfLogicalProcessors=,,$(strip $(shell wmic cpu get NumberOfLogicalProcessors /value)))
else
  OS := $(shell uname -s)
  ARCH := $(shell uname -m)
ifeq ($(OS), Linux)
  NPROCS:=$(shell nproc)
endif
ifeq ($(OS), Darwin)
  NPROCS:=$(subst hw.ncpu: ,,$(shell sysctl hw.ncpu))
endif
endif

ifeq ($(OS), Windows_NT)
# path to the root of the SLAyer source, in windows format since it is passed to the linker
SRC=$(shell cygpath -m $(CURDIR))
EXE=.exe
OSBUILDFLAGS=-lib esp -lflags "-cclib /link/NODEFAULTLIB:LIBCMT"
Frontend_byte=Frontend.byte
Frontend_native=Frontend.native
else
SRC=$(CURDIR)
endif


# To build SLAyer execute:
#       make
# To build all unit tests execute:
#       make test
# To build a set of unit tests <test1>...<testN> execute:
#       make TEST={<test1>.ml ... <testN>.ml} test
#
# By default set TEST to the set of ml files in StandAlone:
TEST:=$(notdir $(wildcard $(SRC)/UnitTests/StandAlone/*.ml))

# To build a single module execute:
#       make M=<module> module
M=

SLAyer_CFLAGS?=-cflags -annot

# Note: ocamlbuild seems not to have a rule for building .cmi, .cmx, and .o
# from .ml directly, and instead builds .cmo and .cmi from .ml and then
# builds .cmx and .o from .cmi and .ml, thereby compiling everything twice.


# path to the Z3 source directory, build directory, and the subdirectory for the compiled OCaml interface
# these are in unix format since they are used in make rules where windows' colons are invalid
Z3_SRC=$(CURDIR)/../tools/Z3/src
Z3_BLD=$(Z3_SRC)/../build
Z3_BLD_ML=$(Z3_BLD)/api/ml
# and in windows format for the linker
Z3_BLD_ML_WIN=$(SRC)/../tools/Z3/build/api/ml

# path to the ESP compiled OCaml interface, in windows format for the linker
ESP_BLD=$(SRC)/../tools/esp/ocaml

# name of directory to contain SLAyer compiled code
BUILD=_build/$(OS)/$(ARCH)

# common ocamlbuild options
OCAMLBUILD= \
  ocamlbuild \
  -j $(NPROCS) -no-links -build-dir $(BUILD) \
  -tag bin_annot \
  -cflags "-short-paths -w +6+7+27+29+32..39+41+44+45" \
  -cflags -I,$(ESP_BLD) -lflags -I,$(ESP_BLD) \
  -cflags -I,$(Z3_BLD_ML_WIN) -lflags -I,$(Z3_BLD_ML_WIN) \
  $(SLAyer_CFLAGS) \
  -Is Library,contaminated,UnitTests,UnitTests/StandAlone \
  -libs str,unix,z3 \
  $(OSBUILDFLAGS)


## SLAyer targets

default: dbg

help:
	@cat README

all: dbg opt #doc


dbg: $(Z3_BLD_ML)/z3.cma $(Z3_BLD_ML)/z3.cmxa ../bin version
	@echo "======== building SLAyer debug (byte & native) code ========"
	$(OCAMLBUILD) -tag debug \
	slayer.byte $(Frontend_byte) slayer.native $(Frontend_native)
	-@cp -p $(BUILD)/slayer.native ../bin/slayer$(EXE)
	-@if test -f $(BUILD)/Frontend.native; then cp -p $(BUILD)/Frontend.native ../bin/frontend$(EXE); fi


opt: $(Z3_BLD_ML)/z3.cmxa ../bin version
	@echo "======== building SLAyer optimized (native) code ========"
	$(OCAMLBUILD) -build-dir $(BUILD)_opt -tag debug -cflags -noassert,-unsafe -lflags -noassert,-unsafe \
	slayer.native $(Frontend_native)
	-@cp -p $(BUILD)_opt/slayer.native ../bin/slayer$(EXE)
	-@if test -f $(BUILD)_opt/Frontend.native; then cp -p $(BUILD)_opt/Frontend.native ../bin/frontend$(EXE); fi


test: $(Z3_BLD_ML)/z3.cmxa version
	@echo "======== building SLAyer UnitTests/StandAlone/{$(TEST)} ========"
	$(OCAMLBUILD) -tag debug \
	$(patsubst %.ml,UnitTests/StandAlone/%.native,$(TEST))
	@for ut in $(patsubst %.ml,%.native,$(TEST)); do \
		cp -p $(BUILD)/UnitTests/StandAlone/$$ut ../bin/$$ut; \
	done


testopt: $(Z3_BLD_ML)/z3.cmxa version
	@echo "======== building SLAyer UnitTests/StandAlone/{$(TEST)} ========"
	@$(OCAMLBUILD) \
	  -build-dir $(BUILD_DIR)_opt -tag debug \
	  -cflags -noassert,-unsafe -lflags -noassert,-unsafe \
	$(patsubst %.ml,UnitTests/StandAlone/%.native,$(TEST))
	@for ut in $(patsubst %.ml,%.native,$(TEST)); do \
		cp -p $(BUILD_DIR)_opt/UnitTests/StandAlone/$$ut ../bin/$$ut; \
	done


module: $(Z3_BLD_ML)/z3.cma version
	$(OCAMLBUILD) -tag debug \
	$(M).byte


moduleasm: version
	$(OCAMLBUILD) -tag debug \
	  -cflags -noassert,-unsafe -lflags -noassert,-unsafe \
	  -ocamlopt 'ocamlopt.opt -S' \
	$(M).native


byte: $(Z3_BLD_ML)/z3.cma version
	-$(OCAMLBUILD) -tag debug \
	slayer.byte Frontend.byte


## Versioning

.PHONY:
version:
	echo "let version = \"$(shell git describe --tags)\"" > Version.ml


## Z3
ifeq ($(OS), Windows_NT)
$(Z3_BLD)/Makefile:
	@test "[ $(shell python -c 'import os; print os.name') = nt ]" \
	|| (echo "Python does not report OS is Windows NT.  Do not use cygwin python."; exit 1)
	cd $(Z3_SRC)/..; python scripts/mk_make.py --parallel=$(NPROCS)
else
$(Z3_BLD)/Makefile:
	cd $(Z3_SRC)/..; python scripts/mk_make.py
endif

$(Z3_BLD_ML)/Makefile: $(Z3_SRC)/api/ml/Makefile.build
	mkdir -p $(Z3_BLD_ML)
	cp -p $(Z3_SRC)/api/ml/Makefile.build $(Z3_BLD_ML)/Makefile

ifeq ($(OS), Windows_NT)
.PHONY:
z3dll: $(Z3_BLD)/Makefile
	cd $(Z3_BLD); nmake /nologo api_dll
else
.PHONY:
z3dll: $(Z3_BLD)/Makefile
	$(MAKE) -j $(NPROCS) -C $(Z3_BLD) api_dll
endif

$(Z3_BLD_ML)/%.cma $(Z3_BLD_ML)/%.cmxa: $(Z3_BLD_ML)/Makefile z3dll
	$(MAKE) DEPS='$(CURDIR)/_build*/{Slayer,CounterExample,Pure}.cm{i,o,x}' ARGS='-DUNSAFE_ERRORS -DLEAK_CONTEXTS' -C $(Z3_BLD_ML) z3.cma z3.cmxa


## SLAyer build tools

get-tools: get-flexdll get-ocaml get-sdv get-esp

get-flexdll:
	robocopy /mir //cam-01-srv/dfsroot/groups/varg/tools/flexdll/0.31 ../tools/flexdll ; \
	if [ $$? -ge 8 ]; then $$? else 0; fi

get-ocaml: get-flexdll
	robocopy /mir //cam-01-srv/dfsroot/groups/varg/tools/ocaml/4.01.0 ../tools/ocaml ; \
	if [ $$? -ge 8 ]; then $$? else 0; fi

get-sdv:
	robocopy /mir //cam-01-srv/dfsroot/groups/varg/tools/sdv ../tools/sdv ; \
	if [ $$? -ge 8 ]; then $$? else 0; fi

get-esp:
	robocopy /mir //cam-01-srv/dfsroot/groups/varg/tools/esp ../tools/esp ; \
	if [ $$? -ge 8 ]; then $$? else 0; fi
	pushd ../tools/esp/ocaml; cmd /c build.cmd; popd


## Documentation

# files to document: module dependencies
#
# This list is manually produced from the final link command generated by
# ocamlbuild, removing Timer files, duplicating the list, and
# changing the file extensions.
#
SLAYER_DOC_MOD= \
Library/NSLib.mli Library/NSArg.mli Library/NSOption.mli Library/NSList.mli Library/NSSet.mli Library/NSBinaryRelation.mli Library/NSHashtbl.mli Library/NSHashMap.mli Library/NSHashMultiMap.mli Library/NSPolyHashMap.mli Library/NSHashSet.mli Library/NSMap.mli Library/NSImperativeMap.mli Library/NSMultiMap.mli Library/NSImperativeMultiMap.mli Library/NSImperativeSet.mli Library/NSIndexedSet.mli Library/NSMultiSet.mli Library/NSSortedList.mli Library/NSString.mli Library/NSTuple.mli Library.mli Config.mli Hooks.mli Variable.mli Field.mli Log.mli Type.mli Expression.mli Graph_sig.mli Graph.mli Substitution.mli Pure.mli UnitTests/TestGen.mli SymbolicHeap.mli SIL.mli Interproc_sig.mli AbstractTransitionSystem.mli Discovery.mli HeapGraph.mli UnitTests/TestGenProver.mli Prover.mli Reachability.mli HeapAbstraction.mli Abstraction.mli Interproc.mli Frame.mli SymbolicExecution.mli Analysis.mli SIL_wf.mli Inline.mli Statistics.mli Initialize.mli JoinPoint.mli Peephole.mli slayer.mli \
Library/NSLib.ml Library/NSArg.ml Library/NSOption.ml Library/NSList.ml Library/NSSet.ml Library/NSBinaryRelation.ml Library/NSHashtbl.ml Library/NSHashMap.ml Library/NSHashMultiMap.ml Library/NSPolyHashMap.ml Library/NSHashSet.ml Library/NSMap.ml Library/NSImperativeMap.ml Library/NSMultiMap.ml Library/NSImperativeMultiMap.ml Library/NSImperativeSet.ml Library/NSIndexedSet.ml Library/NSMultiSet.ml Library/NSSortedList.ml Library/NSString.ml Library/NSTuple.ml Library.ml Config.ml Hooks.ml Variable.ml Field.ml Log.ml Type.ml Expression.ml Graph_sig.ml Graph.ml Substitution.ml Pure.ml UnitTests/TestGen.ml SymbolicHeap.ml SIL.ml Interproc_sig.ml AbstractTransitionSystem.ml Discovery.ml HeapGraph.ml UnitTests/TestGenProver.ml Prover.ml Reachability.ml HeapAbstraction.ml Abstraction.ml Interproc.ml Frame.ml SymbolicExecution.ml Analysis.ml SIL_wf.ml Inline.ml Statistics.ml Initialize.ml JoinPoint.ml Peephole.ml slayer.ml

#  contaminated/PolySet.mli Library/NSPolySet.mli CounterExample.mli Instrumentation.mli ReconstructSpecs.mli Unification.mli Predicates.mli PredicateEval.mli
#  contaminated/PolySet.ml Library/NSPolySet.ml CounterExample.ml Instrumentation.ml ReconstructSpecs.ml Unification.ml Predicates.ml PredicateEval.ml

# Don't document Timer since it needs camlp4, and it isn't obvious how to tell
# ocamldoc to run camlp4 on only one file.
# Timer.mli Timer.ml

# files to document: html docs
SLAYER_DOC= $(SLAYER_DOC_MOD)

MISSING_MLI= Graph_sig.mli Interproc_sig.mli

.PHONY: clean_missing_mli
clean_missing_mli:
	rm -f $(MISSING_MLI:%=$(BUILD)/%) \
	       $(MISSING_MLI:%=%) \
	       $(MISSING_MLI:%.mli=$(BUILD)/%.cmi) \
	       $(MISSING_MLI:%.mli=$(BUILD)/%.mli.depends)

../bin:
	mkdir -p ../bin

%.mli : %.ml
	ocamlc -I $(BUILD) -i $< > $*.mli

%.pdf : %.dot
	dot -Tpdf doc/$< > doc/$*.pdf

OCAMLDOC_FLAGS= \
	-d doc \
	-I $(BUILD) \
	-I $(SRC)/../tools/Z3/src/api/ml/ \
	-I $(BUILD)/Library/ \
	-I $(BUILD)/contaminated \
	-I $(BUILD)/UnitTests


.PHONY: doc
doc: byte $(MISSING_MLI)
	echo "\ngenerating SLAyer source documentation\n"
	mkdir -p doc
	ocamldoc.opt -keep-code -html -short-functors -t SLAyer \
	  -hide Variable,Expression,SymbolicHeap \
	  $(OCAMLDOC_FLAGS) doc/depend_module.mli $(SLAYER_DOC)
	rm -f ocamldoc.out
	$(MAKE) clean_missing_mli

# 	@echo "\ngenerating SLAyer module dependency graph\n"
# 	ocamldoc.opt -dot -dot-reduce \
# 	$(OCAMLDOC_FLAGS) $(SLAYER_DOC_MOD)
# 	dot -Tpdf ocamldoc.out > doc/depend_module.pdf

.PHONY: depend_module
depend_module: depend_module.dot
depend_module.dot: default $(MISSING_MLI)
	@echo "\ngenerating SLAyer module dependency graph\n"
	OCAMLLIB=$(SRC)/../tools/ocaml/lib \
	ocamldoc.opt -dot -dot-reduce \
	$(OCAMLDOC_FLAGS) $(SLAYER_DOC_MOD)
	mv ocamldoc.out doc/depend_module.dot
	$(MAKE) clean_missing_mli

.PHONY: depend_module_full
depend_module_full: depend_module_full.dot
depend_module_full.dot: default $(MISSING_MLI)
	@echo "\ngenerating SLAyer unreduced module dependency graph\n"
	OCAMLLIB=$(SRC)/../tools/ocaml/lib \
	ocamldoc.opt -dot \
	$(OCAMLDOC_FLAGS) $(SLAYER_DOC_MOD)
	mv ocamldoc.out doc/depend_module_full.dot
	$(MAKE) clean_missing_mli

.PHONY: depend_type
depend_type: depend_type.dot
depend_type.dot: default $(MISSING_MLI)
	@echo "\ngenerating SLAyer type dependency graph\n"
	OCAMLLIB=$(SRC)/../tools/ocaml/lib \
	ocamldoc.opt -dot-types $(OCAMLDOC_FLAGS) $(SLAYER_DOC)
	mv ocamldoc.out doc/depend_type.dot
	$(MAKE) clean_missing_mli


# cleanup
clean_slayer: clean_missing_mli
	rm -f Version.ml _build*/*.{ml,mli,cmi,cmo,cmx,cma,cmxa,o,obj,depends}

clean: clean_slayer
	rm -rf _build* *~ */*~ ../bin/slayer$(EXE) ../bin/frontend$(EXE) ../bin/*.byte ../bin/*.native doc/*.html doc/depend_module.pdf

clean_mlz3:
	$(MAKE) -C $(Z3_BLD_ML) clean

clean_z3:
	rm -rf $(Z3_BLD)
