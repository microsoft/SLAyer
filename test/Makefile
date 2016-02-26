# Copyright (c) Microsoft Corporation.  All rights reserved.

## parameters

# directories containing tests to run
DIR?=other ssa sll kmdf/1394 csll dev cex #sll_rec

# directories containing directories of multi-file tests to run
# (Make sure MDIR (eg, kmdf/1394) is disjoint from DIR (eg, kmdf/toaster/bus/static).)
TOASTER_BUS=kmdf/toaster/bus/static kmdf/toaster/bus/dynamic kmdf/toaster/bus/static_fail1
TOASTER_FUNC=kmdf/toaster/func/simple kmdf/toaster/func/featured kmdf/toaster/func/simple_fail1
TOASTER_FILTER=kmdf/toaster/filter/generic kmdf/toaster/filter/sideband
TOASTER_TOASTMON=kmdf/toaster/toastmon
MDIR?=multi/link $(TOASTER_BUS) $(TOASTER_FUNC) $(TOASTER_FILTER) $(TOASTER_TOASTMON)

# additional arguments to pass to slayer
ARG?=

# limits
TIMEOUT?=1200
MEMOUT?=3072

# previous tsv file
TSV?=RESULT.curr.tsv

# version to tag results with
VER?=$(shell git rev-parse --short HEAD)


# commands which may need to be explicitly specified if shadowed in $PATH
DIFF?=diff
FIND?=find
GREP?=grep


## environment-specific config

ifeq (${OS}, Windows_NT)
# in windows format since it is passed to wlimit
CURDIR_WIN=$(shell cygpath -m $(CURDIR))
TST=$(shell cygpath -m $(CURDIR))
EXE=.exe
else
TST=$(CURDIR)
EXE=
endif

BUILD_DIR=_build${ARCH}

# executable to test
SLAYER_EXE=$(CURDIR)/../bin/slayer$(EXE)
SLAYER_EXE_WIN=$(CURDIR_WIN)/../bin/slayer$(EXE)
ifeq (${OS}, Windows_NT)
FRONTEND_EXE=$(shell dirname $(SLAYER_EXE))/frontend$(EXE)
endif


default: test

help:
	@cat README

bugs:
	@echo "EXPECTED                                                                      | CORRECT"
	@$(DIFF) -y -W 160 --suppress-common-lines EXPECTED.txt CORRECT.txt || true

report:
	@$(GREP) Expected RESULT.curr.txt

bin:
	mkdir bin

# compile result gathering script
bin/gather_results$(EXE): scripts/gather_results.ml bin
	@ocamlbuild -build-dir $(BUILD_DIR) -no-links -libs str,unix scripts/gather_results.native
	@cp -p $(BUILD_DIR)/scripts/gather_results.native bin/gather_results$(EXE)

# compile result comparison script
bin/compare_results$(EXE): scripts/compare_results.ml bin
	@ocamlbuild -build-dir $(BUILD_DIR) -no-links -libs str scripts/compare_results.native
	@cp -p $(BUILD_DIR)/scripts/compare_results.native bin/compare_results$(EXE)


# C programs under test
TestCs:=$(shell $(FIND) $(DIR) -name '*.c')
TestDirs:=$(MDIR)
UnsortedCompSils:=$(patsubst %.c,%.sil,$(TestCs)) $(foreach dir,$(TestDirs),$(dir)/$(notdir $(dir)).sil)
CompSils:=$(shell scripts/sort_tests.ml -tsv $(TSV) $(UnsortedCompSils))
ifeq (${OS}, Windows_NT)
TestSils:=$(CompSils)
else
TestSils:=$(shell $(FIND) $(DIR) -name *.sil)
endif
SlayerOuts:=$(patsubst %.sil,%.slayer.out,$(TestSils))

TEST_FE=$(CURDIR)/bin/frontend$(EXE)
TEST_BE=$(CURDIR)/bin/slayer$(EXE)
TEST_FE_EXE=$(TST)/bin/frontend$(EXE)
TEST_BE_EXE=$(TST)/bin/slayer$(EXE)

.PHONY: copy_exes
copy_exes: $(SLAYER_EXE) $(FRONTEND_EXE)
	@mkdir -p `dirname $(TEST_FE_EXE)`
	@cp -p -f $(FRONTEND_EXE) $(TEST_FE_EXE)
	@mkdir -p `dirname $(TEST_BE_EXE)`
	@cp -p -f $(SLAYER_EXE) $(TEST_BE_EXE)


# command to run one test
ifeq (${OS}, Windows_NT)
# lookup where test/config.sh says wlimit is
WLIMIT:=$(shell bash -c 'source $(TST)/config.sh; which wlimit.exe')
RUNCMD=\
  ${WLIMIT} /q /w $(TIMEOUT) /m $(MEMOUT) $(TEST_BE_EXE) -st $(ARG) $(notdir $*).sil \
  &> $(notdir $*).slayer.out
COPY_EXES=copy_exes
else
RUNCMD=\
  ((ulimit -t $(TIMEOUT) -v $$(( $(MEMOUT) * 1024 )); \
    $(SLAYER_EXE) -st $(ARG) $(notdir $*).sil) \
   &> $(notdir $*).slayer.out)
COPY_EXES=
endif


# compile c to sil
%.sil : %.c $(FRONTEND_EXE)
	@echo $*.c
	-@bash -c ' \
	  source $(TST)/config.sh; \
	  cd $(dir $*); \
	  $(TEST_FE_EXE) $(ARG) $(notdir $*).c &> $(notdir $*).slayer.out '

# Specific rules for compiling all c files in a test dir to sil
multi/link/link.sil : $(wildcard $(dir $@)/*.c) $(wildcard $(dir $@)/*.h) $(FRONTEND_EXE)
	@echo $(dir $@)
	-@bash -c ' \
	  source $(TST)/config.sh; \
	  cd $(dir $@); \
	  $(TEST_FE_EXE) $(ARG) *.c &> $(basename $(notdir $@)).slayer.out'

kmdf/toaster/%.sil : $(wildcard $(dir $@)/*.c) $(wildcard $(dir $@)/*.h) $(FRONTEND_EXE)
	@echo $(dir $@)
	-@bash -c ' \
	  source $(TST)/config.sh; \
	  INCLUDE="$(SL_INCLUDE_TOASTER);$$INCLUDE"; \
	  cd $(dir $@); \
	  $(TEST_FE_EXE) $(ARG) *.c &> $(basename $(notdir $@)).slayer.out'

kmdf/pci_drv/pci_drv.sil : $(wildcard $(dir $@)/*.c) $(wildcard $(dir $@)/*.h) $(FRONTEND_EXE)
	@echo $(dir $@)
	-@bash -c ' \
	  source $(TST)/config.sh; \
	  INCLUDE="$(SL_INCLUDE_PCIDRV);$$INCLUDE"; \
	  cd $(dir $@); \
	  $(TEST_FE_EXE) $(ARG) *.c &> $(basename $(notdir $@)).slayer.out'


# analyze sil
%.slayer.out : $(SLAYER_EXE) %.sil bin/gather_results$(EXE)
	@echo $*.sil $(ARG)
	@bin/gather_results$(EXE) $(TestSils)
	-@bash -c ' \
	  if test -e $*.sil; then \
	  cd $(dir $*); \
	  status=$$( $(RUNCMD) )$$?; \
	  case $$status in \
	    ( 0 | 1 | 2 | 235 ) ;; \
	    ( 137 | 152 | 233 ) echo -e "RESULT: TIMEOUT" >> $(notdir $*).slayer.out ;; \
	    ( 236 ) echo -e "RESULT: MEMOUT" >> $(notdir $*).slayer.out ;; \
	    ( 139 ) echo -e "RESULT: CRASH" >> $(notdir $*).slayer.out ;; \
	    ( * ) echo -e "RESULT: Error: "$$status >> $(notdir $*).slayer.out ;; \
	  esac fi'


# compile all c to sil
compile: $(COPY_EXES) $(CompSils)


# gather results
define gather-results =
bin/gather_results$(EXE) $(TestSils)
dos2unix -q RESULT.curr.tsv
cp -p RESULT.curr.html RESULT.$(VER).html
cp -p RESULT.curr.tsv RESULT.$(VER).tsv
cp -p RESULT.curr.txt RESULT.$(VER).txt
endef

# ...without running tests
result: bin/gather_results$(EXE)
	$(gather-results)


# ...after running all tests
test: $(COPY_EXES) $(SlayerOuts) bin/gather_results$(EXE)
	$(gather-results)


cleansil:
	@rm -f $(CompSils)


# remove result files
cleanout:
	@rm -f $(SlayerOuts)
	@$(FIND) . -name "*.dot" -exec rm {} +


# remove result files for TIMEOUTs
cleanTO:
	@$(FIND) . -name '*.slayer.out' -exec bash -c '$(GREP) -q TIMEOUT {} && rm {}' \;

# remove result files for MEMOUTs
cleanMO:
	@$(FIND) . -name '*.slayer.out' -exec bash -c '$(GREP) -q MEMOUT {} && rm {}' \;

# remove result files for HIT LIMITs
cleanHL:
	@$(FIND) * -name "*.slayer.out" -exec bash -c '$(GREP) -q LIMIT {} && rm {}' \;


# branch to test
BRANCH=src
SRC=../../$(BRANCH)/$(BUILD_DIR)

.PHONY: prover_test
prover_test:
	@echo ======= compiling and running Prover tests =======
	bash -c ' \
	cd prover ; \
	for tst in *.ml; do \
	  BASE=`basename -s .ml $$tst`; \
	  echo $$BASE; \
	  ocamlopt.opt -I $(TST)/../tools/Z3/build/api/ml -I $(SRC) -I $(SRC)/UnitTests str.cmxa unix.cmxa z3.cmxa $(SRC)/contaminated/PolySet.cmx $(SRC)/Library.cmx $(SRC)/CLArgs.cmx $(SRC)/PrettyPrinting.cmx $(SRC)/Log.cmx $(SRC)/Variable.cmx $(SRC)/Type.cmx $(SRC)/Expression.cmx $(SRC)/Graph_sig.cmx $(SRC)/Graph.cmx $(SRC)/Substitution.cmx $(SRC)/Timer.cmx $(SRC)/Z3ContextTree.cmx $(SRC)/Pure.cmx $(SRC)/UnitTests/TestGen.cmx $(SRC)/SymbolicHeap.cmx $(SRC)/SIL.cmx $(SRC)/Interproc_sig.cmx $(SRC)/AbstractTransitionSystem.cmx $(SRC)/Discovery.cmx $(SRC)/HeapGraph.cmx $(SRC)/Unification.cmx $(SRC)/UnitTests/TestGenProver.cmx $(SRC)/Prover.cmx $(SRC)/Reachability.cmx $(SRC)/HeapAbstraction.cmx $(SRC)/Abstraction.cmx $(SRC)/Frame.cmx $(SRC)/Interproc.cmx $(SRC)/SymbolicExecution.cmx $(SRC)/Analysis.cmx $(SRC)/CounterExample.cmx $(SRC)/SIL_wf.cmx $(SRC)/Inline.cmx $(SRC)/Predicates.cmx $(SRC)/PredicateEval.cmx $(SRC)/slam/io/fileNames.cmx $(SRC)/slam/io/version.cmx $(SRC)/slam/io/lg.cmx $(SRC)/slam/io/io.cmx $(SRC)/slam/io/sAL.cm√√x $(SRC)/slam/util/util.cmx $(SRC)/slam/ir/error.cmx $(SRC)/slam/ir/ctype.cmx $(SRC)/slam/ir/loc.cmx $(SRC)/slam/ir/pExpr.cmx $(SRC)/slam/ir/arrayField.cmx $(SRC)/slam/ir/irUtil.cmx $(SRC)/slam/ir/nCFG.cmx $(SRC)/slam/util/counter.cmx $(SRC)/slam/ir/compilationUnit.cmx $(SRC)/frontend_slam.cmx $(SRC)/ReconstructSpecs.cmx $(SRC)/Statistics.cmx $$BASE.ml -o $$BASE.native; \
	  ./$$BASE.native &> $$BASE.log; \
	  res=$$?; \
	  rm -f $$BASE.{cmi,cmx,native,o,obj}; \
	done \
	'
	@echo ======= gathering Prover test results =======
	@bin/gather_results$(EXE) -ext ml -r prover


unit:
	@echo ======= running SLAyer stand-alone unit tests =======
	bash -c 'for ut in ../bin/*fail*.native; do \
                   wlimit /q /w $(TIMEOUT) /m $(MEMOUT) $$ut \
	             &> `basename $$ut`.log; \
                   res=$$?; \
                   echo $$ut returned $$res; \
                 done'



# Note: use $(OnlySils) here instead of .../driver.sil
clean:
	@rm -rf _build bin
	@mv kmdf/1394/driver/driver.sil kmdf/1394/driver/driver_sil 2>/dev/null; true
	@$(FIND) -regex '.*/slam\.\(li\|db\|f\|fptr\|log\|newpa\|newpa\.dot\|pa\|pa\.dot\|dump/.*\|dump.irdump\)\|.*/slam_acfg\|.*/slam_entrypoints.txt\|.*/slam_watch_starts.txt\|.*/slamcl\.out\|.*/slamresult\.txt\|.*/slam_watch_pairs\.txt\|.*/unreachable_funs.txt\|.*/slamstats\.txt\|.*\.dot\|.*\.dot.pdf\|.*\.i\|.*\.i\.orig\|.*\.li\|.*\.sil\|slam\.log\|.*\.slayer\.out\|.*\.z3\|.*\.z3\.cpp\|.*\.obj\|.*\.tt\|.*\.t2\|.*/ERRORS\.txt\|.*/OUTPUT\.txt\|.*~\|.*/\(abs\|ent\|sub\|sat\|norm\)_.*\.ml\|\./[^Z]*/.*.smt\|.*\.rawcfgf' -delete
	@mv kmdf/1394/driver/driver_sil kmdf/1394/driver/driver.sil 2>/dev/null; true


# print any variable for Makefile debugging
print-%:
	@echo '$*=$($*)'
