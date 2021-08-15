TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

EXECTUABLE_C = c/C$(exeext)
EXECTUABLE_D = d/D$(exeext)
CLEAN_FILES = a/A.o a/A.hi a/A.hie b/B.o b/B.hi b/B.hie c/C.o c/C.hi c/C.hie d/C.o d/C.hi d/C.hie $(EXECTUABLE_C) $(EXECTUABLE_D)

clean:
	$(RM) $(CLEAN_FILES)

multipleHomeUnits_single1: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) -unit @unitA

multipleHomeUnits_single2: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) -unit @unitB

multipleHomeUnits_single3: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) -unit @unitC
	./$(EXECTUABLE_C)

multipleHomeUnits_single4: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) -unit @unitD
	./$(EXECTUABLE_D)

multipleHomeUnits_single5: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) -unit @unitTH

multipleHomeUnits_cpp: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) -unit @unitCPPIncludes -v0

multipleHomeUnits_cpp2: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) -unit @unitCPPIncludes -unit @unitCPPImport  -v0

multipleHomeUnits_cfile: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) -unit @unitCFile -v0

multipleHomeUnits_callstack: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) -unit @unitCallstack -v0
	./callstack/Main || true


multipleHomeUnits001: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) -unit @unitA -unit @unitB

multipleHomeUnits002: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) -unit @unitC -unit @unitD
	./$(EXECTUABLE_C)
	./$(EXECTUABLE_D)

multipleHomeUnits003: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) -unit @unitA -unit @unitB -unit @unitC -unit @unitD
	./$(EXECTUABLE_C)
	./$(EXECTUABLE_D)

multipleHomeUnits004: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) -unit @unitB -unit @unitE

multipleHomeUnits004_recomp: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) -unit @unitB -unit @unitE
	'$(TEST_HC)' $(TEST_HC_OPTS) -unit @unitB -unit @unitE

multipleHomeUnitsModuleVisibility: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) -unit @unitMV -unit @unitMV-import || true

multipleHomeUnitsPackageImports: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) -unit @unitPI -unit @unitB -unit @unitB2

