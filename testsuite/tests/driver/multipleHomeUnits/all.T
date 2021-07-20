test('multipleHomeUnits_single1', [extra_files([ 'a/', 'unitA'])], makefile_test, [])
test('multipleHomeUnits_single2', [extra_files([ 'b/', 'unitB'])], makefile_test, [])
test('multipleHomeUnits_single3', [extra_files([ 'c/', 'unitC'])], makefile_test, [])
test('multipleHomeUnits_single4', [extra_files([ 'd/', 'unitD'])], makefile_test, [])
test('multipleHomeUnits_single5', [extra_files([ 'th/', 'unitTH'])], makefile_test, [])
test('multipleHomeUnits_cpp', [extra_files([ 'cpp-includes/', 'unitCPPIncludes'])], makefile_test, [])
test('multipleHomeUnits_cfile', [extra_files([ 'c-file/', 'unitCFile'])], makefile_test, [])
test('multipleHomeUnits_callstack', [extra_files([ 'callstack/', 'unitCallstack'])], makefile_test, [])

test('multipleHomeUnits001',
    [ extra_files(
        [ 'a/', 'b/'
        , 'unitA', 'unitB'])
    ], makefile_test, [])

test('multipleHomeUnits002',
    [ extra_files(
        [ 'c/', 'd/'
        , 'unitC', 'unitD'])
    ], makefile_test, [])

test('multipleHomeUnits003',
    [ extra_files(
        [ 'a/', 'b/', 'c/', 'd/'
        , 'unitA', 'unitB', 'unitC', 'unitD'])
    ], makefile_test, [])

test('multipleHomeUnits004',
    [ extra_files(
        [ 'b/', 'e/'
        , 'unitB', 'unitE'])
    ], makefile_test, [])

test('multipleHomeUnits004_recomp',
    [ extra_files(
        [ 'b/', 'e/'
        , 'unitB', 'unitE'])
    ], makefile_test, [])

test('multipleHomeUnitsModuleVisibility',
    [ extra_files(
        [ 'module-visibility/', 'module-visibility-import/'
        , 'unitMV', 'unitMV-import'])
    ], makefile_test, [])

test('multipleHomeUnitsPackageImports',
    [ extra_files(
        [ 'b/', 'package-imports/'
        , 'unitB', 'unitPI'])
    ], makefile_test, [])
