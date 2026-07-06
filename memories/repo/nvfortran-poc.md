# nvfortran (NVHPC) PoC build of D-Flow FM

Goal: compile dflowfm with nvfortran 26.3; keep changes mergeable (Intel/GNU builds unaffected).
Branch: fm/task/UNST-XXXX_nvfortran_poc. Working in nvhpc devcontainer.

## Toolchain (nvhpc devcontainer, Ubuntu 22.04, NVHPC 26.3)
- C/C++ = gcc/g++ 11.4; Fortran = nvfortran 26.3; MPI = bundled OpenMPI (mpicc/mpicxx/mpif90 wrappers)
- conan + cmake installed via `uv tool install conan cmake` (system cmake 3.22 too old, need >=3.30)
- gtest via apt: libgtest-dev
- python is `python3.14` (no plain `python`/`python3` on PATH)
- REQUIRED env for every build/conan cmd:
  export DELFT3D_CONAN_PROFILE=delft3d_ubuntu22_nvhpc OMPI_CC=gcc OMPI_CXX=g++ OMPI_FC=nvfortran

## Build commands
- conan deps (already done, all built OK):
  python3.14 run_conan.py install --rebuild-packages --build-type Release --output-folder build_dflowfm_release/conan
- configure (keep-build reuses conan):
  python3.14 build.py --config dflowfm --build-type Debug --build-dir build_dflowfm_debug --keep-build
- build all, keep going to collect every error:
  cmake --build build_dflowfm_debug --config Debug --parallel -- -k
- single-file verbose compile (best triage; prints exact nvfortran cmd + error):
  cd build_dflowfm_debug && make -f <lib>/CMakeFiles/<lib>.dir/build.make <lib>/CMakeFiles/<lib>.dir/<relpath>.f90.o VERBOSE=1
  (object target = <lib>/CMakeFiles/<lib>.dir/<source-relpath>.f90.o)

## Infra files (committed in 33ce03f)
- conan/config/profiles/delft3d_ubuntu22_nvhpc: gcc11, compiler_executables mpicc/mpicxx/mpif90 (so mpi.h found)
- conan/config/settings_user.yml: added Ubuntu22 distro
- run_conan.py: LINUX_PROFILE overridable via DELFT3D_CONAN_PROFILE env
- .devcontainer/delft3d-nvhpc/Dockerfile: exports DELFT3D_CONAN_PROFILE
- ci/dockerfiles/linux/third-party-libs-nvhpc.Dockerfile: NVHPC 26.3 base etc

## Fixes committed (3 commits on top of 33ce03f)
1. (285551fc09) implicit none(type,external): nvfortran 26.3 cannot parse F2018 specifier list, NO flag exists.
   Fix in src/cmake/compiler_options/nvhpc.cmake: add_compile_options for Fortran:
     "$<$<COMPILE_LANGUAGE:Fortran>:-Mpreprocess>" and "$<$<COMPILE_LANGUAGE:Fortran>:-Dnone(...)=none>"
   Function-like macro 'none(...)'->'none' collapses 'implicit none(type,external)'->'implicit none';
   bare 'implicit none' (no parens) untouched. add_compile_options (not CMAKE_Fortran_FLAGS) so CMake
   quotes the parens for the shell. Forces preprocessing of ALL Fortran (.f90 too), nvhpc-only.
2. (a86990f27e) spherepack: dflowfm only uses shaec/shaeci/shsec/shseci (meteo1.f90 selfattraction).
   Full lib fails nvfortran (0074 proc-arg iface mismatch passing vhsgs as procedure(vector_synthesis);
   0310/0038 assumed-shape automatic bounds). FIX: if(NVHPC) in spherepack cmake_deltares/CMakeLists.txt
   subset sources to 11-file scalar closure; redirect meteo1 `use spherepack` ->
   `use scalar_analysis_routines, only: shaec,shaeci` + `use scalar_synthesis_routines, only: shsec,shseci`
   (compiler-agnostic; those modules exist in full build too). spherepack target builds OK.
3. (b57b6eee81) stdlib_sorting: nvfortran FAILS to host-associate PRIVATE parent-module entities into a
   nested (contained) procedure of a submodule's module-procedure. max_merge_stack(param) AND run_type
   (both private in stdlib_sorting.f90) -> 0038/0310 inside merge_sort in stdlib_sorting_ord_sort.f90 +
   stdlib_sorting_sort_index.f90. NOT a log()/associate issue (ruled out via minimal repros).
   FIX: make max_merge_stack and run_type `public` in stdlib_sorting.f90 (one file fixes both submodules).
   GENERAL LESSON: under nvfortran, any parent-module entity used in a nested contained procedure of a
   submodule module-procedure must be PUBLIC.

## WORKFLOW PREFERENCE (user, important)
- Commit strategy and verification are user-driven for now; do not assume autonomous compile/test validation is required before reporting code edits.

## More fixes committed
4. m_alloc.f90: NOT the private/public family. m_alloc `use`s reallocP from BOTH m_alloc_generated (has it)
   and m_alloc_handwritten (does NOT define/export reallocP). Intel/GNU tolerate the bogus only-name;
   nvfortran errors 0084. FIX: drop reallocP from the m_alloc_handwritten use-only list. Compiler-agnostic.
5. (0383b04463) shapelib.F90: nvfortran mis-counts NULL() pointer comps in a PARAMETER structure ctor
   (0066 too few data constants), even via type defaults. FIX: give nvertices a default(=0) and make
   shpobject_null an ordinary default-initialized module var (drop PARAMETER). Compiler-agnostic.
6. (997d67ffe6) -Mbackslash in nvhpc.cmake: nvfortran defaults to C-escape in char literals so '\\' ->
   0026 unmatched quote. Flag name INVERTED: -Mbackslash = treat backslash literally (what we want);
   -Mnobackslash = keep escaping. NOT a preprocessor issue (plain nvfortran fails on '\\' too).
7. FLAP Data_Type_Command_Line_Interface.F90:~2571: nvfortran 0155 cannot resolve generic get_args for
   keyword call get_args(ai=ai). FIX: bypass generic, call specific bindings directly
   (get_args_from_string / get_args_from_invocation). Compiler-agnostic. Verified compiles.

## gtest_discover_tests runtime lib error (NOT a compile error)
- Symptom: build fails at a test target with "error while loading shared libraries: libproj.so.25"
  (or other conan .so). CMake gtest_discover_tests RUNS each freshly-built test exe to list cases;
  that run needs conan shared libs on LD_LIBRARY_PATH. Nothing needs them at compile time.
- FIX: source the conan-generated runtime env before building:
    source build_<dir>/conan/generators/conanrun.sh
  Already wired into the 3 build tasks in .vscode/tasks.json (local/gitignored).

## More compiler fixes committed (all verified by single-file compile, compiler-agnostic)
- waq delwaq_buffer.f90 (8bc1b30842): char(len=1) scalar -> char(len=20) array seq-association
  (0188). FIX: use transfer(); dropped convert() helper.
- waq process_registration.f90 (bd679f2a11): bare proc name as struct-ctor value for procedure-ptr
  component (0084 'SUBROUTINE as FUNCTION'). FIX: allocate + per-element %pronam= / %procpnt=>routine
  (177 entries, generated mechanically). 3 name!=routine: AGECAR->AGECART, PRTMRT->protist_mortality_salinity,
  DREDGE->dredge_process.
- morphology bedcomposition_module.f90 (e44718cfa5): alloc/deallocwork had this intent(in) but
  (de)allocate this%work%...(pointer-component target) -> 0155. FIX: drop intent(in) on this.
- gridgeom gridoperations.F90 (127aebac75): 'elemental recursive' INCELLS (0460). FIX: drop recursive;
  elemental implies pure implies no SAVE locals, so XH/YH already per-invocation.
- flow1d structures.f90 (686b67edad): VALUE on assumed-length char dummy (0155 -> cascade 0089 whole
  module). FIX: intent(in) + local copy of len(string), lowercase the copy.

## More compiler fixes committed (batch 2, all verified single-file, compiler-agnostic)
- test_cross_section.f90 (63bbdaa6f3): rank-2 array initialized from flat (/.../) constructor (0155
  "shape of initializer does not match"). It's a RANK problem, not a count problem: array constructor
  is always rank-1; Intel/gfortran tolerate rank-1->rank-2 as extension. FIX: wrap all 4 refdata
  ctors in reshape(..., (/n,m/)). Element order already column-major so unchanged.
- dfm_volume_tool_main.F90 (f2e7c38bfd): c_f_pointer(xptr, shapearray) with array fptr but NO shape arg
  (0074). Standard requires SHAPE when fptr is array; ifort tolerated omission. FIX: add (/MAXDIMS/).
  NOTE: empirically confirmed nvfortran DOES accept non-interoperable derived-type array c_f_pointer,
  so it was NOT an interop restriction.
- unstruc_bmi.F90 set_var (3b0d0624e4): nvfortran 26.3 ICE (F-0000 "Deferred-length character symbol
  must have descriptor") on deferred-length chars. Two spots: (a) local allocatable `levels` (verbose
  case) -> replaced with fixed-length MAXSTRLEN buffer filled by index (identical behaviour all
  compilers); (b) COSUMO nf_const_operator (m_nearfield, character(:),dimension(:),pointer) alloc+
  transfer -> genuine nvfortran limitation, guarded both nf_const_operator cases with
  #ifndef __NVCOMPILER / #else LEVEL_ERROR / #endif. Intel/GNU keep full COSUMO functionality.
  Remaining: 1 non-fatal W-0469 at line ~1328 (c_loc on multi-char string, e.g. c_loc(md_ident)).

## single-file compile recipe (mpif90 direct, used in batch 2)
- read Fortran_FLAGS/Fortran_DEFINES/Fortran_INCLUDES from <target>/CMakeFiles/<tgt>.dir/flags.make
- strip the hard-coded module dir: sed 's#-module ../fortran_module_dir##'
- mkdir -p /tmp/<d>; mpif90 <FLAGS> <DEFS> <INCS> -I build_dflowfm_debug/fortran_module_dir
    -module /tmp/<d> -c <src> -o /tmp/<d>.o; check EXIT=${PIPESTATUS[0]}
- the -Dnone(...)=none warning when splicing DEFINES is benign

## nvfortran 26.3 limitation catalog (patterns to reuse)
- deferred-length character (allocatable/pointer) code-gen -> ICE. Fix: fixed-length buffer, or guard
  with #ifdef __NVCOMPILER if the feature genuinely needs deferred length.
- c_f_pointer with array fptr REQUIRES explicit shape arg.
- rank-1 (/.../) ctor into rank-2 array REQUIRES reshape.
- parent-module entity used in nested contained proc of a submodule module-proc must be PUBLIC.
- BIND(C) on non-interoperable procedures can compile but crash at runtime with nvfortran (seen in gtest helper using allocatable dummies and derived type with allocatable components). Keep BIND(C) only on actual C-entry test routines; remove from internal helper routines.


- m_longculverts.f90 convertLongCulvertsAsNetwork: structures output path called prop_write_inifile but did not close unit; crsdef path did. nvfortran runtime kept buffered write -> empty/partial file seen. FIX: add close(mout) after writing structures_output.

## compaction / memory strategy (user asked 2026-06-30)
- Compaction is from-scratch each time (re-summarizes whole window incl. previous summary), not
  incremental -> costs latency+tokens proportional to window fullness. Advice given: compact earlier/
  more often (smaller), lean on this memory file for durable facts, rely on transcript path for exact
  old detail. This file IS the durable store; keep it updated so future compactions stay light.

## NEXT
- HEAD = 3b0d0624e4 on branch fm/task/UNST-XXXX_nvfortran_poc.
- Run full keep-going build to find next error:
    [ -f build_dflowfm_debug/conan/generators/conanrun.sh ] && source build_dflowfm_debug/conan/generators/conanrun.sh
    cmake --build build_dflowfm_debug --config Debug --parallel
  (user thought BMI/dflowfm_dll was likely the last blocker; verify by full build/link.)
- Optional: address the W-0469 c_loc-on-multichar warning in unstruc_bmi.F90 if desired (non-fatal).
- getprof_1d.f90: GetCSParsTotal call with hysteresis_for_summerdike(:,LL) could hit OOB when LL not in [1,size(hysteresis,2)] (observed under nvfortran with stricter/runtime bounds behavior). FIX: guard allocation+bounds; fallback to local hysteresis(2)=.true. when out-of-range.
