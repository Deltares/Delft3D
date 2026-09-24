# PETSc solver replay

A standalone C++ benchmark that loads every D-Flow FM PETSc linear-system dump
from a run, solves each system once with configurable PETSc
settings, and compares the results with the recorded reference solutions.

Each numbered solve file contains the matrix, right-hand side, initial solution,
and reference solution. The dump prefix also identifies two shared PETSc vector
files containing the global node IDs and original owner ranks. See
[`petsc_replay_dump.md`](../src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_utils/petsc_replay_dump.md)
for the producer-side format.

## Configure and build

The Conan recipe pins the same `petsc/3.25.3` package as the Delft3D root
recipe. Delft3D uses Release dependency packages for every consumer
configuration. From a Visual Studio developer shell on Windows, generate the
Release dependency configuration and the Debug consumer mapping in the same
multi-config build directory:

```powershell
conan install petsc_solver_replay `
  --output-folder=petsc_solver_replay/build `
  --profile:all=$env:CONAN_DEFAULT_PROFILE `
  --settings:all build_type=Release

conan install petsc_solver_replay `
  --output-folder=petsc_solver_replay/build `
  --profile:all=$env:CONAN_DEFAULT_PROFILE `
  --settings:all build_type=Release `
  --settings:all "&:build_type=Debug"

$toolchain = (Resolve-Path petsc_solver_replay/build/conan/conan_toolchain.cmake).Path
cmake -S petsc_solver_replay -B petsc_solver_replay/build `
  "-DCMAKE_TOOLCHAIN_FILE=$toolchain"

cmake --build petsc_solver_replay/build --config Debug --parallel
ctest --test-dir petsc_solver_replay/build -C Debug --output-on-failure
```

The same configured directory builds Release by changing the configuration:

```powershell
cmake --build petsc_solver_replay/build --config Release --parallel
ctest --test-dir petsc_solver_replay/build -C Release --output-on-failure
```

Release builds should be used for performance measurements.

## Run

On Windows, add the PETSc and Intel MPI DLL directories to `PATH`. The generated
`conanrun.bat` adds PETSc; Intel MPI is supplied by oneAPI:

```cmd
call petsc_solver_replay\build\conan\conanrun.bat
set PATH=%ONEAPI_ROOT%\mpi\latest\bin;%PATH%
```

```powershell
mpiexec -n 4 petsc_solver_replay/build/Release/petsc-solver-replay.exe `
  -replay_file C:\path\flow_solve `
  -replay_node_owners C:\other_run\flow_solve_owner_ranks.bin `
  -ksp_type cg `
  -pc_type asm `
  -ksp_rtol 1e-14
```

Run under the desired MPI launcher to test another partition count. All normal
PETSc KSP and PC options are accepted.

Replay-specific options:

- `-replay_file <prefix>`: required input. The dump prefix, such as
  `C:\path\flow_solve`, replays all matching `<prefix>_*.bin` files once in
  filename order. The zero-padded dump sequence numbers therefore preserve the
  original FM solve order. An individual `.bin` file is not accepted.
- `-replay_node_owners <path>`: optional FM-generated
  `<prefix>_owner_ranks.bin` file defining the replay partition. Replay also
  loads the sibling `<prefix>_global_node_ids.bin` and joins both vectors by
  global node ID, so the metadata may come from another FM run with a different
  row ordering. Every node in the replayed system must be present, and each
  owner rank must be valid for the MPI process count used to launch replay.
  Without this option, PETSc uses its default contiguous partition of the
  recorded ordering.
- `-replay_rebuild_preconditioner <n>`: number of completed linear solves for
  which a preconditioner is kept before rebuilding it. The default `1` rebuilds
  before every subsequent solve, `0` keeps the first preconditioner
  indefinitely, and larger values rebuild after every `n` solves. The count spans
  consecutive dump files. Reuse requires compatible matrix layouts and
  preconditioner support for changed operators.
- `-replay_reference_rtol <value>`: relative comparison tolerance, default
  `1e-10`.
- `-replay_fail_on_reference_error <bool>`: return failure when the tolerance is
  exceeded, default `false` so deliberately different solver settings can still
  be benchmarked.

Reported load, setup, and solve durations are maximum wall-clock times across
MPI ranks. File loading is reported separately from solver setup and execution,
so both one-shot cost and steady-state solver performance remain visible.
The summary also reports total iterations and the maximum iteration count for
an individual solve.
Every dump is solved exactly once, matching the sequence of linear solves in
the recorded FM simulation.
