@echo off
setlocal

uv run petsc_solver_replay\benchmark.py ^
    --output=benchmarks\dcsm\md6\pc01 ^
    --executable=petsc_solver_replay\install\bin\petsc-solver-replay.exe ^
    --mpi-processes=4 ^
    --replay-file=petsc_dump_dcsm_md6\flow_solve ^
    --node-owners=petsc_dump_dcsm_md6\flow_solve_owner_ranks.bin ^
    --runs=9 ^
    --rebuild-preconditioner=1

uv run petsc_solver_replay\benchmark.py ^
    --output=benchmarks\dcsm\md6\pc10 ^
    --executable=petsc_solver_replay\install\bin\petsc-solver-replay.exe ^
    --mpi-processes=4 ^
    --replay-file=petsc_dump_dcsm_md6\flow_solve ^
    --node-owners=petsc_dump_dcsm_md6\flow_solve_owner_ranks.bin ^
    --runs=9 ^
    --rebuild-preconditioner=10

uv run petsc_solver_replay\benchmark.py ^
    --output=benchmarks\dcsm\md0\pc01 ^
    --executable=petsc_solver_replay\install\bin\petsc-solver-replay.exe ^
    --mpi-processes=4 ^
    --replay-file=petsc_dump_dcsm_md0\flow_solve ^
    --node-owners=petsc_dump_dcsm_md0\flow_solve_owner_ranks.bin ^
    --runs=9 ^
    --rebuild-preconditioner=1

uv run petsc_solver_replay\benchmark.py ^
    --output=benchmarks\dcsm\md0\pc10 ^
    --executable=petsc_solver_replay\install\bin\petsc-solver-replay.exe ^
    --mpi-processes=4 ^
    --replay-file=petsc_dump_dcsm_md0\flow_solve ^
    --node-owners=petsc_dump_dcsm_md0\flow_solve_owner_ranks.bin ^
    --runs=9 ^
    --rebuild-preconditioner=10
