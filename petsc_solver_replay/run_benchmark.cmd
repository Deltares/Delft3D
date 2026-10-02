@echo off
setlocal

uv run petsc_solver_replay\benchmark.py ^
    --output=benchmarks\rijn\md6\pc01 ^
    --executable=petsc_solver_replay\install\bin\petsc-solver-replay.exe ^
    --mpi-processes=4 ^
    --replay-file=petsc_dump_rijn_restart_md6\flow_solve ^
    --node-owners=petsc_dump_rijn_restart_md6\flow_solve_owner_ranks.bin ^
    --runs=9 ^
    --rebuild-preconditioner=1

uv run petsc_solver_replay\benchmark.py ^
    --output=benchmarks\rijn\md6\pc10 ^
    --executable=petsc_solver_replay\install\bin\petsc-solver-replay.exe ^
    --mpi-processes=4 ^
    --replay-file=petsc_dump_rijn_restart_md6\flow_solve ^
    --node-owners=petsc_dump_rijn_restart_md6\flow_solve_owner_ranks.bin ^
    --runs=9 ^
    --rebuild-preconditioner=10
