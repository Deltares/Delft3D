# PETSc linear-system replay dumps

Set `DFLOWFM_PETSC_DUMP_PREFIX` before starting D-Flow FM to record every PETSc
linear solve. For example, a prefix of `C:\temp\flow_solve` creates:

```text
C:\temp\flow_solve_00000001.bin
C:\temp\flow_solve_00000002.bin
...
```

The parent directory must already exist. Dumping is disabled when the variable
is unset or empty.

Each file is a collective PETSc binary stream written with
`PetscViewerBinaryOpen`. Objects occur in this fixed order:

1. `Mat`: assembled operator (`A`).
2. `Vec`: right-hand side (`b`).
3. `Vec`: initial solution passed to `KSPSolve` (`x_initial`).
4. `Vec`: solution returned by `KSPSolve` (`x_reference`).

A replay program must call `MatLoad` once and `VecLoad` three times in that
order. PETSc can load the matrix and vectors with a different MPI process
count.

The following immutable metadata is written once per dump prefix:

- `<prefix>_global_node_ids.bin`: one `Vec` containing the one-based D-Flow FM
   global node/cell number for every equation row.
- `<prefix>_owner_ranks.bin`: one `Vec` containing the zero-based MPI rank that
   owned every equation row in the recorded run.

These companion files preserve the original row identity and partition without
duplicating them in every numbered solve file. The solver replay can use the
pair from another FM run as a replacement node-owner map, joining by global node
ID to redistribute the systems for that run's MPI partition.

Compare a replay result against `x_reference`, preferably using both an
absolute norm and a relative norm. Solver settings are deliberately not stored
as part of the system, so they can be supplied to the replay KSP through the
PETSc options database.
