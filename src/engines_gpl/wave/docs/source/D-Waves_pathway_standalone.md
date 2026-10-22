# Standalone D-Waves communication model

**D-Waves owns the calculation schedule. No Delft3D4 or D-Flow FM engine
is launched or advanced.** D-Waves can run independently, read saved
Delft3D4 or D-Flow FM fields, write wave fields onto the Delft3D4 or
D-Flow FM grid, or combine the last two options.

![Standalone communication model](../include/dwaves_pathway_standalone_detailed.svg)

[DOT source](../include/dwaves_pathway_standalone_detailed.dot)

## Participants and channels

| Participant / resource | Responsibility |
|---|---|
| D-Waves manager | Select times, prepare inputs, invoke SWAN, collect and write results. |
| SWAN | Solve waves for each configured grid; exchange staged input/output files with D-Waves, including in library mode. |
| Optional existing Delft3D4 or D-Flow FM com-files | Supply saved geometry and selected hydrodynamic fields. Accept wave fields only with `WriteCOM`. |
| Boundary, forcing and D-Waves output files | Supply independent forcing and store configured wave results. |

Reading hydrodynamic fields and writing wave fields are independent choices.
There is no live handshake and no feedback to an advancing Delft3D4 or
D-Flow FM engine.
Even with `WriteCOM`, this is an offline file workflow, not online coupling.

## Calculation cycle and grids

1. Read configuration and SWAN grids. If hydrodynamic input or output on the
   Delft3D4/D-Flow FM grid is requested, read the existing geometry and
   prepare both mapping directions.
2. For each selected calculation time, visit SWAN grids in configured order.
   Prepare inputs, run SWAN, read results and accumulate any output on the
   Delft3D4/D-Flow FM grid.
   Nested domains use the parent-generated boundary files.
3. After the grid loop, write accumulated wave fields to the com-files if
   `WriteCOM` is enabled; configured D-Waves outputs are also produced.
4. Continue with the next selected time, then finalize.

The time selection is route-dependent: the Delft3D4 route processes MDW
times in `swan_tot`; the standalone D-Flow FM-file route selects MDW times within
the supplied step interval (or its no-explicit-times branch).
The executable calls `wave_main_step(-1)`: **this is not a universal
"run all MDW times" instruction for the D-Flow FM-file route.**

| Saved-data route | Configuration | Runtime remapping |
|---|---|---|
| Delft3D4 NEFIS com-files | Mode 0; leave `[General] FlowFile` blank. `[General] ComFile` identifies runids, or the D-Waves runid is used. | `grmap` applies internal interpolation tables in both directions. |
| D-Flow FM NetCDF com-files | `[Output] COMFile` identifies the existing communication file and partition variants. An arbitrary D-Flow FM map file is not a substitute. | D-Flow FM→SWAN uses `grmap_esmf`; SWAN→D-Flow FM uses `grmap`. |

Weights are prepared once per grid pair per initialization and reused.
Both directional maps are built when Delft3D4 or D-Flow FM grids exist, even in
read-only use; **applying** the reverse map and writing wave fields require
`WriteCOM`. The hydrodynamic and SWAN grids need not match, but coordinates
and required coverage must.
External meteo is a separate optional forcing source; configured external
wind takes precedence over the wind from the Delft3D4 or D-Flow FM files.

## Parallel execution

SWAN grids/nests, Delft3D4 or D-Flow FM partitions and MPI ranks are different
counts.
D-Waves visits the configured SWAN grids sequentially; nesting is not MPI
decomposition. With `SwanMode=lib`, the D-Waves master handles mapping and file
I/O and calls SWAN collectively with the other D-Waves ranks. Workers wait for
SWAN commands and synchronize before the master reads/deletes SWAN files.
`SwanMode=lib` requires an MPI-enabled D-Waves run; the default executable
mode launches SWAN through a script.

## Implementation links

| Responsibility | Entry points |
|---|---|
| Lifecycle and time selection | [executable](../../packages/wave/src/wave_exe.f90), [wave_init](../../packages/manager/src/wave_main.f90#L131), [wave_master_step](../../packages/manager/src/wave_main.f90#L405) |
| Existing file selection | [flow_init](../../packages/data/src/flow_data.f90#L79) |
| Both map directions | [grids_and_gridmaps](../../packages/kernel/src/grids_and_gridmaps.f90#L165), [make_grid_map](../../packages/data/src/swan_flow_grid_maps.f90#L306) |
| Time/grid loops and file exchange | [swan_tot](../../packages/manager/src/swan_tot.f90#L106), [get_flow_fields](../../packages/io/src/get_flow_fields.f90), [put_wave_fields](../../packages/io/src/put_wave_fields.f90) |
| SWAN execution and MPI workers | [run_swan](../../packages/kernel/src/run_swan.f90#L90), [run_swan_slave](../../packages/kernel/src/run_swan_slave.f90#L53), [master/worker dispatch](../../packages/manager/src/wave_main.f90#L385) |

Configuration reference: [D-Waves User Manual](../include/D-Waves_User_Manual.pdf),
Sections 5.4.1.1 and 5.4.9 (GUI), Appendix A.1.1 (MDW keywords), A.1.2
(Delft3D4 offline workflow) and A.1.3.4.2 (D-Flow FM com-file example).
