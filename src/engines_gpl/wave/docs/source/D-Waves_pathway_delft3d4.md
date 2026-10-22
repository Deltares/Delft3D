# Delft3D4-D-Waves communication model

**Delft3D4 owns the time loop. DelftIO carries commands/status;
Delft3D4 NEFIS com-files carry fields.** Delft3D4 and D-Waves are separately
launched processes. This is not the D-Flow FM/DIMR route.

![Delft3D4 communication model](../include/dwaves_pathway_delft3d4_detailed.svg)

[DOT source](../include/dwaves_pathway_delft3d4_detailed.dot)

## Participants and channels

| Channel | Sender/writer | Receiver/reader | Contents |
|---|---|---|---|
| DelftIO | Delft3D4 | D-Waves | Initialization metadata, step/time and finalize commands. |
| DelftIO | D-Waves | Delft3D4 | Calculation completion/status. |
| Delft3D4 NEFIS com-files | Delft3D4 | D-Waves | Selected hydrodynamic fields on Delft3D4 grids. |
| Same com-files | D-Waves, with `WriteCOM` | Delft3D4 | Wave fields already mapped onto Delft3D4 grids. |
| D-Waves/SWAN staged files | D-Waves / SWAN | SWAN / D-Waves | Solver inputs, nesting boundaries and results; internal to D-Waves execution. |

`WaveOL` configures Delft3D4's online behavior; it does not launch D-Waves.
The launcher starts both processes. Mode 1 and a blank D-Flow FM
`[Output] COMFile` select the classic D-Waves handshake.

**Com-file format.** Delft3D4's `FLNcdf` keyword can make the Delft3D4 map,
Fourier, history and drogue files NetCDF, but the com-file cannot: Delft3D4
stops when `com` or `all` is listed. The D-Waves coupling com-file is therefore
always NEFIS (`com-<runid>.dat/def`). D-Waves' NetCDF com-file reader expects
the D-Flow FM unstructured layout and is not a Delft3D4 route.

## Startup and repeated exchange

1. Delft3D4 initializes its DelftIO connection; D-Waves receives domain
   identifiers and time metadata, reads geometry and prepares maps.
2. Delft3D4 advances internal timesteps to a configured wave-coupling point.
   All participating Delft3D4 domains update their com-files before the master
   requests a D-Waves calculation.
3. D-Waves receives the step/time, reads selected Delft3D4 fields and maps them to
   each SWAN grid. It runs the grids in configured order, including nested
   computations using parent boundary output.
4. D-Waves accumulates mapped results on Delft3D4 grids and writes the com-files
   (`WriteCOM` is mandatory online). It then reports completion through DelftIO.
5. Delft3D4's command call returns; it reads wave fields through `setwav` at
   its read point and continues its time loop. D-Waves waits for the next
   command. Delft3D4 sends finalize at the end.

The coupling interval need not equal Delft3D4's numerical timestep. The
com-files are the shared field store, not a replacement for synchronization.
Delft3D4 input selections gate which fields D-Waves reads; the writeback path is
always active online because `WriteCOM` is mandatory.
For this Delft3D4 pathway, D-Waves starts SWAN as a separate executable, which
uses OpenMP for parallelization. This is distinct from Delft3D4 domain
partitioning; the DelftIO command/status exchange does not run SWAN as a
D-Waves MPI library collective.

## Mapping and parallel execution

D-Waves prepares both mapping directions per Delft3D4/SWAN grid pair using
`make_grid_map`/`mkmap`; `grmap` applies the stored weights each calculation.
Delft3D4 and SWAN grids may differ in resolution and extent.

For partitioned Delft3D4 runs, all domain com-files must be ready; the master
partition performs the D-Waves command/status exchange while other partitions
wait. This Delft3D4 partition coordination is separate from SWAN's OpenMP
parallelism. The SWAN-grid/nest loop remains sequential; nests are not
Delft3D4 partitions or MPI ranks.

## Fluid-mud extension: preserve both dependencies

Mode 2 adds a **second connection to the mud model**. D-Waves waits for both
water and mud commands, reads mud density, viscosity, depth and level,
and reports completion to both. The implementation requires one water
domain and one mud domain with identical grids. This restriction concerns
the water/mud grids, not the SWAN grid.

This is a classic Delft3D4 extension, not a D-Flow FM/BMI mud capability or a
general suspended-sediment option. Mode parsing alone does not establish
the required mud connection; the D-Flow FM file branch does not initialize it.

## Implementation links

| Responsibility | Entry points |
|---|---|
| Launch and Delft3D4 initialization | [launcher](../../../flow2d3d/scripts/run_dflow2d3d_dwaves.sh#L136), [trisim](../../../flow2d3d/packages/flow2d3d_manager/src/trisim.F90#L131), [tricom_init](../../../flow2d3d/packages/flow2d3d_manager/src/tricom_init.F90#L1478) |
| Delft3D4 writes, synchronization and wave reads | [tricom_step: postpr and rwbotc](../../../flow2d3d/packages/flow2d3d_manager/src/tricom_step.F90#L670), [master command / partition wait](../../../flow2d3d/packages/flow2d3d_manager/src/tricom_step.F90#L734), [setwav call](../../../flow2d3d/packages/flow2d3d_manager/src/tricom_step.F90#L826) |
| Command/status protocol | [sync_flowwave](../../../../utils_lgpl/delftio/packages/delftio/src/delftio_sync/sync_flowwave.f90#L330), [wave_master_step](../../packages/manager/src/wave_main.f90#L405) |
| Remapping and grid loop | [grids_and_gridmaps](../../packages/kernel/src/grids_and_gridmaps.f90#L165), [get_flow_fields](../../packages/io/src/get_flow_fields.f90#L102), [swan_tot](../../packages/manager/src/swan_tot.f90#L106) |
| Water/mud dependencies | [connection setup](../../packages/data/src/flow_data.f90#L139), [mud-field reads](../../packages/io/src/get_flow_fields.f90#L317), [SWAN mud input](../../packages/data/src/swan_input.f90#L3676) |
| Com-file format (NEFIS only) | [FLNcdf parsing](../../../flow2d3d/packages/flow2d3d_io/src/input/rdprfl.f90#L140), [com rejected](../../../flow2d3d/packages/flow2d3d_io/src/input/rdprfl.f90#L162), [D-Waves NEFIS writeback branch](../../packages/io/src/put_wave_fields.f90#L18), [D-Waves NetCDF reader (D-Flow FM layout)](../../packages/io/src/get_var_netcdf.f90#L128) |
| D-Waves MPI coordination | [master/worker dispatch](../../packages/manager/src/wave_main.f90#L385), [run_swan](../../packages/kernel/src/run_swan.f90#L90), [worker barrier](../../packages/kernel/src/run_swan_slave.f90#L60) |

Configuration reference: [D-Waves User Manual](../include/D-Waves_User_Manual.pdf),
Appendix A.1.1 (`FlowFile` and `FlowMudFile`).
