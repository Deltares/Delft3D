# D-Flow FM-D-Waves communication model

**DIMR controls lifecycle and timing through BMI. D-Flow FM and D-Waves exchange
fields through NetCDF com-files, not BMI field arrays.** D-Waves reads the
D-Flow FM grid from those files and owns the spatial remapping in both directions.

![D-Flow FM communication model](../include/dwaves_pathway_fm_bmi_detailed.svg)

[DOT source](../include/dwaves_pathway_fm_bmi_detailed.dot)

## Participants and channels

| Channel | Sender/writer | Receiver/reader | Contents |
|---|---|---|---|
| BMI calls | DIMR | D-Flow FM and D-Waves components | Initialize, update and finalize; D-Waves mode (`mode` variable) and communicator setup. |
| NetCDF com-files | D-Flow FM | D-Waves | D-Flow FM geometry and selected hydrodynamic fields. |
| Same com-files | D-Waves, with `WriteCOM` | D-Flow FM | Wave fields mapped onto the D-Flow FM grid; D-Flow FM reads through its external-forcing/EC path. |
| D-Waves/SWAN staged files | D-Waves / SWAN | SWAN / D-Waves | Solver inputs, nesting boundaries and results, including in library mode. |

D-Flow FM writes its dedicated NetCDF communication format directly, usually
`*_com.nc`; Delft3D4 instead uses NEFIS com-files.
MDW `[Output] COMFile` selects the D-Flow FM file route. Input selections
control which D-Flow FM fields D-Waves reads. Online D-Flow FM runs always
require `WriteCOM = true` (D-Waves aborts otherwise), so wave writeback is not
optional here.

## Startup and repeated exchange

1. In the normal D-Flow FM-master configuration, DIMR initializes D-Flow FM
   first. Its initial external output creates/writes the geometry and initial
   fields needed by D-Waves.
2. DIMR sets D-Waves mode 1 and initializes it: when D-Waves is a `<start>` inside a
   `<startGroup>` within a `<parallel>` block (the usual D-Flow FM coupling
   layout), DIMR writes 1 to D-Waves' BMI `mode` variable before `initialize`.
   A non-blank MDW `[General] FlowFile` forces mode 1 as well.
   D-Waves checks the communication files, reads both grids and prepares maps for
   each D-Flow FM-partition/SWAN-grid pair.
3. DIMR advances components according to its configured schedule. D-Flow FM
   uses internal numerical steps and writes com-file fields at its output
   times; D-Waves' scheduled `update(dt)` advances its calculation time.
4. Within that D-Waves update, D-Waves visits SWAN grids in configured order:
   read/remap D-Flow FM fields, run SWAN, read results and accumulate any
   D-Flow FM-grid output. Nested grids use parent-generated boundary output.
5. D-Waves writes the accumulated wave fields (`WriteCOM` is mandatory online)
   before returning from its update. D-Flow FM reads available wave fields
   through EC during its forcing updates. The schedule repeats until DIMR
   finalizes the components.

Coordinate the D-Waves schedule and D-Flow FM com-output times. The diagram
separates responsibilities; it does not impose a universal D-Flow FM/D-Waves
update order or imply simultaneous file access. DIMR does not transfer the
gridded fields. Online D-Waves uses one calculation time per update, but can
still run multiple SWAN grids/nests.

## Mapping and parallel execution

| Direction | Initialize once per grid pair | Apply at each calculation |
|---|---|---|
| D-Flow FM → SWAN | `make_grid_map` invokes ESMF weight generation and loads sparse weights. | `get_flow_fields` → `get_var_netcdf` → `grmap_esmf`. |
| SWAN → D-Flow FM | `make_grid_map` → `mkmap` builds separate reference/weight tables. | With `WriteCOM`: `map_swan_output` → `grmap` → `wave2com` → `put_wave_fields`. |

Both maps are constructed when D-Flow FM grids are initialized, even for
read-only input. Reverse **application/writeback**, not map construction,
requires `WriteCOM` (mandatory in online runs). Weights are reused throughout
the run; the reverse map is not the inverse of the ESMF map. The legacy
`wave2flow` conversion is not used.

D-Flow FM partitions, D-Waves MPI ranks and SWAN grids/nests are distinct.
D-Waves reads the D-Flow FM partition com-files and constructs maps per grid pair.
Within the D-Waves communicator, the master handles file I/O and remapping.
For `SwanMode=lib`, workers join each `swan_lib` solve and synchronize before
the master processes output files. The grid/nest loop is sequential, not
one independent SWAN nest per rank. DIMR's `<parallel>` scheduling block
should not be confused with either engine's spatial MPI decomposition.

Standalone reading of existing D-Flow FM com-files uses the same mapping
machinery without advancing D-Flow FM; see the
[standalone model](./D-Waves_pathway_standalone.md).
Mode-2 fluid mud remains a [Delft3D4-only extension](./D-Waves_pathway_delft3d4.md).

## Implementation links

| Responsibility | Entry points |
|---|---|
| DIMR lifecycle and schedule | [runParallelInit and scheduler](../../../dimr/packages/dimr_lib/src/dimr.cpp#L429), [D-Waves mode 1 in a startGroup](../../../dimr/packages/dimr_lib/src/dimr.cpp#L515), [D-Waves mode 0 in a first start block](../../../dimr/packages/dimr_lib/src/bmi.cpp#L176), [example schedule](../../../../../examples/dflowfm/08_dflowfm_sequential_dwaves/dimr_config.xml) |
| D-Waves BMI and grid setup | [wave_bmi](../../packages/wave/src/wave_bmi.f90#L63), [BMI mode variable](../../packages/wave/src/wave_bmi.f90#L308), [wave_init](../../packages/manager/src/wave_main.f90#L131), [grids_and_gridmaps](../../packages/kernel/src/grids_and_gridmaps.f90#L82) |
| D-Flow FM writes communication fields | [flowinit initial output](../../../dflowfm/packages/dflowfm_kernel/src/dflowfm_manager/unstruc_api.F90#L263), [flow_externaloutput](../../../dflowfm/packages/dflowfm_kernel/src/dflowfm_manager/flow_externaloutput.F90#L210), [wricom](../../../dflowfm/packages/dflowfm_kernel/src/dflowfm_io/wricom.f90#L44) |
| D-Flow FM reads wave fields | [set_wave_parameters](../../../dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/timespace/fm_external_forcings_update.f90#L565), [set_all_wave_parameters / ecGetValues](../../../dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/timespace/fm_external_forcings_update.f90#L717) |
| D-Waves remapping and writeback | [make_grid_map](../../packages/data/src/swan_flow_grid_maps.f90#L306), [get_flow_fields](../../packages/io/src/get_flow_fields.f90), [map_swan_output](../../packages/kernel/src/map_swan_output.f90), [swan_tot writeback](../../packages/manager/src/swan_tot.f90#L499) |
| D-Waves MPI coordination | [master/worker dispatch](../../packages/manager/src/wave_main.f90#L385), [run_swan](../../packages/kernel/src/run_swan.f90#L90), [worker barrier](../../packages/kernel/src/run_swan_slave.f90#L60) |

Configuration reference: [D-Waves User Manual](../include/D-Waves_User_Manual.pdf),
Sections 5.4.1.1–5.4.1.2 and 5.4.9 (GUI), Appendices A.1.1 (MDW keywords) and
A.1.3.4.2 (D-Flow FM example).
