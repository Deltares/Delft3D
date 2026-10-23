# D-Flow FM-D-Waves communication model

**DIMR controls lifecycle and timing through BMI. D-Flow FM and D-Waves exchange
fields through NetCDF com-files, not BMI field arrays.** 
D-Waves reads the
D-Flow FM grid from those files and owns the spatial remapping in both directions.

![D-Flow FM communication model](../include/dwaves_pathway_fm_bmi_detailed.svg)

[DOT source](../include/dwaves_pathway_fm_bmi_detailed.dot)

## Participants and channels

| Channel | Sender/writer | Receiver/reader | Contents |
|---|---|---|---|
| BMI calls | DIMR | D-Flow FM and D-Waves components as library | Initialize, update and finalize; D-Waves mode (`mode` variable) and communicator setup. |
| NetCDF com-files | D-Flow FM | D-Waves | D-Flow FM geometry and selected hydrodynamic fields. D-Waves reads them directly with the NetCDF library, not through the EC module. |
| (same) NetCDF com-files | D-Waves, with `WriteCOM` | D-Flow FM | Wave fields mapped onto the D-Flow FM grid; D-Flow FM reads them through its external-forcing/EC module path. |
| D-Waves/SWAN staged files | D-Waves / SWAN | SWAN / D-Waves | Solver inputs, nesting boundaries and results, including in library mode. |

D-Flow FM writes its dedicated NetCDF communication format directly, usually
`*_com.nc`.
MDW `[Output] COMFile` selects the D-Flow FM file route. Input selections
control which D-Flow FM fields D-Waves reads. Online D-Flow FM runs always
require `WriteCOM = true` (D-Waves aborts otherwise), so D-Waves writeback is not
optional here.

### Who reads the com-file, and who interpolates

The two sides read the same file with different mechanisms:

- **D-Flow FM reads the wave fields through the EC module.** The com-file is
  registered as D-Flow FM's wave input file
  ([wricom.f90](../../../dflowfm/packages/dflowfm_kernel/src/dflowfm_io/wricom.f90#L80)).
  Each wave quantity (`hrms`, `tp`, `dir`, ...) is registered with
  `ec_addtimespacerelation`
  ([flow_initwaveforcings_runtime.f90](../../../dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/flow_initwaveforcings_runtime.f90#L87))
  and read with `ecGetValues`
  ([fm_external_forcings_update.f90](../../../dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/timespace/fm_external_forcings_update.f90#L725)).
  For online runs EC treats the file as holding a single time field. EC only
  interpolates in time here; the fields are already on the D-Flow FM grid.
- **D-Waves reads the D-Flow FM fields directly with the NetCDF library**
  (`nf90_open`, `nf90_inq_varid`, ...), not through EC
  ([get_var_netcdf.f90](../../packages/io/src/get_var_netcdf.f90#L126)).

Spatial interpolation of field values in D-Waves is done by ESMF (D-Flow FM →
SWAN) or by D-Waves' own `mkmap`/`grmap` tables (SWAN → D-Flow FM); see
"Mapping and parallel execution" below. D-Waves does link the EC module, but
only uses it for one geometric step, not for interpolating values: when it
reads the D-Flow FM grid from the com-file
([read_netcdf_grd](../../packages/data/src/read_grids.f90#L575)) and the
non-spherical grid is not purely quadrilateral, it calls the EC routine
`tricall` to triangulate the cell centres
([read_grids.f90](../../packages/data/src/read_grids.f90#L1013)). The
resulting mesh is written to the temporary source-grid file
`TMP_ESMF_RegridWeightGen_flow_source_<n>.nc`
([read_grids.f90](../../packages/data/src/read_grids.f90#L1051)), which is the
input for the ESMF weight generation.

## Startup and repeated exchange

1. In the normal D-Flow FM-master configuration, DIMR initializes D-Flow FM
   first. D-FlowFM's initial external output creates/writes the geometry and initial
   fields needed by D-Waves.
2. DIMR sets D-Waves mode 1 and initializes it: when D-Waves is a `<start>` inside a
   `<startGroup>` within a `<parallel>` block (the usual D-Flow FM coupling
   layout), DIMR writes 1 to D-Waves' BMI `mode` variable before `initialize`.
   A non-blank MDW `[General] FlowFile` value, forces mode 1 as well.
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
   through the EC module during its forcing updates. The schedule repeats until DIMR
   finalizes the components.

The diagram at the top of this page
([dwaves_pathway_fm_bmi_detailed](../include/dwaves_pathway_fm_bmi_detailed.svg))
shows which component is responsible for what. It does not prescribe a fixed
update order between D-Flow FM and D-Waves, and it does not imply that both
access the com-file at the same time. The D-Waves update schedule and the
D-Flow FM com-file output times must be aligned in the configuration, so that
D-Waves finds the fields it needs. DIMR does not transfer the gridded fields;
they travel only through the com-files. Each online D-Waves update covers one
calculation time, but can still run multiple SWAN grids and nests.

## Mapping and parallel execution

| Direction | Initialize once per grid pair | Apply at each calculation |
|---|---|---|
| D-Flow FM → SWAN | `make_grid_map` invokes ESMF weight generation and loads sparse weights. | `get_flow_fields` → `get_var_netcdf` → `grmap_esmf`. |
| SWAN → D-Flow FM | `make_grid_map` → `mkmap` builds separate reference/weight tables. | With `WriteCOM`: `map_swan_output` → `grmap` → `wave2com` → `put_wave_fields`. |

Both maps are constructed when D-Flow FM grids are initialized, even for
read-only input. Reverse **application/writeback**, not map construction,
requires `WriteCOM` (mandatory in online runs). Weights are reused throughout
the run; the reverse map is not the inverse of the ESMF map. The
`wave2flow` conversion of vector fields to curvilinear grid directions is not
used here; it applies only to the Delft3D4 NEFIS com-file route (see the
[Delft3D4 pathway](./D-Waves_pathway_delft3d4.md)).

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
| D-Waves reads the D-Flow FM grid and fields | [read_netcdf_grd (grid, EC `tricall` triangulation)](../../packages/data/src/read_grids.f90#L575), [get_var_netcdf (direct NetCDF reads)](../../packages/io/src/get_var_netcdf.f90#L126) |
| D-Waves remapping and writeback | [make_grid_map](../../packages/data/src/swan_flow_grid_maps.f90#L306), [get_flow_fields](../../packages/io/src/get_flow_fields.f90), [map_swan_output](../../packages/kernel/src/map_swan_output.f90), [swan_tot writeback](../../packages/manager/src/swan_tot.f90#L499) |
| D-Waves MPI coordination | [master/worker dispatch](../../packages/manager/src/wave_main.f90#L385), [run_swan](../../packages/kernel/src/run_swan.f90#L90), [worker barrier](../../packages/kernel/src/run_swan_slave.f90#L60) |

Configuration reference: [D-Waves User Manual](../include/D-Waves_User_Manual.pdf),
Sections 5.4.1.1–5.4.1.2 and 5.4.9 (GUI), Appendices A.1.1 (MDW keywords) and
A.1.3.4.2 (D-Flow FM example).
