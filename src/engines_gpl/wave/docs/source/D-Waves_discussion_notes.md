# D-Waves discussion notes

Working record of the code-pathway discussions, for later refinement of
[D-Waves code pathways](./D-Waves_code_pathways.md). These are explanatory
notes, not a new configuration specification or a record of executed model
tests. Source references describe the current implementation and may move
as the code changes.

The preCICE proof-of-concept pathway is outside this review. These notes do
not change its documentation or the existing diagrams.

## Terminology: Delft3D4 FLOW versus D-Flow FM

To avoid ambiguity, these notes use **Delft3D4 FLOW** for the classic
structured-grid `d_hydro` engine and **D-Flow FM** for the flexible-mesh
engine. They are distinct engines and have different D-Waves coupling/data
routes:

| Engine | Current D-Waves route described here |
|---|---|
| Delft3D4 FLOW | Separate `d_hydro` and WAVE processes; DelftIO carries step commands/status; legacy `com-<runid>` files carry gridded fields. |
| D-Flow FM | FM NetCDF `*_com.nc` communication files; live component scheduling through DIMR/BMI. |

Where a section says **FLOW** generically, it means the hydrodynamic-model
side or its fields, not that Delft3D4 FLOW and D-Flow FM are interchangeable.
Any claim specific to one engine is labelled explicitly below.

## 1. Which modes correspond to Delft3D4 and FM?

Both normal online pathways use `flow_online = 1`; there is no separate
numeric FM mode. `stand_alone = 0` means no live FLOW synchronization.
`flow_mud_online = 2` adds fluid-mud operations to classic Delft3D4 coupling.

Mode and data route are separate selectors:

| Scenario | Mode | Data route and coordination |
|---|---|---|
| Independent or offline WAVE | 0 | No live FLOW handshake; optional saved FLOW data. |
| Delft3D4 online | 1 | Blank `swan_run%flowgridfile`: DelftIO command/status synchronization and legacy com-files for fields. |
| FM through DIMR/BMI | 1 | Nonblank `swan_run%flowgridfile`: FM NetCDF communication files; BMI updates drive WAVE stepping. |
| Delft3D4 water FLOW plus fluid mud | 2 | Classic pathway with additional mud initialization, synchronization and input fields. |

MDW `[Output] COMFile` sets the internal `swan_run%flowgridfile`; this is not
an MDW keyword called `FlowGridFile`. A nonblank path alone does not establish
online coupling, since the FM file route is also available in standalone mode.

References:
[mode constants](../../packages/data/src/wave_data.f90#L40),
[route initialization](../../packages/data/src/flow_data.f90#L79),
[stepping branches](../../packages/manager/src/wave_main.f90#L430),
[MDW output parsing](../../packages/data/src/swan_input.f90#L1516).

## 2. Is mode 2 a separate driver, and can it be used with FM?

Mode 2 is a conditional extension of the classic mode-1 pathway, not a
separate top-level driver or time loop. It shares the WAVE initialization,
stepping, SWAN orchestration and finalization routines.

The additional work occurs at several stages:

- Initialize a second, mud-model connection and collect `mudids`.
- Wait for commands from both water FLOW and the mud computation.
- Read mud density, viscosity, depth and level, and prepare SWAN mud input
  and physics settings.
- Report completion to both models.

The implementation requires one water domain and one mud domain and assumes
their grids are identical. This is not a general suspended-sediment switch.

Mode 2 is not an implemented FM/BMI mud-coupling option. Although parsing
nonblank `FlowFile` and `FlowMudFile` can assign mode 2, the FM NetCDF branch
skips the mud connection and `mudids` initialization needed by the mud reader.
Parser assignability does not establish a working FM mud pathway.

References:
[mode selection](../../packages/data/src/swan_input.f90#L915),
[mud initialization](../../packages/data/src/flow_data.f90#L139),
[additional command wait](../../packages/manager/src/wave_main.f90#L445),
[completion reporting](../../packages/manager/src/wave_main.f90#L478),
[mud fields](../../packages/io/src/get_flow_fields.f90#L317),
[SWAN mud input grid](../../packages/data/src/swan_input.f90#L3238),
[SWAN mud physics](../../packages/data/src/swan_input.f90#L3676).

### Interpreting the startup message

The parser prints `Delft3D-WAVE runs online with Delft3D-FLOW, including MUD`
when it has assigned `flow_mud_online`. Treat this as a mode-selection
message, not as confirmation that a particular FLOW engine has connected or
that FM/BMI mud coupling is implemented. The parser assigns mode 2 when
`[General] FlowFile` and `[General] FlowMudFile` are nonblank; that check
does not inspect the later FM NetCDF route.

The actual route is selected later by `flow_init`: a nonblank internal
`swan_run%flowgridfile` takes the FM NetCDF branch first. That branch checks
communication files but does not initialize the mud connection or `mudids`
required by mode-2 mud field reads. With a blank FM path, non-standalone mode
enters the classic Delft3D4/DelftIO branch, where the additional mud
handshake is implemented. The BMI wrapper also explicitly rejects the
mode-2 end-time and timestep queries as not implemented.

Thus the startup text is overly broad/ambiguous for distinguishing engines;
it should not be used as evidence that mode 2 works with FM. A more precise
message would name the requested mode and say that the actual data/coupling
route depends on the configured file path and caller.

References:
[mode assignment and startup message](../../packages/data/src/swan_input.f90#L915),
[FM branch precedes classic mode branch](../../packages/data/src/flow_data.f90#L68),
[mode-2 BMI time-query limitations](../../packages/wave/src/wave_bmi.f90#L213).

## 3. Why would standalone WAVE need FLOW data?

It does not inherently need it. Standalone describes execution, not the
source of forcing.

- **Independent wave calculation:** bathymetry, water level, currents, wind
  and boundary inputs are supplied through WAVE configuration/input files
  as needed; FLOW results are not required.
- **Offline calculation using FLOW results:** selected fields from a prior
  FLOW run supply hydrodynamic conditions without running FLOW alongside WAVE.

Water depth and currents affect wave propagation, refraction, breaking and
wave-current interaction. Saved FLOW results are one optional source of
those conditions.

In `swan_tot`, the call to `get_flow_fields` is gated by the derived internal
flag `swan_run%useflowdata`, rather than by standalone versus online mode.
That flag reflects configured FLOW-sourced inputs; it is not an MDW key.

Standalone scheduling also depends on the route. The legacy route lets
`swan_tot` process the MDW times; the FM file route selects times within the
supplied step interval, or computes once when no timepoints are specified.
Do not describe the executable's `wave_main_step(-1)` as universally running
all MDW times.

References:
[input preparation and optional FLOW reads](../../packages/manager/src/swan_tot.f90#L138),
[derived flag](../../packages/data/src/swan_input.f90#L2074),
[standalone scheduling](../../packages/manager/src/wave_main.f90#L485).
The [D-Waves User Manual](../include/D-Waves_User_Manual.pdf), Appendix A.1.2,
describes the legacy offline workflow.

## 4. Must the SWAN and FLOW grids be the same?

No. FLOW fields are first read on the FLOW grid, then mapped onto the SWAN
grid before the SWAN calculation:

```text
FLOW fields on FLOW grid -> spatial mapping -> input fields on SWAN grid
```

`get_flow_fields` uses `grmap` for the legacy route and `grmap_esmf` with
regridding weights for the FM NetCDF route. Using FLOW results does not make
the FLOW grid the SWAN computational grid.

The identical-grid assumption in mode 2 concerns the water-FLOW and
mud-model grids, not the SWAN/FLOW pair.

Reference:
[field reading and mapping](../../packages/io/src/get_flow_fields.f90#L102).

## 5. Does FM convert its output into a com-file for WAVE?

FM writes a dedicated NetCDF communication file directly from its model
state. This is not a conversion of ordinary FM map output into a legacy
Delft3D4 com-file.

```text
FM model state
  -> wricom -> unc_write_map_filepointer
  -> *_com.nc
  -> WAVE: get_flow_fields -> get_var_netcdf
  -> fields on FM mesh -> grmap_esmf -> fields on SWAN grid
```

| Engine | Communication format read by WAVE |
|---|---|
| Classic Delft3D4 | Legacy NEFIS com-files |
| D-Flow FM | NetCDF `*_com.nc` files |

FM's `wricom` creates or opens the communication file for online SWAN
coupling, writes FLOW information through `unc_write_map_filepointer`, and
closes the file to flush it. WAVE's dedicated NetCDF reader reads variables
such as `FlowElem_bl`, `s1`, `ucx` and `ucy`.

A suitably populated saved FM communication file can also be reused by
standalone WAVE, subject to its scheduling rules. An arbitrary FM map-output
file is not a substitute: WAVE expects the communication-file schema and
the fields required by the configured inputs.

BMI provides lifecycle and stepping here, not generic in-memory exchange
of hydrodynamic field arrays through WAVE's `get_var`.

References:
[FM writer](../../../dflowfm/packages/dflowfm_kernel/src/dflowfm_io/wricom.f90#L44),
[WAVE NetCDF reader](../../packages/io/src/get_var_netcdf.f90#L123),
[mapping](../../packages/io/src/get_flow_fields.f90#L102),
[WAVE BMI wrapper](../../packages/wave/src/wave_bmi.f90).

## 6. What are the meteo calls in wave_init for?

They prepare optional external, file-based forcing for SWAN, primarily
spatially/time-varying wind. The same infrastructure also handles configured
sea-ice fields. They are not FLOW-WAVE synchronization calls and do not
require a FLOW model.

### Initialization

For each SWAN grid with `n_extforces > 0`, `wave_init` performs:

| Call | Purpose |
|---|---|
| `initmeteo` | Initialize the external-forcing context for the SWAN grid. |
| `addmeteoitem` | Register/read configured forcing-file information. |
| `gridtometeo` | Supply SWAN coordinates and active-cell information for evaluating forcing on that grid. |
| `getmeteotypes` | Inspect input types; WAVE rejects `meteo_on_computational_grid`, identified by its error message as FLOW-grid input. |
| `getmeteoquantities` | When ice damping is enabled, check available ice quantities against the selected ice model. |

Initialization prepares the sources and target grid; it does not set one
fixed wind field for the whole simulation.

### During calculation and finalization

At applicable calculation times, `swan_tot` calls `meteoupdate` using the
WAVE reference date, timezone and current time. It then uses `getmeteoval`
to retrieve `windu` and `windv` when wind is not taken from FLOW, and ice
fields when applicable. Wind arrays are staged for SWAN in `WNDNOW`.
Finalization calls `deallocmeteo` for each SWAN grid.

```text
External wind files
  -> wave_init: register sources and SWAN target grid
  -> swan_tot: evaluate forcing at current time
  -> wind components on SWAN grid -> SWAN wind input
```

WAVE can therefore use FLOW water levels/currents while reading atmospheric
wind directly from external files, or use those files in a standalone run.
When both FLOW wind and external meteo wind are configured, the input parser
gives the external meteo wind precedence.

The atmospheric-pressure retrieval shown in `swan_tot` is commented out;
it should not be documented as an active input there.

References:
[meteo initialization](../../packages/manager/src/wave_main.f90#L255),
[runtime update and retrieval](../../packages/manager/src/swan_tot.f90#L173),
[SWAN wind-file staging](../../packages/manager/src/swan_tot.f90#L273),
[wind-source precedence](../../packages/data/src/swan_input.f90#L2051),
[meteo cleanup](../../packages/manager/src/wave_main.f90#L591).

## 7. How is flow_data used, and must standalone WAVE start FLOW?

`flow_data` is not a FLOW solver or an engine launcher. It maintains WAVE's
information about FLOW data sources and establishes the classic online
control connection when required.

Its module state includes `num_subdomains`, classic domain identifiers
`runids`, mud identifiers `mudids` and `num_muddomains`, and the setup flag
`flow_data_initialized`. The main setup routine is `flow_init`.

| Pathway | What `flow_init` does | Is a live FLOW computation required? |
|---|---|---|
| Independent standalone WAVE | Normally not called when neither FLOW input nor com-file output is requested. | No. |
| Standalone with saved Delft3D4 data | Selects legacy com-file runids and reads timing information from the first com-file; no handshake. | No; the required files must already exist. |
| Standalone with saved FM data | Checks the configured NetCDF communication file or its partition variants and records the domain count; no handshake. | No; the required files must already exist. |
| Delft3D4 online, mode 1 | Waits for DelftIO initialization, receives domain identifiers and timing, and acknowledges the connection. | Yes, started externally. |
| Delft3D4 water plus mud, mode 2 | Adds a mud connection and domain-count checks to the classic water connection. | Yes, both water and mud computations participate. |
| FM through DIMR/BMI | Checks NetCDF communication files rather than performing the classic DelftIO handshake. | Yes for live coupling; DIMR manages the components. |

FLOW input and FLOW-grid output are independent selections. `WriteCOM`
can require communication-file/grid initialization even when hydrodynamic
input from FLOW is disabled.

### Setup versus field acquisition

`flow_init` identifies data sources or establishes the classic connection.
It does not itself read and map all hydrodynamic fields:

```text
wave_init
  -> flow_init: identify files/domains or establish classic connection
  -> grids_and_gridmaps: read grids and prepare spatial mappings

swan_tot, during calculation
  -> get_flow_fields: read selected fields and map them onto SWAN grids
```

For independent standalone execution, `grids_and_gridmaps` sets
`n_flow_grids = 0` when neither FLOW input nor com-file output is requested.
The SWAN grids are still initialized.

### Process ownership

Standalone WAVE does not need D-Flow FM or Delft3D4 FLOW running alongside
it, and it does not start either engine. Offline use requires suitable saved
results, not a live producer.

The WAVE executable reads its arguments and calls `wave_main_init`,
`wave_main_step` and `wave_main_finish`. For classic online coupling, the
coupled-run script separately starts `d_hydro` and `wave ... 1`. In the
DIMR/BMI pathway, DIMR manages component initialization and stepping; WAVE
does not launch an FM executable.

Launching the WAVE executable is not sufficient to guarantee standalone
behavior. It defaults to mode 0, but a nonblank MDW `[General] FlowFile`
changes the mode during parsing. With the classic route this can make WAVE
wait for FLOW, rather than merely read saved results.

References:
[flow_data state and initialization](../../packages/data/src/flow_data.f90#L37),
[wave_init call conditions](../../packages/manager/src/wave_main.f90#L194),
[grid/domain setup](../../packages/kernel/src/grids_and_gridmaps.f90#L82),
[WAVE executable](../../packages/wave/src/wave_exe.f90#L82),
[classic coupled launcher](../../../flow2d3d/scripts/run_dflow2d3d_dwaves.sh#L131),
[MDW mode override](../../packages/data/src/swan_input.f90#L915).

## 8. What does `swan_run%sferic = swan_grids(1)%sferic` do?

Each SWAN grid has a `sferic` flag read from its grid definition. For
formatted grid files, `read_grd` detects the keyword `Spherical` in the
header, or reads `Coordinate System`; when the latter is present, it
overrides the header keyword.

After grids are initialized, `wave_main_init` copies the flag from the first
SWAN grid into the run-level `swan_run%sferic`. SWAN input generation uses
the corresponding `sferic` setting to emit `COORD SPHE` in the SWAN input
when it is true. This tells SWAN to interpret coordinates as spherical /
geographical coordinates (for example longitude and latitude), rather than
as Cartesian plane coordinates.

This is a coordinate-system choice, not a statement that the grid is
curvilinear, that it is an FM grid, or that FLOW and SWAN grids must match.
It is also a run-level setting taken from the first SWAN grid; if nested
SWAN grids could have differing coordinate systems, that assumption warrants
checking rather than assuming the flag is independently honored per grid.

References:
[grid coordinate-system detection](../../packages/data/src/read_grids.f90#L399),
[assignment from the first SWAN grid](../../packages/manager/src/wave_main.f90#L253),
[SWAN `COORD SPHE` input generation](../../packages/data/src/swan_input.f90#L3071).

## 9. What does the number of SWAN grids mean?

In this code, `n_swan_grids` is the number of configured SWAN computational
domains, generally the number of `[Domain]` entries parsed from the MDW file
(`swan_run%nnest`). These are SWAN grids used for nesting, not a count of
MPI ranks or FLOW domains/partitions.

`grids_and_gridmaps` reads the grid names from the `swangrids` file when
present; otherwise it uses each MDW domain's `curlif` grid filename. It
initializes one SWAN grid per configured domain and separately initializes
FLOW grids. During a calculation, `swan_tot` loops over each SWAN grid and
prepares its inputs and SWAN run.

Parallelism is a separate concern. WAVE initializes its MPI state and the
manager distinguishes master and non-master ranks; `n_swan_grids` itself
does not specify rank count or rank-to-grid assignment. Likewise, `n_flow_grids`
is determined separately from FLOW domains / NetCDF partitions when FLOW
data or output is used.

Useful distinction:

| Quantity | Represents |
|---|---|
| `n_swan_grids` / `swan_run%nnest` | Configured SWAN computational domains/grids, including nested grids. |
| `n_flow_grids` | FLOW input/output grids or domains used for mapping. |
| MPI rank count (`numranks`) | Parallel processes; independent of the configured SWAN-grid count. |

References:
[MDW domain count](../../packages/data/src/swan_input.f90#L1683),
[SWAN and FLOW grid initialization](../../packages/kernel/src/grids_and_gridmaps.f90#L82),
[per-SWAN-grid calculation loop](../../packages/manager/src/swan_tot.f90#L131),
[MPI/master initialization](../../packages/manager/src/wave_main.f90#L124),
[master-only initialization](../../packages/manager/src/wave_main.f90#L188).

## 10. What does the number of FLOW grids mean?

`n_flow_grids` counts the separate FLOW spatial domains/partitions that WAVE
loads as grids for input mapping and/or WAVE output on the FLOW grid. It is
not a count of SWAN grids, MPI ranks, or the number of variables.

The count comes from `num_subdomains`, but the source of that count depends
on the route:

- For classic Delft3D4 offline input, it is the number of runids in
  `[General] ComFile` (or one default runid).
- For classic online coupling, FLOW supplies the domain names during the
  DelftIO initialization handshake.
- For FM NetCDF files, it is one for a single communication file, or the
  number of sequential partition communication files found for a partitioned
  output.

For standalone mode, `n_flow_grids` is zero unless FLOW data is selected
(`useflowdata`) or WAVE output on the FLOW grid is requested (`swwav` /
`WriteCOM`). In coupled mode, the initialized FLOW domains are set up as
grids. Thus output-only use can require FLOW-grid setup even without reading
hydrodynamic forcing.

`grids_and_gridmaps` creates a mapping for each SWAN-grid/FLOW-grid pair.
At calculation time, WAVE reads each selected FLOW grid's fields and maps
them onto the applicable SWAN grid. This lets multiple FLOW domains or FM
partitions cover the WAVE area; the count describes the input/output grid
collection, not a requirement that the FLOW and SWAN grids have the same
shape or resolution.

```text
SWAN grid 1  <->  FLOW grid/partition 1
            <->  FLOW grid/partition 2
SWAN grid 2  <->  FLOW grid/partition 1
            <->  FLOW grid/partition 2
```

References:
[count choice by run mode](../../packages/kernel/src/grids_and_gridmaps.f90#L82),
[domain/partition discovery](../../packages/data/src/flow_data.f90#L60),
[pairwise grid-map creation](../../packages/kernel/src/grids_and_gridmaps.f90#L165),
[FLOW-field mapping](../../packages/io/src/get_flow_fields.f90#L102).

## 11. Do FLOW and SWAN domains need matching x-y extents?

They do not need identical extents, dimensions, or resolution. For FLOW
forcing to affect a SWAN grid, however, their physical x-y locations must
overlap where the SWAN calculation needs those FLOW fields. If one SWAN grid
extends beyond an individual FLOW domain, another FLOW partition/domain may
cover that area. If selected FLOW data is expected throughout a SWAN area,
the set of FLOW domains should collectively provide valid coverage there.

The code does not pair domains by matching names or grid numbers. During
initialization, `grids_and_gridmaps` loops over every SWAN grid and every
FLOW grid and constructs a pairwise map. Each pair's mapping is calculated
from the grids' coordinates and geometry:

```text
for each SWAN grid:
    for each FLOW grid:
        make_grid_map(FLOW source, SWAN receiver)
```

For classic com-file grids, `make_grid_map` builds interpolation references
and weights with `mkmap`. For FM NetCDF grids, it generates/loads ESMF
regridding weights for that FLOW-grid/SWAN-grid pair. The mapping operation
updates the part of the SWAN grid covered by the current FLOW domain; the
FM route warns when a pair has no overlap. Partition/domain coverage is
tracked so a SWAN point can be associated with the FLOW source that covers
it.

The coordinate systems must of course describe compatible physical
locations (for example, both geographical coordinates or correctly
transformed Cartesian coordinates). "Need not match" means the grids need
not have the same boundary, cell count or spacing; it does not mean unrelated
coordinate systems or non-overlapping locations can be mapped meaningfully.

References:
[pairwise FLOW-to-SWAN and SWAN-to-FLOW map setup](../../packages/kernel/src/grids_and_gridmaps.f90#L165),
[classic `mkmap` interpolation setup](../../packages/data/src/swan_flow_grid_maps.f90#L520),
[FM regridding weights and no-overlap handling](../../packages/data/src/swan_flow_grid_maps.f90#L361),
[FLOW-to-SWAN field mapping](../../packages/io/src/get_flow_fields.f90#L102).

## 12. What do the `r_tmp_filename` / `p_tmp_filename` loops do?

These loops do not compare coordinates, search for overlapping grid cells,
or generate interpolation weights. They copy temporary grid-file names
between the per-pair `flow2swan_maps(i_swan, i_flow)` objects so each
individual grid can reuse one serialized NetCDF representation across its
mapping pairs:

- `r_tmp_filename` is the temporary file for the **receiver** grid, the SWAN
  grid. For each SWAN grid `i`, the first loop copies the receiver filename
  from pair `(i,1)` to the other FLOW-pair entries `(i,j)`.
- `p_tmp_filename` is the temporary file for the **provider** grid, the FLOW
  grid. For each FLOW grid `j`, the second loop copies the provider filename
  from pair `(1,j)` to the other SWAN-pair entries `(i,j)`.

The files are initially assigned while the grids are read: SWAN-grid
serialization sets the receiver filename, and FM NetCDF FLOW-grid reading
sets the provider filename. Reusing those names avoids requiring a separate
temporary serialized copy of the same grid for every pair.

The actual spatial mapping setup happens afterward in the nested pair loop,
which calls `make_grid_map` once for every FLOW/SWAN pair. For the FLOW-to-
SWAN direction, `make_grid_map(j, i, f, s, f2s, netcdf_files)` identifies
FLOW grid `j` as provider and SWAN grid `i` as receiver. In the FM NetCDF
route, that call invokes `ESMF_RegridWeightGen` using the provider and
receiver temporary files and reads the resulting weights. In the classic
route, the internal `mkmap` path constructs interpolation references and
weights instead. Therefore the filename-copy loops prepare reusable inputs;
they neither decide overlap nor perform the remapping.

References:
[grid-file creation and filename propagation](../../packages/kernel/src/grids_and_gridmaps.f90#L122),
[temporary filename meanings](../../packages/data/src/swan_flow_grid_maps.f90#L58),
[pairwise `make_grid_map` calls](../../packages/kernel/src/grids_and_gridmaps.f90#L165),
[FM ESMF weight generation](../../packages/data/src/swan_flow_grid_maps.f90#L361),
[classic `mkmap` setup](../../packages/data/src/swan_flow_grid_maps.f90#L520).

## 13. Why does the standalone executable pass `stepsize = -1`?

The command-line executable sets `stepsize = -1.0_hp` immediately before
calling `wave_main_step(stepsize)`. This is a hard-coded argument from the
standalone executable, not a time interval read from the MDW file.

Its meaning depends on the selected standalone route:

- **Blank `swan_run%flowgridfile` (legacy standalone route):** the standalone
  branch calls `swan_tot(..., selectedtime=0)` directly and does not use
  `stepsize`. `swan_tot` processes the configured MDW time list itself.
  In this route, `-1` is effectively unused by the manager.
- **Nonblank `swan_run%flowgridfile` (FM NetCDF route):** the standalone
  branch does use `stepsize`. If the MDW has no explicit times, it uses a
  one-computation branch; otherwise it forms an end time from current time
  plus `stepsize` and selects any configured times in that interval.

Therefore `-1` does **not** mean “run the whole simulation” in a universal
sense. In particular, with a nonblank FM communication-file path it is not
a positive duration covering the MDW schedule. The executable's hard-coded
value and this route-dependent behavior should be kept distinct; callers
using the BMI interface provide their own `update(dt)` interval.

References:
[CLI step assignment and call](../../packages/wave/src/wave_exe.f90#L104),
[standalone route branches and interval logic](../../packages/manager/src/wave_main.f90#L485),
[legacy SWAN time loop](../../packages/manager/src/swan_tot.f90#L106),
[BMI-provided step](../../packages/wave/src/wave_bmi.f90#L146).

## 14. Is `swan_tot` SWAN itself, and who runs the time steps?

`swan_tot` is a Fortran routine inside the WAVE manager, not a standalone
SWAN executable. It prepares each SWAN grid's inputs, writes SWAN input,
invokes `run_swan`, reads SWAN output, and handles WAVE-side mapping and
writeback.

`run_swan` chooses how the SWAN solver is executed according to MDW
`[General] SwanMode`:

- `exe` is the default; WAVE launches the separate `swan.sh` / `swan.bat`
  program for each SWAN calculation.
- `lib` calls `swan_lib` from within the WAVE process. The parser only allows
  this mode when WAVE is run using MPI.

Thus, in standalone operation, WAVE calls `swan_tot`; whether SWAN itself is
an external process or an in-process library depends on `SwanMode`. `swan_tot`
orchestration remains inside WAVE in either case.

### Who controls calculation times?

For standalone with blank `swan_run%flowgridfile`, `wave_master_step` calls
`swan_tot(..., selectedtime=0)`. `swan_tot` iterates over the MDW `timwav`
schedule, sets the WAVE time for each entry and performs the corresponding
SWAN calculations. In this route the manager processes the MDW schedule; it
is not SWAN independently deciding the complete WAVE time sequence.

With nonblank `swan_run%flowgridfile` (selected by `[Output] COMFile`), the
standalone branch in `wave_master_step` uses its `stepsize` to choose the
time interval. MDW `timwav` entries are candidate calculation times within
that interval; they do not by themselves override the interval. With no
explicit times, the code has a separate single-computation branch.

Do not confuse this with `[General] FlowFile`: that setting changes the
WAVE coupling mode during MDW parsing and is not the FM communication-file
path. A classic online run waits for FLOW commands; it is not standalone
scheduling merely because WAVE was launched directly.

References:
[standalone scheduling](../../packages/manager/src/wave_main.f90#L485),
[MDW schedule loop and call to `run_swan`](../../packages/manager/src/swan_tot.f90#L106),
[SWAN execution-mode selection](../../packages/data/src/swan_input.f90#L827),
[`run_swan` library/executable branches](../../packages/kernel/src/run_swan.f90#L65),
[MDW FlowFile mode selection](../../packages/data/src/swan_input.f90#L915).

## 15. Is WAVE-SWAN file communication only used in standalone mode?

No. The SWAN input/output file interface is internal to the WAVE-SWAN
integration and is used whenever WAVE runs SWAN, whether WAVE is standalone
or coupled. `swan_tot` prepares the grid's inputs, writes the SWAN `INPUT`
file, calls `run_swan`, then reads SWAN's output. The `INPUT` file is opened
as a formatted text file by `write_swan_inp`.

This describes the SWAN control/data staging interface; it does not mean
that every file involved is ASCII, nor that these files carry the outer
FLOW-WAVE synchronization protocol. WAVE's coupling with classic FLOW uses
DelftIO commands/status plus legacy com-files for fields; FM uses its NetCDF
communication file, with BMI/DIMR lifecycle and stepping.

### SWAN library execution with MPI

The file interface is also used when SWAN is run as a library. `[General]
SwanMode=lib` selects `swan_lib` rather than launching the SWAN executable;
the MDW parser restricts this mode to MPI runs. WAVE's master rank performs
the WAVE-side input preparation and writes the input files. In
`run_swan`, it broadcasts `SWAN_GO`; each MPI rank enters `swan_lib` on the
engine communicator, then ranks synchronize at a barrier. The barrier
ensures SWAN has finished accessing shared files before WAVE's master reads
or deletes its outputs. The WAVE slave routine waits for the broadcast,
calls `swan_lib` when instructed, and also joins the barrier.

This MPI behavior is not specific to standalone mode. It applies whenever
the library mode is selected and WAVE schedules a SWAN calculation; the
outer standalone/coupled scheduling determines when that calculation begins.

### Coupled-run sequence

At each classic online coupling point, WAVE receives FLOW's command and
time, obtains the configured FLOW fields, stages SWAN inputs, invokes SWAN,
reads the result, writes configured WAVE output to the FLOW com-file, and
reports completion. In FM/BMI coupling, DIMR calls WAVE's BMI `update(dt)`;
WAVE advances its time by `dt`, uses the configured FM communication-file
route for fields, runs SWAN through the same internal interface, and returns
from the update for DIMR orchestration. Thus the prescribed exchange time
controls the outer model coupling, while the WAVE-SWAN file staging is an
implementation detail inside each WAVE calculation.

```text
Classic FLOW -- command/time --> WAVE manager
                                  |
                                  +-- prepare files --> SWAN (exe or MPI library)
                                  +-- read results <-- SWAN
                                  +-- write fields/status --> FLOW

DIMR -- BMI update(dt) --> WAVE manager
                            |
                            +-- prepare files --> SWAN (exe or MPI library)
                            +-- read results <-- SWAN
                            +-- return from update --> DIMR
```

References:
[formatted SWAN INPUT creation](../../packages/data/src/swan_input.f90#L2981),
[WAVE writes inputs and calls SWAN](../../packages/manager/src/swan_tot.f90#L344),
[SWAN library and executable branches](../../packages/kernel/src/run_swan.f90#L65),
[library mode MPI validation](../../packages/data/src/swan_input.f90#L827),
[slave-rank library call and barrier](../../packages/kernel/src/run_swan_slave.f90#L52),
[classic coupling step loop](../../packages/manager/src/wave_main.f90#L405),
[BMI `update(dt)`](../../packages/wave/src/wave_bmi.f90#L130).

## 16. Can standalone WAVE write WAVE results onto FLOW grids?

Yes. Standalone WAVE can write selected WAVE results to FLOW communication
files when MDW `[Output] WriteCOM` is enabled (`swan_run%swwav`). This does
not start a FLOW executable and does not create a FLOW simulation. It writes
the configured WAVE-to-FLOW fields into the identified communication file(s)
so they can be used by a later FLOW run or other workflow.

For this output-only case, WAVE initializes the FLOW grids and mapping even
when `useflowdata` is false. For every SWAN-grid/FLOW-grid pair,
`map_swan_output` maps WAVE results from the SWAN grid onto that FLOW grid.
After the SWAN-grid results have been combined, WAVE converts the parameters
and `put_wave_fields` writes them to the communication file(s).

The write path differs by file format:

- **Classic Delft3D4:** the legacy com-file route uses `wave2flow` for vector
  conversion, then `wave2com` and `put_wave_fields` to write the WAVE fields.
- **FM:** with `[Output] COMFile` selecting the NetCDF route, the classic
  `wave2flow` conversion is skipped; WAVE uses `wave2com` and
  `put_wave_fields` for the NetCDF communication file.

The relevant target grid and communication file(s) must be configured. In
the standalone initialization path, `flow_init` checks/counts the legacy
runid file(s) or FM NetCDF communication file(s) when either FLOW input or
`WriteCOM` is requested. This is communication-file writeback, not ordinary
WAVE map output and not a complete set of FLOW results.

```text
SWAN result on SWAN grid
  -> map_swan_output
  -> WAVE fields on FLOW grid
  -> (legacy only: wave2flow)
  -> wave2com -> put_wave_fields
  -> existing/configured FLOW communication file(s)
```

References:
[standalone flow initialization for input or output](../../packages/manager/src/wave_main.f90#L216),
[FLOW-grid maps initialized when `swwav` is enabled](../../packages/kernel/src/grids_and_gridmaps.f90#L82),
[SWAN-to-FLOW result mapping](../../packages/manager/src/swan_tot.f90#L412),
[legacy conversion and communication-file writeback](../../packages/manager/src/swan_tot.f90#L499),
[WriteCOM parsing](../../packages/data/src/swan_input.f90#L1516),
[communication-file checks](../../packages/data/src/flow_data.f90#L60).

## 17. Why is `nttide` forced to one for non-standalone FM?

After parsing the MDW file, `wave_main_init` executes:

```fortran
if (wavedata%mode /= stand_alone .and. swan_run%flowgridfile /= ' ') then
   swan_run%nttide = 1
endif
```

This condition identifies the non-standalone FM NetCDF route: the run is
online/coupled and has a nonblank `[Output] COMFile` path. In that pathway,
the external caller controls when WAVE advances. For DIMR/BMI, each
`update(dt)` calls `wave_main_step(dt)`; for each online step,
`wave_master_step` advances WAVE time by that interval and calls
`swan_tot(..., selectedtime=0)`.

Setting `nttide` to one prevents the MDW's standalone list of calculation
times from causing multiple SWAN calculations inside a single externally
scheduled FM update. In other words, the coupled caller's timestep is the
outer schedule, and WAVE performs one SWAN calculation for each call/update.
This is distinct from standalone FM-file use: when `mode` is
`stand_alone`, this condition is false, so the standalone scheduling logic
can use the MDW times (within its `stepsize` interval) or its no-times
single-computation branch.

It does not mean there is only one SWAN grid: `nttide` is the count of
calculation times, while `n_swan_grids` is the number of configured SWAN
domains/grids.

References:
[post-MDW `nttide` adjustment](../../packages/manager/src/wave_main.f90#L180),
[BMI update forwarding `dt`](../../packages/wave/src/wave_bmi.f90#L130),
[FM coupled time advance and one `swan_tot` call per step](../../packages/manager/src/wave_main.f90#L432),
[standalone FM scheduling](../../packages/manager/src/wave_main.f90#L497).

## 18. Classic Delft3D4 FLOW-WAVE timing, data exchange and remapping

### Coupling cadence and time advancement

In the classic online route, FLOW owns the simulation time loop. In
`tricom_step`, FLOW sets `itwav = itcomc` on each FLOW step and triggers a
WAVE calculation when `nst == itwav` with online coupling enabled
(`waveol == 2`). Thus WAVE is called at the configured FLOW communication /
wave-coupling times, not necessarily at every internal FLOW time step.

At a coupling time, FLOW updates the relevant communication-file data
(including the online bed-level write), then the master domain calls
`flow_to_wave_command(flow_wave_comm_perform_step, ..., nst)`. The
communication layer sends the command and FLOW timestep index; WAVE receives
them through `wave_from_flow_command` and `wave_master_step` sets WAVE's
time using `settimtscale`. WAVE runs its calculation, then sends a completion
status using `wave_to_flow_status`. The FLOW command call waits for this
status before returning, so the exchange is a synchronized handshake.

### Which channel carries what?

Classic coupling uses both DelftIO and the legacy FLOW communication
com-file, for different purposes:

| Channel | Carries |
|---|---|
| DelftIO `FLOW2WAVE_DATA` / `WAVE2FLOW_DATA` | Step/finalize command, FLOW step index, and WAVE success/error status. |
| Legacy com-file(s), typically `com-<runid>` | Spatial model fields: configured FLOW inputs for WAVE, and configured WAVE results for FLOW. |

The DelftIO streams do not carry the gridded hydrodynamic and wave fields.
WAVE's field routines read/write the com-file(s); the file/grid maps are
prepared during WAVE initialization.

### FLOW to WAVE

At the coupling time, WAVE's `swan_tot` calls `get_flow_fields` for each
configured FLOW domain and SWAN grid when `useflowdata` is enabled. On the
classic blank-`flowgridfile` route, the routine reads the requested fields
from the legacy com-file using helpers such as `get_dep`, `get_lev`,
`get_cur` and `get_wind`. It then maps the fields from the FLOW grid onto
the SWAN grid with the precomputed FLOW-to-SWAN map and `grmap`. For
currents, `flow2wav` first converts the FLOW velocity representation to
Cartesian values at cell centres; this is a vector-coordinate conversion,
not the spatial grid remapping.

Only configured FLOW-derived inputs are fetched. Other WAVE inputs can come
from WAVE files or external meteo forcing.

### WAVE to FLOW

If `[Output] WriteCOM` is enabled, after SWAN has run WAVE calls
`map_swan_output` for each SWAN-grid/FLOW-grid pair. That routine spatially
interpolates selected SWAN results onto the FLOW grid with `grmap` and the
precomputed SWAN-to-FLOW map. After combining output across SWAN grids,
WAVE performs the communication-format conversions (`wave2flow` for
classic vector directions and `wave2com` for com-file parameters) and writes
the fields to the com-file(s) through `put_wave_fields`.

FLOW then discovers the new wave-time record (`rdtimw`) and reads /
interpolates it into FLOW arrays using `setwav`. In the normal online
sequence, `tricom_step` sets `itrw = nst + 1` after issuing the WAVE command;
the wave result is then read at the following FLOW step when the com-file
record is available.

### Where the spatial maps are made

During initialization, `grids_and_gridmaps` loops over every FLOW/SWAN pair
and calls `make_grid_map` in both directions:

```text
make_grid_map(FLOW, SWAN, flow2swan_map, ...)
make_grid_map(SWAN, FLOW, swan2flow_map, ...)
```

For the classic route, `make_grid_map` constructs interpolation reference
indices and weights internally using `mkmap`. At runtime, `grmap` applies
those precomputed references and weights; it does not search for matches
again on every coupling step. The reverse SWAN-to-FLOW mapping is also built
internally. This differs from the FM NetCDF route, which uses external ESMF
weight generation for FLOW-to-SWAN mapping.

```text
FLOW time loop reaches coupling point
  -> FLOW updates legacy com-file(s)
  -> DelftIO command/time -> WAVE
  -> get_* reads configured FLOW fields from com-file
  -> grmap: FLOW grid -> SWAN grid
  -> SWAN run
  -> map_swan_output + grmap: SWAN grid -> FLOW grid
  -> wave2flow / wave2com -> put_wave_fields writes com-file(s)
  -> WAVE status -> FLOW
  -> FLOW setwav reads/interpolates wave record at next read point
```

Key references:
[FLOW step/coupling trigger and `itwav = itcomc`](../../../flow2d3d/packages/flow2d3d_manager/src/tricom_step.F90#L660),
[FLOW command and wave-record read logic](../../../flow2d3d/packages/flow2d3d_manager/src/tricom_step.F90#L734),
[DelftIO command/time and status channels](../../../../utils_lgpl/delftio/packages/delftio/src/delftio_sync/sync_flowwave.f90#L330),
[WAVE command receive, time update and status](../../packages/manager/src/wave_main.f90#L430),
[classic FLOW input reads and FLOW-to-SWAN `grmap`](../../packages/io/src/get_flow_fields.f90#L102),
[pairwise maps created in both directions](../../packages/kernel/src/grids_and_gridmaps.f90#L165),
[classic `mkmap` map setup](../../packages/data/src/swan_flow_grid_maps.f90#L520),
[SWAN-to-FLOW mapping](../../packages/kernel/src/map_swan_output.f90#L50),
[com-file writeback](../../packages/manager/src/swan_tot.f90#L499),
[FLOW reads wave fields with `setwav`](../../../flow2d3d/packages/flow2d3d_manager/src/tricom_step.F90#L826).

## 19. How is Delft3D4 FLOW's `tricom_init` invoked?

WAVE does not call `tricom_init`. It is part of the **Delft3D4 FLOW**
(`d_hydro`) engine's own initialization path; it is not a D-Flow FM routine:

```text
d_hydro / FLOW component startup
  -> trisim(...)
  -> trisim_init(...)
  -> tricom_init(gdp)
```

The `trisim` wrapper calls `trisim_init`; after Delft3D4 FLOW's preprocessing
and initial setup, `trisim_init` calls `tricom_init`. This happens because
the Delft3D4 FLOW engine is being initialized, not because WAVE invoked a
FLOW routine.

The Delft3D4 FLOW `.mdf` configuration controls whether Delft3D4 FLOW enables
its WAVE functionality and online coupling. Its `dimpro` reads the `WaveOL` setting:
`false`/default gives `waveol = 0` (offline), `true` gives `waveol = 2`
(online), and the value `mimic` gives `waveol = 1`. It also determines
whether `wave` is enabled from the relevant program-option setting. During
`tricom_init`, if Delft3D4 FLOW has wave enabled and `waveol == 2`, it
initializes the DelftIO connection through `flow_to_wave_init`.

That Delft3D4 FLOW-side setting does **not** launch the WAVE executable. For
classic online coupling, both engines must be started/configured: `d_hydro`
initializes its endpoint from its MDF configuration, and WAVE must also be
started with the matching online mode/configuration. The coupled-run script
demonstrates this by starting `d_hydro` and `wave <mdw-file> 1` as separate
processes. They rendezvous through DelftIO; Delft3D4 FLOW then initiates each
calculation command from `tricom_step`.

References:
[FLOW `trisim` wrapper](../../../flow2d3d/packages/flow2d3d_manager/src/trisim.F90#L127),
[`trisim_init` call to `tricom_init`](../../../flow2d3d/packages/flow2d3d_manager/src/trisim_mod.F90#L295),
[MDF parsing of `WaveOL`](../../../flow2d3d/packages/flow2d3d_io/src/input/dimpro.f90#L215),
[DelftIO initialization from FLOW](../../../flow2d3d/packages/flow2d3d_manager/src/tricom_init.F90#L1478),
[FLOW-issued step command](../../../flow2d3d/packages/flow2d3d_manager/src/tricom_step.F90#L734),
[launcher starts both executables](../../../flow2d3d/scripts/run_dflow2d3d_dwaves.sh#L131).

## 20. When does D-Waves read the D-Flow FM grid in DIMR/BMI coupling?

The FM grid is not a pre-run input that the user must create manually.
DIMR initializes the designated master component first; in the usual FM–
WAVE setup, this is D-Flow FM. Its BMI `initialize` path runs `flowinit`,
which performs model initialization and then writes initial external output
through `flow_externaloutput(time1)`. When online SWAN coupling is enabled,
that output path calls `wricom`, creating/writing the initial FM NetCDF
communication file, including the grid geometry and initial fields.

After the master FM component initializes, DIMR initializes the WAVE
component. WAVE reads the FM grid **during its own initialization**, after
DIMR calls WAVE BMI `initialize`; it does not ask FM for the grid through a
BMI grid-variable request. WAVE's `initialize` enters `wave_main_init`,
which calls `wave_init`. WAVE reads the MDW configuration first, then
initializes its FM-grid metadata and maps before the first coupled
`update(dt)`.

For the FM route, the MDW `[Output] COMFile` path is stored as
`swan_run%flowgridfile`. When FLOW-sourced fields (`useflowdata`) or
WAVE-to-FLOW output (`swwav` / `WriteCOM`) are requested, `wave_init` calls
`flow_init`. Its FM branch checks the configured NetCDF communication file
and counts file partitions; it does not perform the classic DelftIO
handshake.

Then `wave_init` calls `grids_and_gridmaps`. That routine recognizes the
nonblank FM communication-file path, reads the SWAN grids, and reads each
FM communication-file grid as an `NC` grid through `Alloc_and_get_grid`.
It constructs the FLOW-to-SWAN and SWAN-to-FLOW mappings before the first
WAVE update. For FM input, the FLOW-to-SWAN mapping setup also prepares the
ESMF regridding files/weights.

```text
DIMR -> D-Flow FM BMI initialize
  -> flowinit -> flow_modelinit
  -> initial flow_externaloutput(time1) -> wricom -> FM *_com.nc exists
DIMR -> WAVE BMI initialize
  -> wave_main_init -> wave_init
       -> read MDW / identify [Output] COMFile
       -> flow_init: check FM communication file(s)
       -> grids_and_gridmaps: read FM and SWAN grids; prepare maps/weights
DIMR -> BMI updates according to the configured component schedule
```

### Runtime timing and com-file updates

The D-Flow FM and WAVE data path remains file-backed, while DIMR controls
when the components are advanced. In a DIMR `<parallel>` block, the
designated master (normally D-Flow FM) determines the overall time advance.
The WAVE component's `<startGroup><time>...</time>` schedule in
`dimr_config.xml` determines its scheduled execution times. DIMR calls
WAVE's BMI `update(dt)` when WAVE is due; the supplied `dt` is the elapsed
interval since that WAVE component's previous update. WAVE's BMI wrapper
passes that interval to `wave_main_step`, which advances WAVE time and runs
the SWAN calculation.

D-Flow FM updates the NetCDF com-file from its external-output path. The
initial record is written during FM initialization; subsequent records are
written at FM's configured com-file output instants while FM is advanced.
The DIMR schedule for WAVE should therefore be coordinated with the FM
com-file write schedule so the needed FM fields are available for each WAVE
update. DIMR's XML schedule controls component timing; it does not itself
transfer the gridded FM/WAVE fields. Those are read/written through the
NetCDF com-file.

An example DIMR configuration uses FM as the `<start>` master and schedules
WAVE with `<time>0.0 3.6e3 9.99e4</time>`; the listed values are WAVE's
component execution times, not a replacement for FM's internal numerical
timestep. The concrete schedule must match the intended case and its output
cadence.

The distinction is:

- The FM component may already be initialized by DIMR before WAVE
  initialization. Its initialization-time output creates/writes the
  communication file that WAVE reads for geometry; WAVE does not get the
  grid through a direct BMI variable request.
- Reading the grid geometry and preparing maps happens in initialization;
  reading time-varying FM fields (such as water level/currents) happens in
  `get_flow_fields` during `swan_tot` at calculation time.
- If neither FM input nor WAVE output is configured, the code need not
  initialize/read FM grids for WAVE.

References:
[BMI `initialize` wrapper](../../packages/wave/src/wave_bmi.f90#L63),
[WAVE initialization order and `flow_init` conditions](../../packages/manager/src/wave_main.f90#L179),
[FM file check](../../packages/data/src/flow_data.f90#L68),
[grid-file read and pairwise map preparation](../../packages/kernel/src/grids_and_gridmaps.f90#L82),
[FM NetCDF grid reader call](../../packages/kernel/src/grids_and_gridmaps.f90#L142),
[runtime field acquisition](../../packages/manager/src/swan_tot.f90#L160),
[DIMR initializes master before WAVE](../../../dimr/packages/dimr_lib/src/dimr.cpp#L429),
[FM BMI initialization runs `flowinit`](../../../dflowfm/packages/dflowfm_lib/src/unstruc_bmi.F90#L233),
[FM initialization writes initial external output](../../../dflowfm/packages/dflowfm_kernel/src/dflowfm_manager/unstruc_api.F90#L263),
[FM online com-file output via `wricom`](../../../dflowfm/packages/dflowfm_kernel/src/dflowfm_manager/flow_externaloutput.F90#L216),
[FM `wricom` writes the NetCDF file](../../../dflowfm/packages/dflowfm_kernel/src/dflowfm_io/wricom.f90#L44),
[DIMR schedules component updates](../../../dimr/packages/dimr_lib/src/dimr.cpp#L968),
[example DIMR Wave schedule](../../../../../examples/dflowfm/08_dflowfm_sequential_dwaves/dimr_config.xml),
[Wave BMI update forwards `dt`](../../packages/wave/src/wave_bmi.f90#L130).

## 21. Where are D-Flow FM fields regridded to SWAN and back?

For the FM NetCDF route, the geometry-based mapping setup happens during
WAVE initialization in `grids_and_gridmaps`. It constructs a mapping for
each configured FLOW-grid/SWAN-grid pair. The two directions use different
mapping implementations:

| Direction | Setup during initialization | Applied to fields during calculation |
|---|---|---|
| D-Flow FM → SWAN | `make_grid_map(j, i, f, s, f2s, netcdf_files)` invokes `ESMF_RegridWeightGen` for the FM NetCDF route, then reads its sparse `row`, `col`, and `S` weights into `flow2swan_maps`. | `get_flow_fields` reads time-varying values from the NetCDF file with `get_var_netcdf`, then calls `grmap_esmf` to apply the stored weights to each requested field. |
| SWAN → D-Flow FM | `make_grid_map(i, j, s, f, s2f, .false.)` uses `mkmap` to build the internal reference and interpolation-weight tables in `swan2flow_maps`. This direction is not delegated to ESMF. | `swan_tot` calls `map_swan_output`, which applies the stored tables with `grmap` to map wave output fields onto the FM grid before `put_wave_fields` writes the NetCDF com-file. |

The mapping weights are **not recomputed for each time step**. They are
prepared at initialization and then reused while the grid geometry stays
unchanged; at each calculation time, the current field values are read or
produced and the relevant stored mapping is applied. In FM→SWAN, the
`grmap_esmf` application uses the sparse ESMF `row`, `col`, and `S` arrays
already held in the `grid_map`. In SWAN→FM, `grmap` uses the `ref_table`
and `weight_table` produced by `mkmap`.

“Once” means once per configured source/destination grid pair and per WAVE
initialization, not one universal map for all domains. With multiple FM
partitions or SWAN grids, setup runs for the corresponding pairs. The
FM→SWAN external weight generation is therefore associated with each
FLOW-to-SWAN pair; the reverse map is also built separately for each
SWAN-to-FLOW pair. These are spatial remapping weights, distinct from the
time selection of records in the NetCDF com-file.

### Standalone WAVE reading existing D-Flow FM output

When standalone WAVE is configured to use an existing FM NetCDF
communication/output file as FLOW input (`useflowdata`), and is not
configured to write wave fields back (`swwav` / `WriteCOM`), the required
remapping is **one-way: D-Flow FM → SWAN**. During `wave_init`,
`flow_init` checks the configured NetCDF file and `grids_and_gridmaps`
reads its FM grid, then builds the FM-to-SWAN ESMF map. At each WAVE
calculation, `get_flow_fields` reads the needed FM fields and calls
`grmap_esmf` with the stored weights. Standalone WAVE does not advance
D-Flow FM; it consumes the fields already present in the file.

The reverse map is only constructed/applied if standalone WAVE is also
configured to write wave fields to the FLOW grid (`swwav` / `WriteCOM`).
In that optional file-writeback case the same initialization sets up both
directions, and `swan_tot` maps SWAN output back with
`map_swan_output`/`grmap` before `put_wave_fields` writes it. The presence
of an FM NetCDF file alone does not make the coupling bidirectional.

```text
Standalone read-only FM input:
  grids_and_gridmaps
    FM -> SWAN: make_grid_map(..., netcdf_files)
      -> ESMF_RegridWeightGen -> load sparse weights in grid_map

Each WAVE calculation:
  FM -> SWAN: get_var_netcdf -> grmap_esmf(stored ESMF weights)

Optional standalone WriteCOM:
  Initialization: SWAN -> FM make_grid_map(..., .false.) -> mkmap
  Each WAVE calculation: map_swan_output -> grmap(stored tables)
                         -> put_wave_fields(NetCDF com-file)
```

References:
[pairwise initialization calls](../../packages/kernel/src/grids_and_gridmaps.f90),
[`make_grid_map` and external weight loading](../../packages/data/src/swan_flow_grid_maps.f90),
[`mkmap` weight construction](../../packages/data/src/mkmap.f90),
[FM-to-SWAN field read and mapping](../../packages/io/src/get_flow_fields.f90),
[`get_var_netcdf`](../../packages/io/src/get_var_netcdf.f90),
[`grmap_esmf`](../../packages/io/src/grmap_esmf.f90),
[`map_swan_output`](../../packages/kernel/src/map_swan_output.f90),
[`grmap`](../../packages/kernel/src/grmap.f90),
[runtime mapping and com-file writeback](../../packages/manager/src/swan_tot.f90).

## Points to carry into later refinement

- Always distinguish Delft3D4 FLOW (`d_hydro`, DelftIO, legacy com-files)
  from D-Flow FM (DIMR/BMI, NetCDF communication files).
- Keep execution mode, forcing source, file format and grid mapping distinct.
- Show FLOW input and external meteo as optional inputs, not mandatory
  standalone dependencies.
- Distinguish the WAVE executable from a coupled launcher: WAVE does not
  start a FLOW engine, and executable use does not by itself imply mode 0.
- Keep mode 2 within the classic Delft3D4 pathway.
- Preserve route-dependent standalone scheduling caveats.
- Retain function/state references when simplifying explanatory diagrams.
- Keep DOT as the diagram source of truth and preserve the preCICE reference
  content outside the current-pathway review.
