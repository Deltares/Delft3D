# D-Waves code pathways

This note describes the D-Waves execution paths in the current checkout for
standalone use, Delft3D4 online coupling, and D-Flow FM orchestration through
BMI. It also compares those paths with the
[UNST-9075 preCICE proof-of-concept branch](https://github.com/Deltares/Delft3D/tree/all/poc/UNST-9075_preCICE_Proof_of_Concept).
The diagram is available as [SVG](../include/dwaves_code_pathways.svg); its Graphviz
source is [DOT](../include/dwaves_code_pathways.dot).

![D-Waves execution paths](../include/dwaves_code_pathways.svg)

## Manual and implementation references

The three current pathways below have been cross-checked against the
[D-Waves User Manual](../include/D-Waves_User_Manual.pdf), version 2027.01,
revision 81307, dated 15 October 2026. Relevant sections are:

- Section 3.3.1: running models separately or coupled and exporting a DIMR
  configuration.
- Sections 5.4.1.1–5.4.1.2: Flow communication files and the online coupling
  timeframe.
- Section 5.4.9: output to the Flow grid and computational grids.
- Appendix A.1.2: offline calculations using existing FLOW results.
- Appendix A.1.3.4.2: an FM–D-Waves example using `[Output] COMFile`.

The manual describes model setup; the source references below establish the
actual entry points and branching conditions. Its section 2.1.3 refers readers
to the D-Flow FM manual for further FM coupling details; it does not itself
specify the BMI call contract. The preCICE reference section is outside this
review.

## At a glance

| Scenario | Entry point | Coordination and data path |
|---|---|---|
| Standalone | `wave` executable (`wave_exe`) | `wave_main` runs SWAN from the MDW schedule. Optional Flow data can be read from configured Flow files. |
| Delft3D4 online | `wave` executable with mode `1`, launched alongside `d_hydro` | Flow and Wave rendezvous and exchange step commands/status through DelftIO (`sync_flowwave`); the model fields themselves are read from and written to FLOW communication (com) files. |
| D-Flow FM / BMI | `wave` shared library, loaded as a DIMR component | DIMR initializes and advances the BMI components. D-Waves reads/writes the NetCDF communication file selected by `[Output] COMFile`; this is not direct field-array exchange through the Wave BMI `get_var` interface. |

The `stand_alone`, `flow_online`, and `flow_mud_online` mode values are `0`,
`1`, and `2`, respectively, in
[wave_data.f90](../../packages/data/src/wave_data.f90). The executable defaults
to mode `0`; its optional second command-line argument overrides the mode.
The BMI library starts with an unset mode and must be given a mode before its
`initialize` entry point is called.

The initial mode is not the only selector: MDW `[General] FlowFile`, when
nonblank, sets `wavedata%mode` to `flow_online`; a nonblank `FlowMudFile`
inside that branch selects `flow_mud_online`
([read_dwaves_mdw](../../packages/data/src/swan_input.f90#L915)).
Removing `FlowFile` does not reset a mode already selected by the executable
or BMI caller.

### Which mode couples to which FLOW engine?

**Both D-Flow FM and Delft3D4 FLOW normally use `flow_online = 1`.**
There is no separate numeric FM mode. The mode selects standalone versus
online behavior (with mode 2 adding fluid-mud coupling); it does not by itself
identify the FLOW engine.

| Coupling scenario | WAVE mode | Pathway selector and coordination |
|---|---|---|
| D-Flow FM through DIMR/BMI | `flow_online = 1` | DIMR selects mode 1. A nonblank `[Output] COMFile` populates `swan_run%flowgridfile`, selecting the FM NetCDF route; BMI `update(dt)` drives stepping. |
| Delft3D4 FLOW online | `flow_online = 1` | With blank `swan_run%flowgridfile`, WAVE uses the DelftIO initialization/command/status handshake and legacy FLOW com-files. |
| Delft3D4 FLOW plus fluid mud | `flow_mud_online = 2` | Extends the classic Delft3D4 pathway with a separate mud connection and mud inputs for SWAN. |

The route selection is visible in
[flow_init](../../packages/data/src/flow_data.f90#L79) and
[wave_master_step](../../packages/manager/src/wave_main.f90#L430).
A nonblank FM file path can also be used in standalone mode for offline
file-backed input; the path alone does not establish live coupling.

`flow_mud_online` is an extension of the classic Delft3D4 online pathway,
**not an FM/BMI coupling option in the current implementation** and not a
general suspended-sediment switch. It follows the same initialization,
stepping, SWAN orchestration and finalization chain as mode 1, with conditional
mud operations at several stages rather than a separate driver or time loop.
It couples the water-phase FLOW model and a fluid-mud computation:
WAVE waits for both models and reports completion to both. It reads mud
density, viscosity, depth and level from the mud com-file to configure SWAN's
mud-layer physics. The implementation requires one water domain and one mud
domain and assumes their grids are identical. See
[mud initialization and domain checks](../../packages/data/src/flow_data.f90#L139),
[mud stepping](../../packages/manager/src/wave_main.f90#L445),
[mud field acquisition](../../packages/io/src/get_flow_fields.f90#L317), and
[SWAN mud configuration](../../packages/data/src/swan_input.f90#L3676).

Although MDW parsing can assign mode 2 when both `FlowFile` and `FlowMudFile`
are nonblank, this does not establish an FM mud pathway: the FM NetCDF branch
of `flow_init` does not initialize the mud connection or `mudids`, which the
mud field reader needs. Represent mode 2 as conditional additions within the
Delft3D4 pathway, not as a fourth top-level pathway or an alternative FM mode.

### MDW keys versus internal state

| Input setting | Implementation state / effect |
|---|---|
| `[General] FlowFile` | Selects online mode during MDW parsing. It is not the FM NetCDF communication-file path. |
| `[General] ComFile` | Populates `swan_run%comfile`: one or more legacy FLOW runids for offline NEFIS com-file input. |
| `[Output] COMFile` | Populates `swan_run%flowgridfile`: the FM NetCDF communication-file path, whose name must end in `com.nc`. |
| Flow input selections (`FlowBedLevel`, `FlowWaterLevel`, `FlowVelocity`, `FlowWind`, `FlowVegetation`) | Contribute to the internally derived `swan_run%useflowdata` flag. External meteo input can take precedence over Flow wind. |
| `[Output] WriteCOM` | Populates `swan_run%swwav`, enabling WAVE results to be written back to communication files; default is false. |

There is no MDW `FlowGridFile` or `useflowdata` input keyword in this parser.
The older `[Output] FlowGridForCom` keyword is explicitly rejected. See
[MDW output parsing](../../packages/data/src/swan_input.f90#L1516),
[Flow-input selection](../../packages/data/src/swan_input.f90#L2074), and
[Flow file initialization](../../packages/data/src/flow_data.f90#L79).

## Standalone

1. The `wave` executable in
   [wave_exe.f90](../../packages/wave/src/wave_exe.f90) parses the MDW filename
   and optional mode. With no mode argument it uses `0`.
2. It calls `wave_main_init`, then `wave_main_step(-1)`, then
   `wave_main_finish` in
   [wave_main.f90](../../packages/manager/src/wave_main.f90).
3. Initialization reads the MDW input, configures SWAN grids and grid maps,
   and prepares any configured external forcing. The manager calls
   `swan_tot` to stage SWAN input, run SWAN, and consume its results.
4. In mode `stand_alone`, finalization releases the SWAN and forcing resources.
   Scheduling depends on the file route: with blank `swan_run%flowgridfile`,
   `wave_master_step` calls `swan_tot(..., selectedtime=0)`, whose own loop
   processes the MDW times. With a nonblank FM file path, the manager instead
   selects times within the supplied step interval (or computes once if no
   timepoints were specified). Thus `wave_main_step(-1)` is not a universal
   "run all times" operation for both routes. See
   [wave_master_step](../../packages/manager/src/wave_main.f90#L485) and
   [swan_tot](../../packages/manager/src/swan_tot.f90#L106).

Standalone does not require a live Flow process. It can nevertheless be
configured to use existing Flow data. Appendix A.1.2 of the manual describes
the legacy offline workflow: remove `[General] FlowFile`, use mode 0, and
specify calculation timepoints that match stored FLOW output times. By
default, the legacy com-file runid matches the WAVE runid; `[General] ComFile`
can select another runid or a list of partition/domain runids. This corresponds
to the standalone branch of
[flow_init](../../packages/data/src/flow_data.f90#L95).

For the FM NetCDF route, `[Output] COMFile` supplies the path, and `flow_init`
checks that file or its partition variants even in standalone mode, when Flow
input or com-file output is requested
([flow_init](../../packages/data/src/flow_data.f90#L79)).
This is file-backed reuse, not live online synchronization; the FM time-window
behavior above still applies.

Relevant implementation: [wave_exe.f90](../../packages/wave/src/wave_exe.f90),
[wave_main.f90](../../packages/manager/src/wave_main.f90), and
[flow_data.f90](../../packages/data/src/flow_data.f90).

Detailed diagram (actual subroutine calls and module-level state):
[dwaves_pathway_standalone_detailed.svg](../include/dwaves_pathway_standalone_detailed.svg)
([DOT source](../include/dwaves_pathway_standalone_detailed.dot)).

## Coupling with Delft3D4

The classic online path uses two processes. The
[D-Waves run script](../../../flow2d3d/scripts/run_dflow2d3d_dwaves.sh) starts
`d_hydro` and starts `wave <mdw-file> 1`; the latter selects `flow_online`.
The Delft3D4 side initializes and steps the coupling from
[tricom_init.F90](../../../flow2d3d/packages/flow2d3d_manager/src/tricom_init.F90)
and its FLOW time loop.

The synchronization contract is implemented by
[sync_flowwave.f90](../../../../utils_lgpl/delftio/packages/delftio/src/delftio_sync/sync_flowwave.f90):

- FLOW initializes the connection and publishes its domain names, reference
  date, timestep, and coupling configuration.
- D-Waves enters `flow_init`; with blank `swan_run%flowgridfile` (no FM
  `[Output] COMFile`), it waits for
  `wave_from_flow_init` and records the connected FLOW domains.
- On each step, D-Waves waits for FLOW's command and timestep, sets the WAVE
  time, calls `swan_tot`, then reports completion. FLOW and WAVE exchange
  step commands and status flags through DelftIO's `FLOW2WAVE_DATA` and
  `WAVE2FLOW_DATA` streams (`flow_to_wave_command`/`wave_from_flow_command`
  and `wave_to_flow_status`/`flow_from_wave_status`). The synchronization
  module also defines the corresponding MUD streams for the fluid-mud mode.
- The model fields themselves (water levels, currents, bed levels, and wave
  forcing) are not carried by these DelftIO streams: WAVE reads Flow fields
  from the FLOW communication (com) file through `get_flow_fields`, and
  writes its results back to the com-file through `wave2flow`, `wave2com`,
  and `put_wave_fields` when `[Output] WriteCOM` enables this
  ([swan_tot](../../packages/manager/src/swan_tot.f90#L499)). FLOW reads the wave
  results back from the same com-file through `setwav` in `tricom_step.F90`.
- The FLOW manager handles the coupling for serial or parallel FLOW runs;
  the DelftIO layer carries the domain/partition information for the control
  channel described above.

The online loop is in
[wave_main.f90](../../packages/manager/src/wave_main.f90), and the mode selection
is defined in [wave_data.f90](../../packages/data/src/wave_data.f90). This path
is not the D-Flow FM BMI path: it is a FLOW/WAVE process pair coordinated by
DelftIO.

Detailed diagram (both processes, the DelftIO sync points, and module-level
state): [dwaves_pathway_delft3d4_detailed.svg](../include/dwaves_pathway_delft3d4_detailed.svg)
([DOT source](../include/dwaves_pathway_delft3d4_detailed.dot)).

## Coupling with D-Flow FM through BMI

The Wave target is built both as an executable and as a shared library. The
library target includes [wave_bmi.f90](../../packages/wave/src/wave_bmi.f90)
through [wave_dll.cmake](../../packages/wave/wave_dll.cmake).
The BMI `initialize`, `update`, and `finalize` wrappers delegate to
`wave_main_init`, `wave_main_step(dt)`, and `wave_main_finish`.

In DIMR's parallel component initialization,
[dimr.cpp](../../../dimr/packages/dimr_lib/src/dimr.cpp) initializes the master
component first (the code notes that Wave must be initialized after Flow).
For a Wave component, DIMR obtains the BMI `mode` variable and sets it to
`1` before calling Wave's BMI `initialize`. DIMR then advances the BMI
components and coordinates their configured couplers.

There are two important details about what “via BMI” means in the current
code:

1. The Wave BMI wrapper's `get_var` exposes `mode` and
   `engine_comm_world`; its `set_var` accepts the `mode` key. It does not
   expose the SWAN grid and Flow-forcing fields as generic BMI variables.
2. The actual Flow-input route depends on the MDW configuration. With a
   nonblank `[Output] COMFile` (`swan_run%flowgridfile`), `flow_data` recognizes the FM NetCDF/com-file
   path, checks for partition files, and `swan_tot` obtains Flow forcing
   through `get_flow_fields` when `swan_run%useflowdata` is true. In
   `flow_online` mode with blank `swan_run%flowgridfile`,
   the manager instead waits on the DelftIO `wave_from_flow_command` path.

For the FM route, each `update(dt)` advances `wavedata%time%timsec` by the
supplied interval, then calls `swan_tot`; the non-standalone FM initialization
sets `swan_run%nttide = 1`. This differs from processing a list of offline
timepoints
([wave_init](../../packages/manager/src/wave_main.f90#L180),
[wave_master_step](../../packages/manager/src/wave_main.f90#L432)).

The manual's sections 5.4.1.1 and 5.4.9 confirm that FM and WAVE use a
communication file, including for returning WAVE results on the Flow grid.
Flow input and Flow-grid output are independent selections: even without
hydrodynamic input, Flow-grid output requires an existing communication file.
In the code, `flow_init` checks files when either `useflowdata` or `swwav` is
true, while `swan_tot` writes back only when `swwav` (`WriteCOM`) is true.
For FM, the writeback uses `wave2com` and `put_wave_fields`; the preceding
`wave2flow` vector conversion is specific to the legacy, blank-file-path
branch
([flow_init](../../packages/data/src/flow_data.f90#L79),
[swan_tot](../../packages/manager/src/swan_tot.f90#L499)).

Consequently, DIMR/BMI provides the current component lifecycle and stepping
interface, while configured file-backed Flow input is a separate part of the
D-Waves data path. Do not infer direct in-memory Flow-to-Wave array exchange
from the BMI component registration alone.

Relevant implementation: [wave_bmi.f90](../../packages/wave/src/wave_bmi.f90),
[wave_dll.cmake](../../packages/wave/wave_dll.cmake),
[dimr.cpp](../../../dimr/packages/dimr_lib/src/dimr.cpp),
[flow_data.f90](../../packages/data/src/flow_data.f90), and
[swan_tot.f90](../../packages/manager/src/swan_tot.f90).

Detailed diagram (DIMR, the BMI wrapper, the `swan_run%flowgridfile` branch
decision, and module-level state):
[dwaves_pathway_fm_bmi_detailed.svg](../include/dwaves_pathway_fm_bmi_detailed.svg)
([DOT source](../include/dwaves_pathway_fm_bmi_detailed.dot)).

## Reference comparison: UNST-9075 preCICE proof of concept

The proof-of-concept branch is used here only as a reference for comparison.
Its D-Waves changes are not in the current checkout or main branch and must
not be read as a supported current D-Waves pathway. The reference branch adds
preCICE-specific hooks in its D-Waves manager:

- Its [`wave_main.F90`](https://github.com/Deltares/Delft3D/blob/all/poc/UNST-9075_preCICE_Proof_of_Concept/src/engines_gpl/wave/packages/manager/src/wave_main.F90)
  adds preCICE lifecycle entry points, including `initialize_fm_coupling`,
  `is_fm_coupling_ongoing`, and `advance_fm_time_window`.
- The initialization path reads the SWAN grid from the MDW file and registers
  active vertices and triangles on a preCICE mesh.
- Its [`swan_tot.F90`](https://github.com/Deltares/Delft3D/blob/all/poc/UNST-9075_preCICE_Proof_of_Concept/src/engines_gpl/wave/packages/manager/src/swan_tot.F90)
  passes preCICE state into Flow-field acquisition and writes selected SWAN
  results (including wave height/direction/period, forces, momentum, and
  dissipation fields) to preCICE.
- The proof-of-concept advances preCICE time windows explicitly. That
  protocol is distinct from the current DelftIO handshake and from DIMR's
  current BMI lifecycle.

The comparison also shows that the Wave BMI wrapper and executable source
are not themselves the preCICE adapter: the GitHub blobs for
`wave_bmi.f90` and `wave_exe.f90` have the same IDs on the current branch and
the proof-of-concept branch, while the proof-of-concept changes the manager
and SWAN orchestration path. In the current checkout,
[wave_main.f90](../../packages/manager/src/wave_main.f90) and
[swan_tot.f90](../../packages/manager/src/swan_tot.f90) have no preCICE state
or lifecycle calls. Treat the proof-of-concept as comparison material only,
not as a current implementation or evidence that FM/BMI coupling uses
preCICE.

## Diagram notes

The SVG shows the three current pathways and a separate, dashed preCICE
reference lane, explicitly outside the current pathway set. Arrows to/from
Flow files are configuration-dependent:
`[Output] COMFile` sets the internal `swan_run%flowgridfile` path; Flow-input
selections determine `swan_run%useflowdata`, and `[Output] WriteCOM` controls
writeback. The overview retains its three-column layout and preCICE reference
panel; the reviewed labels distinguish MDW keys from internal state and
describe the route-dependent standalone scheduling.

The DOT files are the source of truth. To regenerate the SVGs, run from
`src/engines_gpl/wave/docs/include`:

```sh
for diagram in dwaves_code_pathways dwaves_pathway_standalone_detailed \
               dwaves_pathway_delft3d4_detailed dwaves_pathway_fm_bmi_detailed; do
    dot -Tsvg "$diagram.dot" -o "$diagram.svg"
done
```
