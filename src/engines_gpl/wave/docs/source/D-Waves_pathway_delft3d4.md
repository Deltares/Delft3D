# D-Waves online pathway with Delft3D4 FLOW

This page describes the classic structured-grid `d_hydro` / WAVE process
pair. Delft3D4 FLOW and D-Flow FM are distinct engines: this pathway uses
DelftIO for step synchronization and legacy FLOW com-files for gridded
fields. Mode 2 is a fluid-mud extension of this pathway.

## Detailed diagram

![Detailed Delft3D4 FLOW-D-Waves pathway](../include/dwaves_pathway_delft3d4_detailed.svg)

Graphviz source: [DOT](../include/dwaves_pathway_delft3d4_detailed.dot).

## Startup and initialization

The coupled-run script starts `d_hydro` and `wave <mdw-file> 1` as separate
processes. The WAVE mode argument selects `flow_online = 1`; a blank
`swan_run%flowgridfile` selects the classic DelftIO route. WAVE does not call
Delft3D4 FLOW's `tricom_init`: FLOW reaches that routine through its own
`trisim` / `trisim_init` initialization path.

On the FLOW side, the MDF `WaveOL` option determines whether the FLOW engine
uses offline, mimic, or online behavior; online coupling initializes DelftIO
when wave support is enabled. That FLOW-side setting does not launch WAVE:
both processes must be started and configured to rendezvous.

On the WAVE side, `flow_init` waits for FLOW's DelftIO initialization,
receives domain identifiers and timing information, and establishes the
classic connection. WAVE then initializes FLOW and SWAN grids and prepares
the spatial maps. File/grid setup and field acquisition are separate:
`flow_init` establishes the connection, while `get_flow_fields` reads
configured fields during each WAVE calculation.

## Step synchronization and data channels

Delft3D4 FLOW owns the simulation time loop. At its configured wave/com-file
coupling points (`itwav = itcomc`), the FLOW time loop issues a perform-step
command and timestep index through DelftIO. WAVE receives the command, updates
its time with `settimtscale`, runs the calculation, then reports completion.
FLOW waits for that status before continuing. WAVE is called at the configured
coupling cadence, not necessarily at every internal FLOW timestep.

The control and field channels have different roles:

| Channel | Contents |
|---|---|
| DelftIO `FLOW2WAVE_DATA` / `WAVE2FLOW_DATA` (and the corresponding MUD streams in mode 2) | Initialization, step/finalize commands, FLOW timestep index, and WAVE completion status. |
| Legacy FLOW com-file(s), usually `com-<runid>` | Gridded FLOW fields read by WAVE and, when `WriteCOM` is enabled, selected WAVE results written back for FLOW. |

DelftIO does not carry the gridded hydrodynamic or wave fields. WAVE reads
configured bed level, water level, velocity, wind or other selected inputs
from the legacy com-file through helpers such as `get_dep`, `get_lev`,
`get_cur` and `get_wind`. FLOW-current vector conversion (`flow2wav`) is
distinct from spatial interpolation. WAVE maps the fields from the FLOW
grid(s) onto each SWAN grid using the precomputed map.

When `[Output] WriteCOM` is enabled, WAVE maps selected SWAN outputs back to
the FLOW grid, converts them for the legacy communication format, and writes
them with `put_wave_fields`. FLOW discovers and reads the resulting wave
record through `setwav` at its subsequent read point.

## Grid mapping and calculation

During WAVE initialization, `grids_and_gridmaps` prepares FLOW-to-SWAN and
SWAN-to-FLOW maps for each configured grid pair. The classic route builds
interpolation references and weights with `mkmap`; `grmap` applies those
weights at runtime. Mapping weights are reused at each coupling point rather
than recomputed for every field or timestep.

The grids need not have identical dimensions or extents. Physical
coordinates must be compatible, and FLOW domains/partitions should cover
the parts of the SWAN computational grids that require FLOW forcing. The
counts of SWAN grids, FLOW grids and MPI ranks are separate concepts.

At each step `swan_tot` stages the SWAN inputs, calls `run_swan`, reads SWAN's
results, and performs configured output mapping. The surrounding synchronization
still uses DelftIO; the WAVE-to-SWAN input/output file interface is internal
to WAVE and does not replace the FLOW-WAVE protocol.

## Fluid-mud mode (mode 2)

`flow_mud_online = 2` is a conditional extension of the classic mode-1
pathway, not a separate driver, time loop, or FM/BMI feature. It adds:

- A second mud-model connection and mud-domain identifiers.
- A wait for both water FLOW and mud commands, and completion reporting to
  both models.
- Reads of mud density, viscosity, depth and level, which configure SWAN's
  mud-layer input and physics.

The implemented case requires one water domain and one mud domain with
identical grids. It is not a general suspended-sediment switch.

The manual's Appendix A.1.1 (printed pages 85–87) defines `FlowFile` as
selecting online FLOW input and `FlowMudFile` as selecting the mud phase of
a two-phase FLOW model. Those keyword descriptions do not specify the
engine-specific branch behavior below; that distinction is verified against
the current code.

The parser's startup message saying that WAVE runs online with FLOW,
including MUD, reports mode selection only. Nonblank `FlowFile` and
`FlowMudFile` can assign mode 2, but this does not verify which route is
selected or that the models have connected. With a nonblank FM
`swan_run%flowgridfile`, `flow_init` takes the FM NetCDF branch and does not
initialize the mud connection or `mudids` required by the mud-field reader.
FM/BMI mud coupling is therefore not implemented by this path. The BMI
wrapper also rejects the mode-2 end-time and timestep queries as not
implemented.

## Implementation references

- [D-Waves User Manual, Appendix A.1.1](../include/D-Waves_User_Manual.pdf)
  (printed pages 85–87): MDW `FlowFile` and `FlowMudFile` keyword descriptions.
- [Delft3D4 coupled launcher](../../../flow2d3d/scripts/run_dflow2d3d_dwaves.sh#L131)
- [Delft3D4 FLOW initialization path](../../../flow2d3d/packages/flow2d3d_manager/src/trisim.F90#L127)
- [FLOW communication initialization](../../../flow2d3d/packages/flow2d3d_manager/src/tricom_init.F90#L1478)
- [FLOW coupling schedule and wave reads](../../../flow2d3d/packages/flow2d3d_manager/src/tricom_step.F90#L660)
- [DelftIO synchronization commands and status](../../../../utils_lgpl/delftio/packages/delftio/src/delftio_sync/sync_flowwave.f90#L330)
- [WAVE online loop and mode-2 waits](../../packages/manager/src/wave_main.f90#L405)
- [Classic FLOW field reads and mapping](../../packages/io/src/get_flow_fields.f90#L102)
- [Pairwise grid-map setup](../../packages/kernel/src/grids_and_gridmaps.f90#L165)
- [Mode-2 connection setup](../../packages/data/src/flow_data.f90#L139)
- [Mud-field input](../../packages/io/src/get_flow_fields.f90#L317)
- [SWAN mud configuration](../../packages/data/src/swan_input.f90#L3676)
- [FM branch in FLOW initialization](../../packages/data/src/flow_data.f90#L68)
