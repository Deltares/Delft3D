# D-Waves communication models

This documentation explains **who controls time, where fields are exchanged,
and who performs remapping** for the three current execution scenarios.
Implementation links on each page connect those responsibilities to code.

\dotfile dwaves_communication_model.dot "D-Waves communication models"

[DOT source](../include/dwaves_communication_model.dot)

## Available scenarios

| Scenario | Time/control owner | Field channel | Details |
|---|---|---|---|
| Standalone | D-Waves calculation schedule; no live Delft3D4 or D-Flow FM engine | Optional existing Delft3D4 or D-Flow FM com-files | [Standalone model](./D-Waves_pathway_standalone.md) |
| Delft3D4 online | Delft3D4 time loop; DelftIO step commands and completion status | Delft3D4 NEFIS com-files read/written by Delft3D4 and D-Waves | [Delft3D4 model, including mud](./D-Waves_pathway_delft3d4.md) |
| D-Flow FM online | DIMR/BMI lifecycle and component schedule | NetCDF com-files read/written by D-Flow FM and D-Waves | [D-Flow FM model](./D-Waves_pathway_fm_bmi.md) |

In all three cases, D-Waves manages the SWAN computations and maps fields
between the hydrodynamic (Delft3D4 or D-Flow FM) and SWAN grids. In standalone
runs, reading saved hydrodynamic input and writing wave fields
(`[Output] WriteCOM`) are independent choices. **DelftIO and DIMR online runs
require `WriteCOM = true`:** otherwise D-Waves aborts during input checking.
The `--online=precice` selection is present in the branch, but its coupling
initialization and field exchange are not implemented yet.

\dotfile dwaves_code_pathways.dot "D-Waves code entry points per pathway"

[DOT source](../include/dwaves_code_pathways.dot)

The diagram above is a developer view: it lists the main code entry points of
each pathway side by side. Its preCICE panel marks the unfinished implementation
steps; see the preCICE status and reference comparison at the end of this page.

## Communication models

These pathways differ in who controls the calculation schedule and how
hydrodynamic and wave fields move between components. **Control is not field
exchange:** DelftIO (Delft3D4) and DIMR/BMI (D-Flow FM) coordinate execution,
while the com-files carry the gridded fields.

### Delft3D4 online

Delft3D4 owns the simulation time loop. Delft3D4 and D-Waves run as separate
processes. DelftIO carries initialization metadata, step/time commands and
completion status; it does not carry the gridded hydrodynamic or wave fields.
Those fields are read from and, when `WriteCOM` is enabled, written to
Delft3D4 com-files, which use NEFIS. This is a different file format from the
NetCDF communication files used by D-Flow FM. Delft3D4's `FLNcdf` keyword can
make the map, Fourier, history and drogue files NetCDF, but Delft3D4 stops if
`com` (or `all`) is listed for NetCDF output
([rdprfl.f90](../../../flow2d3d/packages/flow2d3d_io/src/input/rdprfl.f90#L162)),
so the com-file used for D-Waves coupling is always NEFIS.

The D-Waves process starts SWAN as a separate executable, which uses OpenMP for
parallel computation. Delft3D4 domain partitioning is separate from SWAN's
OpenMP parallelism. For more on the synchronization and mode-2 water/mud
exchange, see the [Delft3D4 pathway details](./D-Waves_pathway_delft3d4.md).

## Selecting the connection mode

The internal `wave_flow_mode_type` stores two independent selections:
`online` (`none`, `delftio`, `dimr` or `precice`) and `com_file` (`none`,
NEFIS or NetCDF). This replaces the old internal integer `mode` and the
separate `flowgridfile` field.

The D-Waves executable accepts `--online=<value>`:

| Option | Internal connection | Communication-file type |
|---|---|---|
| `--online=none` (default) | Standalone | Selected from the optional MDW com-file |
| `--online=delftio` | Delft3D4 online through DelftIO | NEFIS |
| `--online=dimr` | D-Flow FM online through DIMR | NetCDF |
| `--online=precice` | preCICE selection | None; preCICE coupling is not implemented yet |

The executable still accepts legacy numeric modes for compatibility: `0`
selects standalone, `1` selects DelftIO/NEFIS, and `2` selects DelftIO/NEFIS
with the legacy mud connection. The BMI interface also retains its `mode`
variable. DIMR's `"online with DflowFM"` value maps to `dimr`/NetCDF, while
`"stand-alone"` maps to standalone. These BMI values are not the executable's
numeric modes.

### D-Flow FM online through DIMR/BMI

DIMR controls the component lifecycle and the update schedule. In the usual
configuration, it initializes D-Flow FM before D-Waves and calls the components
according to the configured schedule. Before initializing D-Waves, DIMR sets
the BMI `mode` variable. D-Waves translates the online value to the internal
`dimr` connection and NetCDF com-file type. The value depends on where D-Waves
appears in the `<control>` section of `dimr_config.xml`:

- D-Waves is a `<start>` inside a `<startGroup>` within a `<parallel>` block,
  with D-Flow FM as the `<parallel>` block's own `<start>` (as in the
  [example configuration](../../../../../examples/dflowfm/08_dflowfm_sequential_dwaves/dimr_config.xml)):
  DIMR sets the BMI mode to `"online with DflowFM"`.
- D-Waves is a plain `<start>` directly under `<control>`, as the first block
  and outside any `<parallel>` block, so that DIMR drives D-Waves on its own:
  DIMR sets the BMI mode to `"stand-alone"` (no D-Flow FM coupling).

In the coupled layout, D-Flow FM and D-Waves exchange fields through NetCDF
communication files. BMI controls component execution but does not transfer
gridded fields.

For parallel D-Waves runs configured with `SwanMode=lib`, multiple D-Waves MPI
processes participate in the SWAN solve: the master handles input/output and
mapping, while each D-Waves rank calls `swan_lib` collectively. This
D-Waves/SWAN MPI parallelism is distinct from D-Flow FM partitioning and
DIMR's component schedule. See the
[D-Flow FM/BMI pathway details](./D-Waves_pathway_fm_bmi.md).

### Standalone

Standalone D-Waves owns its calculation schedule and does not start or advance
a Delft3D4 or D-Flow FM engine. It may read saved Delft3D4 NEFIS or D-Flow FM
NetCDF communication files. Reading hydrodynamic fields and writing wave fields
with `WriteCOM` are independent choices; writing fields does not make the run
live coupling. See the
[standalone pathway details](./D-Waves_pathway_standalone.md).

### Shared execution model

- D-Waves prepares both map directions per grid pair at initialization. It
  applies the reverse mapping and writes wave fields only when `WriteCOM`
  enables writeback (always the case online).
- D-Waves visits the SWAN grids in the configured order; nested grids use the
  boundary output of their parent grid. The numbers of Delft3D4 or D-Flow FM
  partitions, SWAN nests and D-Waves ranks are independent of each other.
- In Delft3D4 online runs, SWAN is started as an executable that uses OpenMP.
  In parallel D-Flow FM/BMI runs with `SwanMode=lib`, the D-Waves MPI ranks
  call the SWAN library collectively.

### Configuration and compatibility

| Setting | Role |
|---|---|
| `--online=<value>` | Selects standalone (`none`, the default), DelftIO/NEFIS (`delftio`), DIMR/NetCDF (`dimr`), or the reserved preCICE mode (`precice`). preCICE lifecycle and field exchange are not implemented yet. |
| Legacy numeric mode / BMI `mode` | Compatibility inputs at the executable and BMI boundaries. They are translated to the internal `wave_flow_mode_type`; do not use the legacy integer to describe the internal connection model. |
| `wave_flow_mode_type` | Internal pair of `online` and `com_file` selections. Online selections determine the expected communication-file type; standalone file type is determined from the optional com-file configuration. |
| `[General] FlowFile` | Removed as a mode selector. Use `--online` for executable runs; DIMR supplies the online selection through the BMI interface. |
| `[General] ComFile` | Existing Delft3D4 offline runid selection (manual A.1.2). |
| `[General] FlowMudFile` | Deprecated Delft3D4 water/mud coupling input. The mode remains supported for compatibility; it is not supported with DIMR or preCICE. |
| `[Output] COMFile` | D-Flow FM NetCDF communication-file path, typically ending in `com.nc`; used for the D-Flow FM file route, including offline use. |
| Hydrodynamic input selections (`FlowBedLevel`, `FlowWaterLevel`, `FlowVelocity`, ...) | Select which Delft3D4 or D-Flow FM fields D-Waves reads. |
| `[Output] WriteCOM` | Writes wave fields to the communication file(s). Optional in standalone runs; mandatory for DelftIO and DIMR online runs. |

The online selection identifies the connection type; the communication-file
type identifies the file format used for field exchange. Preserve the legacy
`FlowMudFile` water/mud behavior for Delft3D4; D-Flow FM/BMI does not implement
that path.

Configuration reference: [D-Waves User Manual](https://content.oss.deltares.nl/dhydro/D-Waves_User_Manual.pdf),
version 2027.01, revision 81378 (28 October 2026, DRAFT), Sections 3.3.1, 5.4.1.1–5.4.1.2 and 5.4.9
(GUI), Appendices A.1.1 (MDW keywords, including `FlowMudFile` and
`WriteCOM`) and A.1.2 (Delft3D4 offline workflow). The manual predates this
branch's `--online` option and removal of `FlowFile`; source links on the
scenario pages establish the branch behavior.

## preCICE status and reference comparison

This branch adds `--online=precice` and the corresponding internal mode, but
the D-Waves initialization and field-exchange path is still marked TODO. The
option must not be interpreted as a working preCICE coupling.

The UNST-9075 proof-of-concept branch remains useful only as a comparison for
the missing implementation. Its D-Waves changes are not the implementation in
this branch:

The reference branch adds preCICE-specific hooks in its D-Waves manager:

- Its [`wave_main.F90`](https://github.com/Deltares/Delft3D/blob/all/poc/UNST-9075_preCICE_Proof_of_Concept/src/engines_gpl/wave/packages/manager/src/wave_main.F90)
  adds preCICE lifecycle entry points, including `initialize_fm_coupling`,
  `is_fm_coupling_ongoing`, and `advance_fm_time_window`.
- The initialization path reads the SWAN grid from the MDW file and registers
  active vertices and triangles on a preCICE mesh.
- Its [`swan_tot.F90`](https://github.com/Deltares/Delft3D/blob/all/poc/UNST-9075_preCICE_Proof_of_Concept/src/engines_gpl/wave/packages/manager/src/swan_tot.F90)
  passes preCICE state into hydrodynamic-field acquisition and writes selected SWAN
  results (including wave height/direction/period, forces, momentum, and
  dissipation fields) to preCICE.
- The proof-of-concept advances preCICE time windows explicitly. That
  protocol is distinct from the current DelftIO handshake and from DIMR's
  current BMI lifecycle.

Unlike the proof of concept, this branch does not yet have preCICE state,
lifecycle calls, or field exchange in
[wave_main.f90](../../packages/manager/src/wave_main.f90) and
[swan_tot.f90](../../packages/manager/src/swan_tot.f90). Treat the reference as
comparison material only, not as evidence that the branch has a working
preCICE pathway or that D-Flow FM/BMI coupling uses preCICE.
