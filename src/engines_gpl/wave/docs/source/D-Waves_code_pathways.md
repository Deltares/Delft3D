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

- Section 3.3.1 (printed page 11): project-versus-model workflows and DIMR
  configuration export.
- Sections 5.4.1.1–5.4.1.2 (printed pages 35–36): D-Flow FM input from
  communication files and the online-coupling timeframe.
- Section 5.4.9 (printed pages 60–61): output to the Flow grid and
  communication-file requirements.
- Appendix A.1.1 (printed pages 85–87): MDW keywords, including `FlowFile`
  and `FlowMudFile`.
- Appendix A.1.2 (printed page 89): offline calculations using existing
  FLOW results.
- Appendix A.1.3.4.2 (printed page 94): an FM–D-Waves MDW example using
  `[Output] COMFile` and `WriteCOM`.

The manual describes model setup; the source references below establish the
actual entry points and branching conditions. Its section 2.1.3 refers readers
to the D-Flow FM manual for further FM coupling details; it does not itself
specify the DIMR/BMI call contract. It also does not describe the classic
Delft3D4/DelftIO control protocol or the implementation's grid-remapping
algorithms. Those details are documented from source on the pathway pages.
The preCICE reference section is outside this review.

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

## Detailed pathway pages

The overview diagram and comparison above summarize the three current
pathways. Subpages contain the full call sequences, data routes, mapping and
timing details, and each pathway's detailed diagram and DOT source:

- [Standalone execution and optional saved FLOW input](./D-Waves_pathway_standalone.md)
- [Online coupling with Delft3D4 FLOW, including its mode-2 fluid-mud extension](./D-Waves_pathway_delft3d4.md)
- [D-Flow FM orchestration through DIMR/BMI](./D-Waves_pathway_fm_bmi.md)

Mode 2 remains part of the classic Delft3D4 pathway, not a fourth pathway or
an implemented FM/BMI mud option. Standalone FM-file use is covered by the
standalone page; the same communication-file route can be used for live
FM/BMI orchestration.

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

The overview SVG continues to show the three current pathways and a separate,
dashed preCICE reference lane outside the current pathway set. The overview
diagram and its preCICE comparison are unchanged; configuration-dependent
Flow-file inputs and writeback are detailed on the pathway pages above.

The overview DOT file is the source of truth for this image. To regenerate
the overview SVG, run from `src/engines_gpl/wave/docs/include`:

```sh
dot -Tsvg dwaves_code_pathways.dot -o dwaves_code_pathways.svg
```

Each detailed SVG and its DOT source is linked from its corresponding
pathway page.
