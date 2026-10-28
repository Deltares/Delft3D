# Proposed preCICE FM–Wave coupling design

**Status: PROPOSAL. Nothing described as "proposed" exists in the code base.**
"Actual" statements cite sources: the PoC trace is in
[poc_precice_detailed_pathway.md](poc_precice_detailed_pathway.md), its effect on the main
pathways in [poc_precice_pathway_impact.md](poc_precice_pathway_impact.md), and the
baseline route in [main_wave_fm_pathway_review.md](main_wave_fm_pathway_review.md).

\dotfile precice_coupling_design_proposal.dot "Proposed design: opt-in preCICE endpoints on the existing route"

[DOT source](../include/precice_coupling_design_proposal.dot) | [SVG preview](../include/precice_coupling_design_proposal.svg)

## 1. Why not take the PoC as is (actual evidence)

* The PoC deletes the legacy online route (`wave2com`, `put_wave_fields`, `comcount`, FM `wricom`)
  and hard-wires preCICE, so no mode remains undisturbed (impact report).
* It builds `precice_fortran` unconditionally, hardcodes `../precice_config.xml`, runs serial
  to preCICE before `initialize_wave_mpi`, supports the first SWAN grid only, and breaks BMI/DIMR
  (dummy state in `unstruc_bmi.F90`, never-created state in `wave_bmi.f90`).
* No working bidirectional run is demonstrated by the sources; the FM-side wave read is
  partially stubbed/debug-printing (see detailed trace, "NOT established").

## 2. Lessons from the main FM adapter (actual)

Directory `dflowfm_kernel/src/dflowfm_manager/precice_adapter/`
([adapter.F90](../../../dflowfm/packages/dflowfm_kernel/src/dflowfm_manager/precice_adapter/adapter.F90),
`builder.F90`, `facade.F90`, `interface.F90`, `utils.F90`):

| Lesson | Main FM behaviour | Wave analogue (proposed) |
|---|---|---|
| Opt-in | `--precice` sets the facade `is_enabled`; otherwise all calls are no-ops | mdw keyword, default off; facade no-ops when off |
| Builder | configured in `flow_modelinit.F90` after geometry/initial data exist | configure after `grids_and_gridmaps`, before first `get_flow_fields` |
| Polymorphism | abstract `interface.F90` | same, plus a stub implementation for no-preCICE builds |
| Windows | `summed_time_progress` accumulated per user step; exchange and `precicef_advance` only at window end; exceeding window = `mess(LEVEL_ERROR)` | Wave advances one full window per `wave_master_step` |
| Errors | `mess(LEVEL_ERROR, ...)` | same; no `stop` inside readers (the PoC uses `stop` in `precice_read_data`) |
| MPI | `create_with_communicator(DFM_COMM_DFMWORLD)` when `jampi==1` | participant on Wave master rank only |
| Weakness to avoid | config path hardcoded (TODO in builder) | config path is a keyword |

## 3. Design principles (proposed)

1. **Opt-in, default off.** Legacy com-file, DelftIO, stand-alone and FM-BMI/DIMR modes execute
   exactly the same code as on main. New code sits behind `if (precice_active)` guards.
2. **No signature changes** to `wave_main_*`, `wave_bmi` or `swan_tot`; adapter state is
   module-level (facade), as in FM.
3. **Replace endpoints, not the pipeline.** Reuse `grids_and_gridmaps` (ESMF/`grmap`),
   `swan_tot`, the nested-grid loop, and the SWAN computation.
4. **No dual scheduling claim.** The first target is two independent processes
   (`dflowfm` + `wave`), as in the PoC's `run_precice.sh`. Coupling under DIMR is a
   separate, later investigation.

## 4. Activation and build gating (proposed)

* mdw: `[General] Coupler = precice`, `PreciceConfigFile = <path>` (names illustrative).
* CMake option, e.g. `WAVE_WITH_PRECICE`, defining `HAVE_PRECICE`. Off: a stub module compiles
  and any request for the coupler stops with a clear error. Note main FM already requires
  preCICE at configure time (`src/cmake/CMakeLists.txt:68`), so the Wave option may default to
  the FM state, but the legacy Wave code path must compile without it.

## 5. Data flow reusing the main route (proposed)

**Option A (recommended): exchange on the FM mesh, map in Wave with ESMF.**
preCICE uses matching vertex identity (same vertex set and order; concrete mapping scheme to be verified, see below) between the FM and Wave participants on the FM mesh
(`fm_flow_cells`, matching the main adapter). Wave's read endpoint fills the arrays that
`get_flow_fields` fills today (bed `dps`, `s1`, `u1/v1`, wind, vegetation) on the FM flow grid,
after which the existing ESMF mapping and nested-grid logic run. The write endpoint
takes the arrays that `put_wave_fields`/`wave2com` hold. Open point resolved conservatively for v1 (see below), with preCICE peer-mesh access as a later alternative.

**v1 default (proposed): existing static geometry-only file.** Wave reads the FM flow-grid geometry
(the netcdf grid already named by the mdw flow-grid keyword, read by `read_netcdf_grd`) once at
`grids_and_gridmaps`. There is no polling of, and no writing to, any legacy communication file
at run time; the file is used for geometry only. Required validation before the first exchange
(failure = `mess` error, no coupling started):

* the Wave-side vertex/cell **count** equals the FM participant's registered mesh size;
* **topology** (cell-to-node connectivity) and **vertex order** match the FM `fm_flow_cells`
  registration order, checked via a shared hash of coordinates and connectivity exchanged at
  initialisation, or via **global IDs** supplied by FM;
* in MPI-partitioned FM runs, Wave uses the global-ID ordering, not partition-file order.

Evidence limit: whether the FM main adapter currently exposes such IDs/hash is not verified
here; Phase 0 must confirm it. **Later alternative:** use preCICE peer mesh access
(`receive-mesh`/`api-access`), at the cost of changing the Wave grid-construction order
(`read_netcdf_grd` input becomes an in-memory mesh) and of verifying API support in the vendored
preCICE version.

**Mapping terminology:** "identity mapping" is not a preCICE mapping scheme. Option A means
that both participants use the *same vertex set and ordering* so that no spatial interpolation is
needed; the concrete preCICE configuration for that (e.g. a consistent/nearest-neighbour mapping
or a single shared mesh with direct vertex-to-vertex access) must be chosen and verified in
Phase 0 and validated by a round-trip field test.

**Option B (later, optional, single-grid only):** preCICE maps directly to SWAN nodes as in the
PoC (`linear-cell-interpolation`). It cannot serve multiple/nested grids without per-grid meshes.

## 6. Data contract (proposed; must be fixed in a table beside the code)

| Quantity | Direction | Sign/units to define and test | Actual PoC reference |
|---|---|---|---|
| bed level | FM→Wave | positive down (`-bl`), m | FM `unstruc_api.F90` write routines |
| water level `s1` | FM→Wave | m above reference | same |
| velocity | FM→Wave | Eulerian, m/s, 2D only; 3D = explicit error | `getucxucyeulmag`, 3D → `err` |
| wind | FM→Wave | m/s, averaged from links; direction convention to state | same |
| vegetation | FM→Wave | reduce per-grid only when mdw requests; PoC scalar max is not acceptable for variable fields | `get_flow_fields.F90` 92-105 |
| `hwavcom`, `twavcom`, `phiwav`, `sxwav/sywav`, `sbxwav/sbywav`, `mxwav/mywav`, `uorbwav`, `dsurf`, `dwcap` | Wave→FM | same units/conventions as the legacy com-file writes; direction: decide `pdir` vs legacy `dir` explicitly (PoC sends `pdir` into `phiwav`) | FM `set_all_wave_parameters` |

Forcing targets: FM must keep the legacy `ecGetValues` path when the coupler is off; the
preCICE path writes the same FM arrays (the PoC replaces the legacy path, which must not be copied).

## 7. Cross-cutting rules (proposed)

* **Ownership:** Wave master rank owns the participant and all preCICE calls; slaves never call
  preCICE. Finalise exactly once, guarded (PoC finalises unconditionally).
* **Failure:** missing config, vertex-count or mask mismatch, 3D FM, window overrun, unsupported
  preCICE feature → `mess`/error return and orderly SWAN-worker release
  (`wave_mpi_bcast(SWAN_DONE)`), never an unguarded `stop`.
* **Scheduling:** serial-explicit, `first=wave`, `second=fm`, as in the example config. Wave
  uses `precicef_get_max_time_step_size`; window length is a config concern validated against
  the mdw coupling interval.
* **Rollback:** unsupported at first. Detect checkpoint requirements (action queries; exact
  Fortran binding names to be confirmed against the vendored 3.x API) and abort with a clear
  message. Implicit coupling is a later phase.
* **MPI:** create participant after `initialize_wave_mpi`; FM side keeps
  `DFM_COMM_DFMWORLD` and owned cells only (ghost handling to verify).
* **Mesh/mapping:** single mask routine shared by read/write; no debug-file output; explicit
  handling of multiple and nested SWAN grids via Option A (Wave-side ESMF).
* **Initial data:** use `precicef_requires_initial_data`; config `initialize=` must be consistent
  with participant order; Wave must hold valid FM fields before its first SWAN run.
* **SWAN reuse:** `swan_tot`, input generation and output mapping unchanged.

## 8. Phases (proposed)

0. Spike: geometry source, stub build, confirm Fortran binding names.
1. Facade/builder/stub + mdw keywords; coupler off ⇒ bit-identical legacy runs.
2. Read endpoint (FM→Wave) with FM main adapter extended for flow quantities.
3. Write endpoint (Wave→FM) and FM forcing application; serial-explicit smoke test.
4. Nested/multi-grid and MPI Wave runs.
5. Optional Option B; later DIMR scheduling investigation.

## 9. Regression acceptance tests (proposed)

* Legacy: stand-alone, Delft3D4 flow-wave, FM com-file, and FM BMI/DIMR runs identical to
  baseline binaries (bit-identical output where deterministic).
* Build: no-preCICE build compiles; requesting the coupler errors clearly.
* Coupled: serial-explicit FM↔Wave case within tolerance of the com-file route on wave
  height, set-up and water level.
* Failure: missing config, wrong vertex count, 3D FM, window mismatch each give a defined error.
* Later: nested grids, MPI Wave (>1 rank), restart behaviour.
