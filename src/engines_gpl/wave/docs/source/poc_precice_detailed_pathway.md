# PoC D-Flow FM / D-Waves preCICE coupling: traced pathway

**Status of this page: description of the proof-of-concept (PoC) source as it is, read from source only.**
Nothing here was run. The code shows a FM-to-Wave and a Wave-to-FM data path in
each program, but no document or test in either checkout demonstrates a completed
bidirectional run. The proposal for the main branch is a separate page,
[precice_coupling_design_proposal.md](precice_coupling_design_proposal.md).

\dotfile poc_precice_detailed_pathway.dot "PoC FM / D-Waves preCICE pathway"

[DOT source](../include/poc_precice_detailed_pathway.dot) |
[SVG preview](../include/poc_precice_detailed_pathway.svg)

## Provenance

| Item | Value |
|---|---|
| PoC checkout (read-only) | `C:\checkouts\Delft3D_PoC`, HEAD `0f9c6de2c5` (2026-01-28, a merge) |
| Merge base with this branch | `ad3905a49f` (2026-01-27) |
| Main HEAD when written | `269dd353c9` (2026-10-28) |
| Last commit touching the FM and Wave adapter code | `5c5114c34a` (2025-11-19, "Remove com file from FM side") |

The implementation age is the adapter commit date (about a year), not the merge date.
PoC-side changes were isolated with `git diff ad3905a49f 0f9c6de2c5 -- <path>`, which excludes
everything that main changed afterwards. Line numbers below are PoC lines at `0f9c6de2c5`.
Links point at the sibling checkout and only work on a machine that has it at
`C:\checkouts\Delft3D_PoC`.

## Build gating (none)

- `dwaves_configuration.cmake` and `dflowfm_configuration_basic.cmake` call
  `add_subdirectory(${precice_module} precice_fortran)` when the target does not yet exist;
  `third_parties.cmake` sets `precice_module` to `src/third_party_open/precice`.
- `wave_manager` and `dflowfm_kernel` link `precice_fortran` PRIVATE with no option.
  `unstruc_api.F90` gets the preprocessor flag (`dflowfm_kernel/CMakeLists.txt`).
- The tree contains a vendored preCICE (3.3.0, modified, with Windows binaries and a
  `readme_deltares.txt`). Main uses a different supply route: a Conan `precice/3.4.1` package,
  `find_package(precice CONFIG REQUIRED)` and a `precicef` target. The two are not interchangeable.
- There is no `#ifdef`, option or runtime flag. A PoC build always needs preCICE, and both programs
  always call `precicef_*`.

## Wave side lifecycle

| Step | Symbol and location | What it does |
|---|---|---|
| 1 | [wave_exe.F90][p-exe]:104 `initialize_fm_coupling` | Calls [wave_main.F90][p-wm]:67-82: `precicef_create("wave", "../precice_config.xml", my_rank, numranks, ...)`, registers vertices, then `precicef_initialize`. The path is a hard-coded constant (line 77). `my_rank`/`numranks` keep their module defaults (0/1) because `initialize_wave_mpi` runs later, inside `wave_main_init` (line 371). |
| 2 | `register_wave_nodes_with_precice` (103), `get_swan_grid` (195) | Parses the mdw by hand and uses only the first `Grid=` of the first domain, then `read_grd`. Vertices are points with `kcs == 1` (lines 140, 279-301). A triangle pair is added per quad when all four corners have `kcs == 1` (161-164). Errors are printed and the routine returns. `get_swan_grid` also writes a debug file `wave_coordinates_debug.txt`. |
| 3 | [wave_exe.F90][p-exe]:106-110 | `wave_main_init`, `wave_main_step(0.0_hp)`, then `advance_fm_time_window`. |
| 4 | [wave_exe.F90][p-exe]:112-117 | `do while (is_fm_coupling_ongoing())`: step size is `precicef_get_max_time_step_size`, `wave_main_step`, `advance_fm_time_window`. This replaces the legacy single `wave_main_step(-1.0)` call unconditionally. |
| 5 | `wave_master_step` ([wave_main.F90][p-wm]:646), `settimsec` at 710 | For a mode other than stand-alone with a non-blank `flowgridfile`, time advances by `stepsize` and `swan_tot` is called once (lines 715, 734, 741, 768). With a blank `flowgridfile` the DelftIO handshake is still present. |
| 6 | `swan_tot` ([swan_tot.F90][p-st]:277-280) | `get_flow_fields` runs only when `swan_run%useflowdata`. |
| 7 | `precice_read_data` ([get_flow_fields.F90][p-gff]:109-153) | Reads with relative time 0.0 and scatters into every cell with `kcs /= 0` (line 144). `stop` on a data-dimension mismatch (134). |
| 8 | `swan_tot` 523-525 | `if (i_swan == 1)` calls `write_swan_data_to_precice` (53), which calls `transform_wave_physics_sp` and writes about 38 fields, on `kcs == 1` points only (line 32). |
| 9 | `wave_main_finish` ([wave_main.F90][p-wm]:789) | `precicef_finalize` at 812, unconditional and on every rank. |

Other changes in the same PoC edit that affect all modes: `wave_main_step` and `wave_master_step` gained a
`precice_state` argument; `swan_tot` and `get_flow_fields` lost their `n_flow_grids`/flow-grid arguments;
`grids_and_gridmaps` lost `n_flow_grids`. The files were renamed from `.f90` to `.F90`.
The details per pathway are in [poc_precice_pathway_impact.md](poc_precice_pathway_impact.md).

## FM side lifecycle

| Step | Symbol and location | What it does |
|---|---|---|
| 1 | [unstruc_api.F90][p-ua]:564 `flowinit(precice_state)`; call at 658 | Creates the preCICE participant at the end of model initialisation. |
| 2 | `initialize_precice_coupling` (84) | `precicef_create` when `jampi == 0`, else `precicef_create_with_communicator(DFM_COMM_DFMWORLD)` (lines 100-105). Config path hard-coded at line 96. |
| 3 | `register_flow_nodes_with_precice` (115) | Vertices `xz/yz(1:ndxi)` on mesh `fm_flow_nodes`; triangles from the C routine `tricall` only when preCICE asks for connectivity (147). A triangulation error is printed (171). |
| 4 | lines 108-112 | `precicef_requires_initial_data`, then `precice_write_data(bl, s1)` and `precicef_initialize`. |
| 5 | `flowstep` (661), call at 692 | After `flow_usertimestep`, `advance_precice_time_window(dt_user, ...)` (195): if coupling is ongoing, write data, then `precicef_advance(dt_user)`. |
| 6 | [fm_external_forcings_update.F90][p-fu]:590 `set_all_wave_parameters` | Replaces the legacy `ecGetValues` wave inputs by `read_precice_wave_data` (571), with relative read time `precicef_get_max_time_step_size`. Each read runs only if the target array is allocated. It prints a line at every call (602). |
| 7 | `flowfinalize` (729) | `finalize_precice_coupling` at 742, unconditional. |

FM state: `fm_precice_state_t` ([fm_precice_state_t.F90][p-fs]) holds participant `fm`, mesh `fm_flow_nodes` and all
data names. A module-global instance `global_fm_precice_state` is used by `flow()` ([unstruc_api.F90][p-ua]:523-531)
and by the forcing update.

## Data contract as implemented

Units and signs follow the code; the preCICE config adds no unit conversion.

| Name | Direction | Writer location | Reader and target | Notes |
|---|---|---|---|---|
| `bed_levels` | FM to Wave | `precice_write_bed_levels` (234) writes `-bl` | `get_flow_fields.F90`:71 into `sif%dps` if `qextnd(q_bath) > 0` | Positive down (matches the Wave depth convention). |
| `water_levels` | FM to Wave | `s1` (255-263) | line 75 into `sif%s1` | m. |
| `flow_velocity` (vector) | FM to Wave | `getucxucyeulmag(..., 0, 0)` (291) | line 79 into `sif%u1`, `sif%v1` | 3D FM stops with `err` (285-286). Eulerian, not Stokes-corrected by this call alone (not verified). |
| `wind_velocity` (vector) | FM to Wave | per-node average of link `wx`, `wy` (298-326); skipped if `jawind == 0` | line 83 into `windu`, `windv` | |
| `vegetation_*` | FM to Wave | 331-353; skipped if `javeg == 0` | lines 87-89; diameter and height reduced to a scalar maximum in `sr%veg_diamtr`, `sr%veg_height` (92-105) | Loses spatial variation. Local variable named `maxval` shadows the intrinsic. |
| `hrms`, `tp`, `pdir`, `fx`, `fy`, `wsbodyu/v`, `mx`, `my`, `ubot`, `dissip2`, `dissip3` | Wave to FM | `write_swan_data_to_precice` | `set_all_wave_parameters` 604-615 into `hwavcom`, `twavcom`, `phiwav`, `sxwav`, `sywav`, `sbxwav`, `sbywav`, `mxwav`, `mywav`, `uorbwav`, `dsurf`, `dwcap` | `pdir` fills `phiwav`; the legacy path read `item_dir`. |
| about 24 further names (`hs`, `dir`, `period`, `depth`, `dissip1/4`, `steep`, `wlen`, `rtp`, `tm02`, ...) | Wave to preCICE only | same writer | not read by FM | Defined in [wave_precice_state_t.F90][p-ws]:16-53. |

The data is exchanged in the `kcs`-masked ordering of the SWAN grid on the Wave side and in cell-centre order
(`1:ndxi`) on the FM side. The mapping between the two is delegated to preCICE.

## Scheduling and time

The config at [precice_config.xml][p-cfg] uses a serial-explicit scheme with `first=wave`, `second=fm` and a
fixed 3600 s window. Wave computes the window using the FM data exchanged at the previous advance;
FM then advances with the wave data from the same exchange. The FM→Wave exchanges carry
`initialize="true"`. There is no implicit coupling, no rollback and no sub-cycling check in the code.
FM calls `precicef_advance(dt_user)` every user step, not only at the end of a window; preCICE tolerates this
for a window of several user steps, but the code never checks that `dt_user` divides the window.

## Defects and gaps found in the source

| Item | Evidence |
|---|---|
| Wave always reports rank 0 / size 1 | `initialize_fm_coupling` before `initialize_wave_mpi` (wave_exe.F90:104 vs wave_main.F90:371). With SwanMode MPI and more than one rank every rank would still try its own `precicef_create`, but the code path for that was not examined. |
| Only the first grid is used | `get_swan_grid` first `Grid=`; `swan_tot.F90`:523 `i_swan == 1`; `precice_read_data` uses the same `vertex_ids` for every `i_swan`. Example 12 has two domains. |
| Registration/read mask mismatch | Registration and write use `kcs == 1`; read scatters on `kcs /= 0`. Any `kcs == 2` cell would shift all following values (inferred; not run). |
| BMI/DIMR route is not wired | [wave_bmi.f90][p-bmi]:144-148 declares a local `precice_state` that nothing creates, yet `wave_main_finish` calls `precicef_finalize`. [unstruc_bmi.F90][p-ub]:250 passes a dummy state to `flowinit` (327), and `flowinit` creates a participant. |
| Legacy write-back deleted | `swan_tot` no longer calls `map_swan_output`, `wave2flow`, `wave2com`, `put_wave_fields`. `get_flow_fields` no longer reads NetCDF or calls ESMF mapping. |
| Config path | `"../precice_config.xml"` hard-coded in both programs. |
| FM in parallel | Vertices use `1:ndxi`; whether ghost cells are excluded was not verified. |
| Console noise | `[FM] We are in set_all_wave_parameters` printed at every call. |

## What is NOT established

- A complete FM and Wave run in both directions. The example `08_dflowfm_sequential_dwaves` has a config,
  a mdw and a launcher script, but this review ran nothing, and no expected-results data was found.
- DIMR scheduling of both programs with preCICE. `dimr_config.xml` in that example lists a wave start group,
  but the launcher starts two standalone processes; the PoC BMI code paths cannot do this (see above).
- MPI Wave with preCICE, nested domains, 3D FM, sediment/morphology interaction.
- Example 12 (`12_dflowfm_waves_sinterklaas`): its README calls it temporary and its input data is on a network drive.
- Equivalence of results with the com-file route.

[p-wm]: ../../../../../../Delft3D_PoC/src/engines_gpl/wave/packages/manager/src/wave_main.F90
[p-st]: ../../../../../../Delft3D_PoC/src/engines_gpl/wave/packages/manager/src/swan_tot.F90
[p-gff]: ../../../../../../Delft3D_PoC/src/engines_gpl/wave/packages/io/src/get_flow_fields.F90
[p-ws]: ../../../../../../Delft3D_PoC/src/engines_gpl/wave/packages/io/src/wave_precice_state_t.F90
[p-exe]: ../../../../../../Delft3D_PoC/src/engines_gpl/wave/packages/wave/src/wave_exe.F90
[p-bmi]: ../../../../../../Delft3D_PoC/src/engines_gpl/wave/packages/wave/src/wave_bmi.f90
[p-ua]: ../../../../../../Delft3D_PoC/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_manager/unstruc_api.F90
[p-fs]: ../../../../../../Delft3D_PoC/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_io/fm_precice_state_t.F90
[p-fu]: ../../../../../../Delft3D_PoC/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/timespace/fm_external_forcings_update.F90
[p-ub]: ../../../../../../Delft3D_PoC/src/engines_gpl/dflowfm/packages/dflowfm_lib/src/unstruc_bmi.F90
[p-cfg]: ../../../../../../Delft3D_PoC/examples/dflowfm/08_dflowfm_sequential_dwaves/precice_config.xml
