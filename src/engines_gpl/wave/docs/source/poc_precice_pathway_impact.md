# Impact of the PoC preCICE edits on the main D-Waves pathways

**Scope: which existing main-branch D-Waves pathways would change if the PoC D-Waves edits were merged as they are.**
This page compares main (`269dd353c9`, 2026-10-28) with the PoC (`0f9c6de2c5`). The PoC description is on
[poc_precice_detailed_pathway.md](poc_precice_detailed_pathway.md); the main pathways are on
[main_wave_fm_pathway_review.md](main_wave_fm_pathway_review.md) and the three `D-Waves_pathway_*.md` pages.
The proposal that avoids these impacts is [precice_coupling_design_proposal.md](precice_coupling_design_proposal.md).

## How coupling edits were separated from drift

Main and PoC diverged at `ad3905a49f`. Two different questions need two different diffs:

- **PoC-side change** (what the PoC authors did): `git diff ad3905a49f 0f9c6de2c5 -- <path>`. This touched 32 `src` files (+1090/-801).
  Only this diff is attributed to coupling below.
- **Main-side drift** (what main did in the meantime, unrelated to preCICE): `git diff ad3905a49f HEAD -- <path>`.
  For Wave it includes the boundary spectral cache, `swan_input.f90`, His/map writers, `nautical_convention`/`north_direction`
  arguments and `veg_from_flow` in `swan_tot.f90`, `real(hp)` time handling in `wave_main.f90`, and `put_wave_fields.f90` (-41 lines).
  None of this is a coupling change, but it overlaps the PoC edit in the same files, so a direct merge conflicts.

Main has no preCICE code in `src/engines_gpl/wave` (verified by search). Main FM has its own preCICE adapter; see the proposal.

Classification used in the table:

- **unaffected**: code and behaviour identical.
- **guard bypass**: PoC code runs for this pathway although the pathway does not use preCICE; there is no condition that skips it.
- **changed path**: the pathway executes different code than on main.
- **removed**: main functionality has no equivalent in the PoC.

## Pathway table

Main links are to main source; line numbers are main lines. PoC lines are in the detailed page.

| # | Main pathway | Main entry points | PoC effect | Class | Coupling edit or drift |
|---|---|---|---|---|---|
| 1 | Stand-alone `wave.exe` (mode 0) | [wave_exe.f90](../../packages/wave/src/wave_exe.f90):98-111 `wave_main_init`, one `wave_main_step`, `wave_main_finish` | `initialize_fm_coupling` first and a coupling-driven loop; no config means `precicef_create` fails before the model starts. `wave_main_finish` always finalizes preCICE. | guard bypass and changed path | Coupling. |
| 2 | Stand-alone step logic | [wave_main.f90](../../packages/manager/src/wave_main.f90):368 `wave_main_step`, 405 `wave_master_step` | Extra `precice_state` argument in the signature used by all callers; `swan_tot` called with it. | changed path | Coupling (signature). |
| 3 | Delft3D4 online (DelftIO handshake, blank `flowgridfile`) | `wave_main.f90`:194, 222 `flow_init`; 440, 448 `wave_from_flow_command` | Handshake code is retained, but the entry point (`wave_exe`) is replaced by the preCICE loop and `n_flow_grids` is gone from `grids_and_gridmaps`. Not examined at run time. | changed path | Coupling. |
| 4 | FM com-file read (`flowgridfile` set, FM grid, ESMF) | [swan_tot.f90](../../packages/manager/src/swan_tot.f90):161-163 flow-grid loop calling `get_flow_fields` | The loop and `get_flow_fields` NetCDF read plus ESMF mapping are replaced by `precice_read_data`. | removed | Coupling. |
| 5 | Wave to FM com-file write-back | `swan_tot.f90`:416-421 `map_swan_output`; 500-519 `wave2flow`, `wave2com`, `put_wave_fields`; 523-534 `comcount` | Deleted; replaced by `write_swan_data_to_precice` for `i_swan == 1` only. | removed | Coupling. |
| 6 | `WriteCOM`, `AppendCOM`, `COMFile` mdw keywords | `swan_input.f90`, `put_wave_fields.f90` | The keywords are still parsed; `COMFile` must stay non-blank to avoid the DelftIO path in `wave_init` (line 182/192 test). Nothing is written. | changed path | Coupling; `put_wave_fields.f90` also shows main drift. |
| 7 | FM grid and flow-to-SWAN mapping setup | [grids_and_gridmaps.f90](../../packages/kernel/src/grids_and_gridmaps.f90):1-181 (`n_flow_grids`, map construction lines 130-167) | About 75 lines removed; subroutine signature changed. | removed | Coupling. |
| 7a | Flow-grid containers and flow mode init | `packages/data/src/swan_flow_grid_maps.f90`, `flow_data.f90` (PoC-base diff `ad3905a49f..0f9c6de2c5`, 13 and 12 lines) | `swan_flow_grid_maps`: removes `flow_grids`, `flow2swan_maps`, `swan2flow_maps`, `flow_output_fields`; `init_grids(n_swan_grids, n_flow_grids)` becomes `init_grids(n_swan_grids)`; removes the `read_netcdf_grd(...)` call (flow-link-connectivity netcdf grid read) in `alloc_and_get_grid`. `flow_data`: removes `flow_data_initialized`; `num_subdomains = checkcomfiles(...)` becomes the constant `1`; the legacy stand-alone com-file `get_params(tscale, ...)` time-setting read is removed. | changed path / removed | Coupling (com-file and flow-to-SWAN map removal). Any mode needing flow grids or multiple partitions/com-file subdomains loses its data structures; `grids_and_gridmaps` (row 7) is the caller. Other edits in these files are not seen in this diff (no main drift in the touched hunks). |
| 8 | Nested and multi-grid SWAN | `swan_tot.f90` per-`i_swan` loop; `wave_main.f90`:241 | Only grid 1 is registered, read and written. The first-grid mdw parser is used. | changed path | Coupling. |
| 9 | SwanMode `lib` (DIMR BMI) | [wave_bmi.f90](../../packages/wave/src/wave_bmi.f90):146 `update`, 175 `finalize` | `update` creates a local `precice_state` and calls the new signature; `wave_main_finish` calls `precicef_finalize` without `precicef_create` ever running in that path. | guard bypass and changed path | Coupling. |
| 10 | MPI workers (`run_swan_slave`) | `wave_main.f90`:396 `run_swan_slave`; 538 `wave_mpi_bcast(SWAN_DONE)` | Worker loop unchanged in logic. Wave participant is created with rank 0, size 1 before MPI init; `precicef_finalize` runs on all ranks. | guard bypass | Coupling. |
| 11 | Vegetation from FM | `swan_tot.f90` vegetation branches | Reads preCICE `vegetation_*`, reduces diameter and height to a scalar maximum. Main's `veg_from_flow` (drift) is absent in the PoC. | changed path | Coupling plus drift. |
| 12 | Wind, water level, bed level, current inputs | `get_flow_fields` | Same SWAN fields (`windu/v`, `s1`, `dps`, `u1/v1`) filled from preCICE. FM currents are Eulerian cell-centre values mapped linearly by preCICE rather than by the ESMF `grmap` route. | changed path | Coupling. |
| 13 | SWAN run, hot-start and file staging | `run_swan.f90`, `run_swan_slave.f90`, `read_swan_output.f90` | Unchanged by the PoC. | unaffected | Drift only. |
| 14 | Wave map/his/spectrum NetCDF output | `write_wave_map*.f90`, `write_wave_his_netcdf.f90`, `swan_tot.f90` writer calls | Unchanged by the PoC. | unaffected | Drift only (new arguments on main). |
| 15 | Boundary spectra, `swan_input.f90` generation | boundary spectral cache, `swan_input.f90` | Not present in the PoC base. A merge must keep the main version. | unaffected by PoC | Drift only. |
| 16 | Build (`wave_exe.cmake`, `wave_dll.cmake`, kernel CMake) | `src/engines_gpl/wave/packages/*` | Unconditional link of `precice_fortran`; vendored preCICE sources. Conflicts with main's Conan package and `precicef` target. | changed path | Coupling plus drift. |

## FM-side overlap that affects Wave users

| Main FM item | PoC effect | Class |
|---|---|---|
| `flowinit`, `flowstep`, `flowfinalize` in [unstruc_api.F90](../../../dflowfm/packages/dflowfm_kernel/src/dflowfm_manager/unstruc_api.F90) | Main already calls its adapter through `precice_adapter_is_enabled` (lines 343, 440). The PoC adds `precice_state` arguments and unconditional calls. | changed path; direct conflict |
| Legacy wave forcing `ecGetValues` in `set_all_wave_parameters` | Replaced unconditionally by `read_precice_wave_data`. FM-with-SWAN online runs lose the com-file input. | removed |
| `wricom`, `ComInterval`, `ti_com` | Removed (commit `5c5114c34a`), including the time-vector support in `unstruc_model.f90` and `m_flowtimes.f90`. | removed |
| `unstruc_bmi.F90` `initialize` | Dummy state is passed to `flowinit`, which creates a participant. | guard bypass |
| `getucxucyeulmag`, `getucmag`, `getucxucyeuler` | Signature and bounds changes (`N` argument, assumed-shape output arrays). Used for preCICE, but these are shared compute routines. | changed path |
| `unstruc_netcdf.f90` COM time-dimension append | Appends instead of overwriting at `itim`; touches the map-file writer used beyond COM. | changed path |

## Summary

- **Unaffected by the PoC:** SWAN execution and staging, NetCDF map/his output, boundary spectra.
- **Guard bypass:** `wave_main_finish`, the BMI update path, MPI ranks, the stand-alone exe: all run preCICE code with no activation check.
- **Removed:** the entire online com-file route (read from FM, ESMF mapping, write-back), which is every main pathway
  in [D-Waves_pathway_fm_bmi.md](D-Waves_pathway_fm_bmi.md) and the FM-online part of the Delft3D4 route. Merging the PoC as is would
  replace them rather than add an alternative.
- Run-time checks of the impacts above were not performed.
