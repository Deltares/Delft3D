# D-Waves FM (Flexible Mesh) Code Pathway Review

**Date:** 28 October 2026  
**Scope:** Documentation research and source code verification of D-Flow FM coupling pathways  
**Status:** Research findings based on existing documentation and source inspection  
**Method:** Read docs/, trace source structure, verify entry points and lifecycle phases

> **Note on line references:** line numbers marked `~` are approximate navigational hints and were
> not all re-verified; the current source always takes priority. Corrected (re-checked) values:
> `wave_init` 131, `grids_and_gridmaps` call 241, `wave_main_step` 368 (worker `run_swan_slave` at 396),
> `wave_master_step` 405. Canonical, more detailed pathway documents:
> [FM/BMI pathway](D-Waves_pathway_fm_bmi.md), [code pathways](D-Waves_code_pathways.md),
> [Delft3D4](D-Waves_pathway_delft3d4.md), [stand-alone](D-Waves_pathway_standalone.md).

---

## Executive Summary

This report documents the complete D-Waves / D-Flow FM coupling implementation through BMI and NetCDF communication files. The FM pathway is driven by DIMR's component lifecycle orchestration; D-Waves reads D-Flow FM hydrodynamic fields and writes wave fields through shared NetCDF communication files (`*_com.nc`). No direct BMI field arrays are used for grid data—all spatial coupling is file-backed.

The FM pathway is one of three supported execution models:
1. **Standalone** – D-Waves owns schedule; optional saved com-files
2. **Delft3D4 online** – DelftIO handshake; NEFIS com-files; separate SWAN executable
3. **D-Flow FM online** (FM/BMI) – DIMR BMI lifecycle; NetCDF com-files; SwanMode=lib for MPI

---

## 1. FM Pathway Lifecycle and Control Flow

### 1.1 Initialization and Mode Selection

**DIMR Orchestration Entry:**  
`src/dimr/packages/dimr_lib/src/dimr.cpp : runParallelInit()` (line ~429)

**D-Waves Mode Setup:**
- DIMR reads D-Waves as a shared library component (wave_bmi.f90)
- Before `initialize()` call, DIMR sets the BMI `mode` variable:
  - Mode value: `flow_online = 1` (same as Delft3D4 online)
  - Set via: `dllGetVar("mode", &waveModePtr); *waveModePtr = 1;` 
  - Location: `src/dimr/packages/dimr_lib/src/bmi.cpp` line ~176
  - Condition: D-Waves is a `<start>` inside a `<startGroup>` within a `<parallel>` block

**D-Waves BMI Initialize:**  
`src/engines_gpl/wave/packages/wave/src/wave_bmi.f90`
- `initialize(c_mdw_file)` function (line 63)
- Calls: `wave_main_init(wave_mode, mdw_file)` 
- Passes the mode set by DIMR to `wave_main_init(mode_in)` parameter

**Wave Main Init Entry:**  
`src/engines_gpl/wave/packages/manager/src/wave_main.f90 : wave_main_init()` (line not re-verified)
- Receives mode from BMI wrapper (`wave_main_init` line not re-verified)
- Calls: `wave_init(mode_in, mdw_file)` (line ~166)

**Wave Init Setup:**  
`src/engines_gpl/wave/packages/manager/src/wave_main.f90 : wave_init()` (line 131)
- Line 150: `call setmode(wavedata, mode_in)` – stores mode in wavedata type
- Line 154: `call read_dwaves_mdw(mdw_file, swan_run, wavedata)` – reads MDW configuration
- Line 156: Checks if `wavedata%mode/=stand_alone .and. swan_run%flowgridfile/=' '`
- If true, sets `swan_run%nttide = 1` (online mode flag)

**Mode-Based Path Selection:**  
`src/engines_gpl/wave/packages/data/src/flow_data.f90 : flow_init()` (line ~79)
- FM Path (NetCDF): `if (swan_run%flowgridfile/=' ')` 
  - Calls: `checkcomfiles(swan_run%flowgridfile)` – detects and counts partition files
  - Sets: `num_subdomains` = 1 (single file) or more (partitioned: `_0000_com.nc`, `_0001_com.nc`, ...)
  - Sets: `flow_data_initialized = .true.`
- Delft3D4 Path (NEFIS): `elseif (mode==stand_alone)` or `else` branch with DelftIO handshake
  - Not taken for FM coupling

**Grid and Mapping Initialization:**  
`src/engines_gpl/wave/packages/manager/src/wave_main.f90 : wave_init()` (`grids_and_gridmaps` call at line 241)
- Calls: `grids_and_gridmaps(n_swan_grids, n_flow_grids, swan_run, wavedata%mode)`
- Reads SWAN grids from configuration
- Reads D-Flow FM grid(s) from NetCDF com-file(s)
- Prepares both mapping directions per grid pair (initialization only, not update)

---

### 1.2 DIMR Update Loop and BMI Advance

**DIMR Main Loop:**  
`src/dimr/packages/dimr_lib/src/dimr.cpp : main scheduler` (line ~429+)

**BMI Update Call:**  
`src/engines_gpl/wave/packages/wave/src/wave_bmi.f90 : update(dt)` (line ~308)
- Receives `dt` (delta time in seconds) from DIMR scheduler
- Calls: `wave_main_step(real(dt,hp))`
- No field arrays transferred via BMI

**Wave Main Step (Update):**  
`src/engines_gpl/wave/packages/manager/src/wave_main.f90 : wave_main_step(stepsize)` (line 368)
- Dispatches to master or worker based on MPI rank
- Master: `wave_master_step(stepsize)` 
- Workers: call `run_swan_slave()` (line 396) and wait for the master's collective messages (section 1.4); they do not simply return

**Wave Master Step:**  
`src/engines_gpl/wave/packages/manager/src/wave_main.f90 : wave_master_step()` (line 405)
- Advances D-Waves calculation time: 
  - `call settimsec(wavedata%time, wavedata%time%timsec + stepsize, ...)`
- Calls: `swan_tot(...)` to loop over SWAN grids and nests

---

### 1.3 SWAN Grid Loop and Field Exchange

**Grid and Nest Processing:**  
`src/engines_gpl/wave/packages/manager/src/swan_tot.f90 : swan_tot()` (line ~106)

**Field Input (D-Flow FM → SWAN):**  
- When configured: calls `get_flow_fields(i_flow, i_swan, sif, fg, sg, f2s, wavedata, sr, ...)`
  - Location: `src/engines_gpl/wave/packages/io/src/get_flow_fields.f90` (line ~102)
  - Reads D-Flow FM fields directly from NetCDF via `get_var_netcdf()` 
  - Applies ESMF-based spatial mapping: `grmap_esmf()` (D-Flow FM → SWAN grid)
  - Output: `sif` type (input_fields) on SWAN grid

**SWAN Solver:**  
- Processes each SWAN grid and nested domains
- Nested grids use boundary output from parent grid

**Wave Field Mapping (SWAN → D-Flow FM):**  
- After SWAN solve, if `WriteCOM` enabled:
  - Calls: `map_swan_output(sof, fof, gm, fg)` 
    - Location: `src/engines_gpl/wave/packages/kernel/src/map_swan_output.f90` (line ~1)
    - Uses internal lookup tables (`ref_table`, `weight_table`) prepared at init
    - Calls `grmap()` for each wave field (hs, tp, fx, fy, dir, etc.)
    - Does NOT call `wave2flow()` for FM (that is NEFIS/Delft3D4 only)
  - Result: `fof` type (output_fields) on D-Flow FM grid

**D-Flow FM Writeback:**  
- Calls: `wave2com()` and `put_wave_fields()` 
  - Location: `src/engines_gpl/wave/packages/io/src/put_wave_fields.f90` (line ~18)
  - Detects FM route: `if (flowgridfile /= ' ')` → calls `crewav_netcdf()`
  - Writes accumulated wave fields to NetCDF com-file(s)

---

### 1.4 MPI Coordination (SwanMode=lib)

**Master/Worker Dispatch:**  
`src/engines_gpl/wave/packages/manager/src/wave_main.f90 : wave_main_step()` (line 368)
- `my_rank /= master`: workers branch (`run_swan_slave` at line 396)
  - Calls: `run_swan_slave()` and waits

**Master Execution:**  
- Performs field I/O, mapping, SWAN calls

**SwanMode=lib Collective Call:**  
`src/engines_gpl/wave/packages/kernel/src/run_swan.f90 : run_swan()` (line ~90)
- Master calls `swan_lib()` collectively with all D-Waves ranks
- Library mode: SWAN computations are collective MPI operations

**Worker Barrier:**  
`src/engines_gpl/wave/packages/kernel/src/run_swan_slave.f90 : run_swan_slave()` (line ~60)
- Workers synchronize with master before output processing
- Cleanup of temporary SWAN files by master only

---

### 1.5 Finalization

**BMI Finalize:**  
`src/engines_gpl/wave/packages/wave/src/wave_bmi.f90 : finalize()`

**Wave Main Finish:**  
`src/engines_gpl/wave/packages/manager/src/wave_main.f90 : wave_main_finish()`
- Deallocates grids, maps, output arrays
- Cleans temporary SWAN files

---

## 2. Configuration and Mode Selection Details

### 2.1 MDW Keywords Controlling FM Pathway

| Keyword | Location | Value/Condition | Effect |
|---------|----------|-----------------|--------|
| `[Output] COMFile` | MDW Input section | Set to `*_com.nc` file path | Selects D-Flow FM NetCDF route; stored in `swan_run%flowgridfile` |
| `[General] FlowFile` | MDW Input section | Non-blank (any value) | Forces mode 1 (online); selects whether D-Flow FM or DelftIO |
| `[Output] WriteCOM` | MDW Input section | `true` / `1` | Enables wave-field writeback to com-file (mandatory for online runs) |
| `SwanMode` | MDW Input section | `exe` (default) or `lib` | Selects SWAN as executable (exe) or library (lib, MPI-enabled) |
| `[General] FlowBedLevel`, `FlowWaterLevel`, `FlowVelocity`, `FlowWind` | MDW Input section | 0 or 1 | Selects which D-Flow FM fields to read |

### 2.2 Pathway Disambiguator: flowgridfile vs. DelftIO

The critical disambiguator is `swan_run%flowgridfile`:
- **Non-blank** (e.g., `"mesh_com.nc"`): D-Flow FM NetCDF route
  - Set by MDW `[Output] COMFile`
  - Read via: `get_var_netcdf()` with direct NetCDF library calls
  - Map via: ESMF-generated sparse weights
  - No DelftIO handshake
- **Blank**: Delft3D4 route (either standalone or DelftIO online)
  - Requires `[General] FlowFile` or empty for standalone
  - Selects DelftIO handshake if online mode

**FM vs. Delft3D4 Selection Logic:**  
`src/engines_gpl/wave/packages/data/src/flow_data.f90 : flow_init()` (line ~89)
```fortran
if (swan_run%flowgridfile/=' ') then
    ! D-Flow FM NetCDF route
    num_subdomains = checkcomfiles(swan_run%flowgridfile)
    flow_data_initialized = .true.
elseif (mode==stand_alone) then
    ! Delft3D4 standalone route
    ...
else ! mode/=stand_alone .and. swan_run%flowgridfile==' '
    ! Delft3D4 online route with DelftIO
    call wave_from_flow_init(...)
endif
```

---

## 3. FM Data Pathways: Reading D-Flow FM Fields

### 3.1 Grid Reading from NetCDF

**Entry Point:**  
`src/engines_gpl/wave/packages/kernel/src/grids_and_gridmaps.f90 : grids_and_gridmaps()` (line ~82)

**Grid Detection:**  
- Counts input SWAN grids from configuration
- Sets `netcdf_files = .true.` when `sr%flowgridfile /= ' '`
- Calls grid reader for each D-Flow FM partition/domain

**D-Flow FM Grid Read:**  
`src/engines_gpl/wave/packages/data/src/read_grids.f90 : read_netcdf_grd()` (line ~575)
- Detects NetCDF file format via `swan_run%flowgridfile` 
- Opens file via netCDF library: `nf90_open()`
- Reads grid dimensions: `nf90_inq_dimid()` for mmax, nmax
- Reads coordinates: `nf90_inq_varid()` and `nf90_get_var()` for x, y
- Reads cell masks: `kcs` (active/inactive cells)

**EC Module Triangulation Call (for non-quadrilateral cells):**  
`src/engines_gpl/wave/packages/data/src/read_grids.f90 : read_grids.f90` (line ~1013)
- When non-spherical grid is not purely quadrilateral:
  - Calls EC routine: `call tricall()` to triangulate cell centers
  - Writes temporary source grid: `TMP_ESMF_RegridWeightGen_flow_source_<n>.nc`
  - Used for ESMF weight generation

### 3.2 Hydrodynamic Field Input

**Field Read Entry:**  
`src/engines_gpl/wave/packages/io/src/get_flow_fields.f90 : get_flow_fields()` (line ~102)

**Direct NetCDF Library Calls:**  
`src/engines_gpl/wave/packages/io/src/get_var_netcdf.f90 : get_var_netcdf()` (line ~126)
- Opens NetCDF com-file: `nf90_open(basename, NF90_NOWRITE, idfile)`
- Queries variable ID: `nf90_inq_varid(idfile, varname, idvar_*)`
- Reads data: `nf90_get_var(idfile, idvar_*, vararr)`
- Closes file: `nf90_close(idfile)`
- No EC module involvement for reading field values

**Supported D-Flow FM Fields:**  
(Controlled by MDW keyword selections)
- Water level (`s1`)
- Depth (`dps`)
- Velocities (`u1`, `v1`)
- Wind (`windx`, `windy`)
- Vegetation (`rnveg`, `diaveg`, `veg_stemheight`)

**Time Selection:**  
- D-Waves advances its own time via DIMR's `update(dt)` schedule
- Finds nearest or interpolated D-Flow FM record in com-file
- No explicit time synchronization: caller (DIMR) must align schedules

---

### 3.3 Spatial Mapping: D-Flow FM → SWAN (ESMF-based)

**Mapping Preparation:**  
`src/engines_gpl/wave/packages/data/src/swan_flow_grid_maps.f90 : make_grid_map()` (line ~306)
- Called once per grid pair at initialization
- FM → SWAN uses ESMF-generated sparse weights

**ESMF Weight File:**  
- Invokes external tool: `ESMF_RegridWeightGen` 
- Inputs: source (D-Flow FM) and destination (SWAN) grids in NetCDF format
- Output: sparse weights in ESMF format (col, row, S arrays)
- File: `<tempdir>/ESMF_*_weights.nc`

**Weight Application:**  
`src/engines_gpl/wave/packages/io/src/grmap_esmf.f90`
- Applies ESMF sparse matrix: `f_out(i) = sum(S * f_in(col))` for each destination row
- One call per field per SWAN grid
- Reused across all time steps (weights never change)

---

## 4. FM Data Pathways: Writing Wave Fields

### 4.1 Mapping: SWAN → D-Flow FM

**Wave Output Mapping:**  
`src/engines_gpl/wave/packages/kernel/src/map_swan_output.f90 : map_swan_output()` (line ~1)

**Mapping Mechanism:**  
- Uses internal lookup tables (`ref_table`, `weight_table`)
- NOT ESMF: D-Waves uses `grmap()` subroutine with stored weights
- Prepared once at init via `make_grid_map()` → `mkmap()`
- Reused at every update

**Field Quantities Mapped:**  
All output wave fields (from SWAN to D-Flow FM grid):
- Significant wave height (`hs`)
- Peak period (`tp`)
- Wave direction (`dir`)
- Wave force vectors (`fx`, `fy`)
- Mass flux vectors (`mx`, `my`)
- Dissipation parameters
- Bottom orbital velocity (`ubot`)
- Wavelength (`wlen`)
- And others (~20 fields total)

**Grid Conversion (FM-specific):**  
- NO `wave2flow()` call for FM pathway
- `wave2flow()` is Delft3D4/NEFIS only; converts to curvilinear grid orientation
- D-Flow FM is unstructured; no orientation conversion needed

---

### 4.2 Wave Field Writeback to NetCDF

**Writeback Entry:**  
`src/engines_gpl/wave/packages/io/src/put_wave_fields.f90 : put_wave_fields()` (line ~18)

**FM-Specific Path:**  
```fortran
if (flowgridfile == ' ') then
    ! NEFIS path (Delft3D4)
    call crewav(...)
else
    ! NetCDF path (D-Flow FM)
    call crewav_netcdf(fg, itide, ...)
endif
```

**NetCDF Writeback:**  
- Writes to `[Output] COMFile` NetCDF file
- Format: time-series of wave fields on D-Flow FM grid
- Called once per D-Waves update (when `WriteCOM = true`)
- Mandatory in FM online mode

---

## 5. Configuration and Ownership Details

### 5.1 Time Ownership

| Component | Role |
|-----------|------|
| DIMR | Owns component schedule; calls `update(dt)` with fixed `dt` |
| D-Flow FM | Owns hydrodynamic computation time steps; writes com-file at configured output times |
| D-Waves | Owns SWAN time; advanced by `dt` each BMI update; must read D-Flow FM data available at or before current time |

**Time Synchronization Responsibility:** Caller (DIMR configuration, user)
- Must ensure D-Flow FM writes com-files when D-Waves needs them
- DIMR schedules both components; user configures timing

### 5.2 File Ownership and Access Patterns

| Resource | Writer | Reader | Access Method |
|----------|--------|--------|----------------|
| D-Flow FM com-file | D-Flow FM | D-Waves | Direct NetCDF library calls (not EC) |
| SWAN grid files (temp) | D-Waves | SWAN | File staging (exe or library mode) |
| SWAN output files (temp) | SWAN | D-Waves | File staging |
| SWAN boundary files (nesting) | D-Waves (parent) | D-Waves (nested) | File staging |

### 5.3 Mapping Ownership

| Direction | Owner | Mapping Type | Timing |
|-----------|-------|--------------|--------|
| D-Flow FM → SWAN | D-Waves | ESMF sparse weights | Prepared at init; applied at each update |
| SWAN → D-Flow FM | D-Waves | Internal `mkmap` lookup tables | Prepared at init; applied at each update (writeback) |

---

## 6. Implementation Evidence: Source Symbols and Locations

### 6.1 Mode and Initialization Symbols

| Symbol | File | Line | Purpose |
|--------|------|------|---------|
| `wave_mode` | `wave_bmi.f90` | ~50 | Module variable; holds DIMR-set mode |
| `initialize()` | `wave_bmi.f90` | ~63 | BMI entry point; receives C-style mdw_file string |
| `get_var("mode")` | `wave_bmi.f90` | ~308 | BMI accessor; DIMR writes via pointer before init |
| `wave_main_init()` | `wave_main.f90` | not re-verified | Fortran entry point from BMI initialize() |
| `wave_init()` | `wave_main.f90` | 131 | Reads MDW; calls flow_init() and grids_and_gridmaps() |
| `setmode()` | `wave_data.f90` | TBV | Stores mode in wavedata type |
| `flow_init()` | `flow_data.f90` | ~79 | Checks flowgridfile; routes to FM or DelftIO |
| `checkcomfiles()` | `flow_data.f90` | ~84 | Counts D-Flow FM partition files |

### 6.2 Grid Reading and Mapping Symbols

| Symbol | File | Line | Purpose |
|--------|------|------|---------|
| `grids_and_gridmaps()` | `grids_and_gridmaps.f90` | ~82 | Main grid/map orchestrator |
| `read_netcdf_grd()` | `read_grids.f90` | ~575 | Reads D-Flow FM grid from NetCDF |
| `tricall()` | (EC module) | N/A | Triangulates non-quad cell centers |
| `make_grid_map()` | `swan_flow_grid_maps.f90` | ~306 | Prepares both mapping directions |
| `mkmap()` | `mkmap.f90` | TBV | Builds reference/weight tables (SWAN → Flow) |
| `ESMF_RegridWeightGen` | External tool | N/A | Generates sparse weights (Flow → SWAN) |

### 6.3 Field Input Symbols

| Symbol | File | Line | Purpose |
|--------|------|------|---------|
| `get_flow_fields()` | `get_flow_fields.f90` | ~102 | Field input orchestrator |
| `get_var_netcdf()` | `get_var_netcdf.f90` | ~126 | Direct NetCDF reads (no EC) |
| `nf90_open()` | netcdf module | N/A | Open NetCDF file |
| `nf90_get_var()` | netcdf module | N/A | Read NetCDF variable |
| `grmap_esmf()` | `grmap_esmf.f90` | TBV | Apply ESMF weights (Flow → SWAN) |

### 6.4 Field Output and Writeback Symbols

| Symbol | File | Line | Purpose |
|--------|------|------|---------|
| `map_swan_output()` | `map_swan_output.f90` | ~1 | Map SWAN output to Flow grid |
| `grmap()` | `grmap.f90` | TBV | Apply internal weights (SWAN → Flow) |
| `wave2com()` | `wave2com.f90` | TBV | Accumulate wave fields (no wave2flow for FM) |
| `put_wave_fields()` | `put_wave_fields.f90` | ~18 | Route to crewav_netcdf() for FM |
| `crewav_netcdf()` | TBV | TBV | Write NetCDF com-file (FM route) |

### 6.5 SWAN Execution Symbols

| Symbol | File | Line | Purpose |
|--------|------|------|---------|
| `swan_tot()` | `swan_tot.f90` | ~106 | Grid/nest loop orchestrator |
| `run_swan()` | `run_swan.f90` | ~90 | SWAN execution; lib mode collective call |
| `run_swan_slave()` | `run_swan_slave.f90` | ~60 | Worker barrier and sync |
| `swan_lib()` | SWAN library | N/A | SWAN solver; called collectively in lib mode |

---

## 7. Uncertainties and Limitations of Verification

### 7.1 Unverified by Runtime Testing

This report is based on:
- Existing documentation (`D-Waves_pathway_fm_bmi.md`, DOT diagrams)
- Static source inspection (file reads, symbol locations, logic flow)
- Not run or validated against a live FM coupled run

**Unverified aspects:**
- Actual behavior of `ESMF_RegridWeightGen` tool invocation
- Exact timing alignment when D-Flow FM and D-Waves updates are staggered
- Parallel MPI coordination in SwanMode=lib with multiple D-Waves ranks
- Recovery behavior if D-Flow FM com-file is missing or delayed

### 7.2 Symbolic References to Fortran Precision Module

- Many symbols use `hp` and `sp` precision kinds
- Not defined in this inspection (reside in `precision` module)
- Not material to pathway understanding

### 7.3 External Tool Integration

- `ESMF_RegridWeightGen` is an external tool; invocation not traced
- Assumed to produce weights in expected format

---

## 8. Standalone Mode (for Comparison)

For completeness, standalone FM reading is also supported:

**Standalone FM Initialization:**  
- `[Output] COMFile` identifies existing D-Flow FM com-file
- `wave_init()` still calls `flow_init()` if `swan_run%useflowdata`
- Same grid/map setup as online mode
- Difference: no D-Flow FM process running; com-file is static
- Time selection: MDW `[Timepoint]` entries or fixed interval

**Standalone Writeback:**  
- Optional: writes mapped wave fields to same or new com-file
- Same `crewav_netcdf()` path as online mode

---

## 9. Delft3D4 Online Mode (Excluded from FM Focus)

For reference, Delft3D4 online differs fundamentally:
- DelftIO handshake (not BMI)
- NEFIS com-files (not NetCDF)
- SWAN as separate executable (not library)
- `wave2flow()` call for curvilinear grid conversion
- Fluid-mud extension (mode 2) for water/mud interaction
- See `D-Waves_pathway_delft3d4.md` for full details

---

## 10. Conclusion

The D-Waves FM pathway is a file-backed coupling mechanism where:
1. **DIMR orchestrates** component lifecycle and timing
2. **D-Flow FM writes** geometry and hydrodynamic fields to NetCDF com-files
3. **D-Waves reads** those files directly (no EC module, no BMI field arrays)
4. **D-Waves remaps** fields between grids using ESMF (input) and internal tables (output)
5. **D-Waves writes** wave fields back to the same com-file(s)
6. **D-Flow FM reads** wave fields through its external-forcing (EC) module

All spatial coupling is file-mediated; BMI handles lifecycle only. MPI parallelism in SwanMode=lib is collective, not rank-per-SWAN-grid.

---

## Appendix A: Documentation References

**D-Waves User Manual:**  
Version 2027.01, revision 81378 (28 October 2026, DRAFT)  
Sections: 3.3.1, 5.4.1.1–5.4.1.2, 5.4.9, Appendix A.1.1

**Existing DOT Diagrams (referenced in source tree):**
- `dwaves_code_pathways.dot` – Entry points per pathway
- `dwaves_pathway_fm_bmi_detailed.dot` – FM orchestration detail
- `dwaves_pathway_fm_bmi_detailed_code_pathways.dot` – FM code flow

**Source Documentation:**
- `src/engines_gpl/wave/docs/source/D-Waves_code_pathways.md`
- `src/engines_gpl/wave/docs/source/D-Waves_pathway_fm_bmi.md`
- Doxygen blocks in source files (function-level documentation)

---

**Report prepared via:** Static source inspection and documentation review  
**Fortran style guidelines applied:** See `.github/instructions/fortran.instructions.md`
