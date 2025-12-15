# Dummy Scalar Exchange Implementation Summary

## Overview
Implementation of preCICE latency testing infrastructure for measuring performance when transferring many scalar values between Delft3D-FM and SWAN/Wave models.

**Date**: 2024  
**Branch**: all/poc/UNST-9189_Scalar_exchange_via_preCICE  
**Purpose**: Isolate and measure preCICE communication overhead independent of physics computations

---

## Implementation Details

### FM Kernel Modifications

**File**: `src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_io/fm_precice_state_t.F90`

Added to `fm_precice_state_t` type:
```fortran
character(kind=c_char, len=13) :: dummy_mesh_name = "fm_dummy_mesh"
integer(kind=c_int), dimension(:), allocatable :: dummy_vertex_ids
integer :: num_dummy_scalars = 0
character(kind=c_char, len=20), dimension(:), allocatable :: dummy_scalar_names
```

**File**: `src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_manager/unstruc_api.F90`

Added functions:
1. **`register_dummy_mesh_with_precice(precice_state, num_scalars)`**
   - Registers single vertex at coordinates (0.0, 0.0)
   - Generates field names: `fm_scalar_0000001` through `fm_scalar_NNNNNNN`
   - Allocates `dummy_vertex_ids(1)` and `dummy_scalar_names(num_scalars)`
   - Called from `initialize_precice_coupling()` with `num_scalars = 100`

2. **`precice_write_dummy_scalars(precice_state)`**
   - Loops through all dummy scalars
   - Generates random value 0-1: `call random_number(rand_val)`
   - Writes to preCICE: `call precicef_write_data(dummy_mesh_name, scalar_name, ...)`
   - Prints progress every 10 scalars
   - Called from `precice_write_data()` after vegetation data write

### Wave Kernel Modifications

**File**: `src/engines_gpl/wave/packages/io/src/wave_precice_state_t.F90`

Added to `wave_precice_state_t` type (mirrors FM):
```fortran
character(kind=c_char, len=15) :: dummy_mesh_name = "wave_dummy_mesh"
integer(kind=c_int), dimension(:), allocatable :: dummy_vertex_ids
integer :: num_dummy_scalars = 0
character(kind=c_char, len=20), dimension(:), allocatable :: dummy_scalar_names
```

**File**: `src/engines_gpl/wave/packages/manager/src/wave_main.F90`

Added function:
- **`register_dummy_mesh_wave(precice_state, num_scalars)`**
  - Registers single vertex at (0.0, 0.0) matching FM location
  - Creates field names matching FM: `fm_scalar_0000001` etc.
  - Allocates arrays for vertex IDs and scalar names
  - Called from `initialize_fm_coupling()` with `num_scalars = 100`

**File**: `src/engines_gpl/wave/packages/manager/src/swan_tot.F90`

Added function:
- **`read_dummy_scalars_from_precice(precice_state)`**
  - Loops through all dummy scalars
  - Reads from preCICE: `call precicef_read_data(dummy_mesh_name, scalar_name, ...)`
  - Computes running sum for statistics
  - Calculates and prints average value
  - Called from `swan_tot` after `write_swan_data_to_precice()`

---

## Configuration Files

### Location
`/workspaces/Delft3D/examples/precice_scalar_latency_test/`

### Files Created

1. **`generate_precice_config.py`** (Python script, 135 lines)
   - Generates preCICE XML configuration with variable scalar count
   - Usage: `python3 generate_precice_config.py [num_scalars]`
   - Default: 100 scalars
   - Creates complete configuration with:
     - Data declarations for all scalars
     - Mesh definitions (fm_dummy_mesh, wave_dummy_mesh)
     - Participant configurations (FM writes, Wave reads)
     - Nearest-neighbor mapping
     - Serial explicit coupling scheme

2. **`precice-config.xml`** (Generated XML, 646 lines)
   - Configuration for 100 scalar fields
   - FM participant: provides fm_dummy_mesh, writes 100 scalars
   - Wave participant: receives fm_dummy_mesh, reads 100 scalars
   - Communication: TCP sockets via `<m2n:sockets>`
   - Mapping: nearest-neighbor (exact match for single point)

3. **`README.md`** (Documentation, comprehensive)
   - Purpose and architecture explanation
   - Setup and execution instructions
   - Performance analysis guidelines
   - Troubleshooting tips
   - Future enhancement suggestions

---

## Data Flow

```
FM Kernel                                 Wave Kernel
─────────                                 ────────────
┌─────────────────────────┐              ┌─────────────────────────┐
│ initialize_precice_     │              │ initialize_fm_          │
│ coupling()              │              │ coupling()              │
│  ↓                      │              │  ↓                      │
│ register_dummy_mesh()   │              │ register_dummy_mesh()   │
│  • 1 vertex at (0,0)    │              │  • 1 vertex at (0,0)    │
│  • 100 scalar names     │              │  • 100 scalar names     │
└─────────────────────────┘              └─────────────────────────┘

Time Loop:
┌─────────────────────────┐              ┌─────────────────────────┐
│ precice_write_data()    │              │ swan_tot()              │
│  ↓                      │   preCICE    │  ↓                      │
│ write_dummy_scalars()   │─────────────→│ read_dummy_scalars()    │
│  • Generate random 0-1  │   Exchange   │  • Read all scalars     │
│  • Write 100 values     │              │  • Compute average      │
│  • Print progress       │              │  • Print statistics     │
└─────────────────────────┘              └─────────────────────────┘
```

---

## Expected Console Output

### FM Console
```
[FM] Registered dummy mesh with 1 vertex at (0,0)
[FM] Created 100 dummy scalar field names
[FM] Starting preCICE coupling...
...
[FM] Wrote 10 dummy scalars, last value: 0.3421
[FM] Wrote 20 dummy scalars, last value: 0.8734
...
[FM] Wrote 100 dummy scalars, last value: 0.5612
```

### Wave Console
```
[Wave] Registered dummy mesh with 1 vertex at (0,0)
[Wave] Created 100 dummy scalar field names for reading
[Wave] Starting preCICE coupling...
...
[Wave] Read 100 dummy scalars, average value: 0.5089
```

### Validation
- Average value should be approximately 0.5 (uniform random 0-1 distribution)
- Significant deviation indicates data transfer issues
- Print statements confirm successful registration and data exchange

---

## Performance Testing Workflow

### 1. Compile with preCICE Support
```bash
cd /workspaces/Delft3D
./build_install_fm_suite_unix.sh -DENABLE_PRECICE=ON
```

### 2. Generate Configuration
```bash
cd examples/precice_scalar_latency_test

# For 100 scalars (default)
python3 generate_precice_config.py

# For custom count
python3 generate_precice_config.py 500
```

### 3. Run Coupled Simulation
Terminal 1 (FM):
```bash
export PRECICE_CONFIG=/workspaces/Delft3D/examples/precice_scalar_latency_test/precice-config.xml
/path/to/install/bin/dflowfm --autostartstop your_model.mdu
```

Terminal 2 (Wave):
```bash
export PRECICE_CONFIG=/workspaces/Delft3D/examples/precice_scalar_latency_test/precice-config.xml
/path/to/install/bin/wave your_model.mdw
```

### 4. Analyze Results
- Monitor console output for timing information
- Check preCICE logs for performance warnings
- Compare latency across different scalar counts
- Identify scaling characteristics

---

## Scaling Test Matrix

| Scalar Count | Use Case | Expected Latency |
|--------------|----------|------------------|
| 10 | Minimal baseline | Low |
| 50 | Small model | Low-Medium |
| 100 | Standard model | Medium |
| 500 | Large model | Medium-High |
| 1000 | Stress test | High |

To test different counts:
1. Modify hardcoded value in `unstruc_api.F90` and `wave_main.F90`
2. Recompile both kernels
3. Generate new preCICE configuration: `python3 generate_precice_config.py [count]`
4. Run test and measure performance

---

## Future Enhancements

### Code Improvements
1. **Parameterized Scalar Count**: Read from configuration file instead of hardcoding
   - Add parameter to MDU/MDW files
   - Parse at runtime and allocate dynamically

2. **Built-in Timing**: Add CPU/wall-clock timing around preCICE calls
   ```fortran
   real(kind=8) :: start_time, end_time
   call cpu_time(start_time)
   call precice_write_dummy_scalars(precice_state)
   call cpu_time(end_time)
   print *, '[FM] Latency:', (end_time - start_time) * 1000.0, 'ms'
   ```

3. **Random Seed Control**: Initialize for reproducibility
   ```fortran
   integer :: seed_size, i
   integer, allocatable :: seed_values(:)
   call random_seed(size=seed_size)
   allocate(seed_values(seed_size))
   seed_values = 12345  ! Fixed seed
   call random_seed(put=seed_values)
   ```

### Testing Extensions
1. **Multi-point Meshes**: Test with 2×2, 10×10 grids
2. **Vector Fields**: Compare scalar vs vector performance
3. **Communication Backends**: Test TCP vs MPI vs shared memory
4. **Mapping Methods**: Compare nearest-neighbor vs RBF

### Automation
1. **Benchmark Script**: Automated testing across scalar counts
2. **Result Visualization**: Plot latency vs scalar count
3. **Regression Testing**: Track performance over code changes

---

## Technical Notes

### Mesh Configuration
- **Single Point Design**: Eliminates geometric complexity, isolates communication
- **Matching Coordinates**: FM and Wave both use (0,0) for exact mapping
- **Nearest-Neighbor Mapping**: Most efficient for single-point case

### Data Integrity
- **Random Values**: Uniform distribution [0,1] for statistical verification
- **Average Check**: Expected ~0.5, indicates successful transfer
- **Per-scalar Validation**: Could extend to checksum or hash verification

### preCICE Settings
- **Serial Explicit Coupling**: Simplest scheme, sequential execution
- **TCP Sockets**: Default communication, can change to MPI for performance
- **Time Window**: 1.0 seconds, configurable
- **Max Time**: 100 seconds, adjust for longer tests

---

## Known Limitations

1. **Hardcoded Scalar Count**: Currently fixed at 100 in source code
   - Requires recompilation to change
   - Should be moved to configuration file

2. **No Performance Instrumentation**: Manual timing required
   - Add built-in profiling in future
   - Integration with preCICE profiling tools

3. **Single Mesh Point**: Not representative of real coupling
   - Useful for overhead measurement only
   - Extend to realistic mesh sizes for application studies

4. **No Error Handling**: Assumes successful preCICE operations
   - Add checks for allocation failures
   - Validate preCICE return codes

---

## References

- **preCICE Documentation**: https://precice.org/docs.html
- **Performance Guide**: https://precice.org/configuration-acceleration.html
- **Profiling Tools**: https://precice.org/tooling-performance-analysis.html
- **Branch**: `all/poc/UNST-9189_Scalar_exchange_via_preCICE`
- **Related Files**:
  - FM: `unstruc_api.F90`, `fm_precice_state_t.F90`
  - Wave: `wave_main.F90`, `swan_tot.F90`, `wave_precice_state_t.F90`
