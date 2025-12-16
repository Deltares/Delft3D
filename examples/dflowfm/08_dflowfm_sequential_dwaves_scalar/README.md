# preCICE Scalar Latency Testing for Delft3D FM-SWAN Coupling# preCICE Scalar Latency Testing for Delft3D FM-SWAN Coupling# preCICE Scalar Latency Testing



This example extends the `08_dflowfm_sequential_dwaves` test case with dummy scalar exchange infrastructure for measuring preCICE communication latency.



## PurposeThis example extends the `08_dflowfm_sequential_dwaves` test case with dummy scalar exchange infrastructure for measuring preCICE communication latency.This directory contains infrastructure for measuring preCICE latency when transferring many scalar values between Delft3D-FM and SWAN/Wave models.



Measure preCICE performance overhead when transferring many scalar values by:



- Using minimal dummy meshes (single point at origin)## Purpose## Purpose

- Transferring only scalar values (no complex physics)

- Supporting configurable scalar counts for scaling tests

- Providing statistical verification of data integrity

Measure preCICE performance overhead when transferring many scalar values by:The dummy scalar exchange system isolates preCICE communication overhead by:

## Quick Start

- Using minimal dummy meshes (single point at origin)- Using minimal meshes (single point at origin)

```bash

cd /workspaces/Delft3D/examples/dflowfm/08_dflowfm_sequential_dwaves_scalar- Transferring only scalar values (no complex physics)  - Transferring only scalar values (no complex physics)



# 1. Add 10 dummy scalars to configuration (matches hardcoded value)- Supporting runtime-configurable scalar counts for scaling tests- Supporting configurable scalar counts for scaling tests

python3 add_dummy_scalars.py 10

- Providing statistical verification of data integrity- Providing statistical verification of data integrity

# 2. Set environment variable

source ~/.precice_env



# 3. Run the coupled model## Quick Start## Implementation

./run_precice.sh

```



## Implementation```bash### FM Side (Data Provider)



### FM Side (Data Provider)cd /workspaces/Delft3D/examples/dflowfm/08_dflowfm_sequential_dwaves_scalar- **Mesh**: `fm_dummy_mesh` with 1 vertex at (0.0, 0.0)



- **Mesh**: `fm_dummy_mesh` with 1 vertex at (0.0, 0.0)- **Operation**: Writes N scalars with random values (0-1)

- **Operation**: Writes N scalars with fixed values (scalar index = value)

- **Fields**: `fm_scalar_0000001` through `fm_scalar_NNNNNNN`# 1. Add 100 dummy scalars to configuration- **Fields**: `fm_scalar_0000001` through `fm_scalar_NNNNNNN`

- **Code**: `src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_manager/unstruc_api.F90`

  - `register_dummy_mesh_with_precice()` - mesh registrationpython3 add_dummy_scalars.py 100- **Code**: `src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_manager/unstruc_api.F90`

  - `precice_write_dummy_scalars()` - value writing

  - `register_dummy_mesh_with_precice()` - mesh registration

### Wave Side (Data Receiver)

# 2. Set environment variable  - `precice_write_dummy_scalars()` - random value generation and writing

- **Mesh**: `wave_dummy_mesh` with 1 vertex at (0.0, 0.0)

- **Operation**: Reads N scalars from preCICEsource ~/.precice_env

- **Fields**: Same as FM, read from `fm_dummy_mesh`

- **Code**: ### Wave Side (Data Receiver)

  - `src/engines_gpl/wave/packages/manager/src/wave_main.F90`: `register_dummy_mesh_wave()`

  - `src/engines_gpl/wave/packages/io/src/get_flow_fields.F90`: `read_dummy_scalars_from_precice()`# 3. Run the coupled model- **Mesh**: `wave_dummy_mesh` with 1 vertex at (0.0, 0.0)



## Configuration Generationcp ../08_dflowfm_sequential_dwaves/run_precice.sh .- **Operation**: Reads N scalars and computes statistics



Use `add_dummy_scalars.py` to augment the base configuration with dummy scalars:./run_precice.sh- **Fields**: Same as FM, read from `fm_dummy_mesh`



```bash```- **Code**: 

# Generate config for 10 scalars (matches hardcoded default)

python3 add_dummy_scalars.py 10  - `src/engines_gpl/wave/packages/manager/src/wave_main.F90`: `register_dummy_mesh_wave()`



# Generate config for custom scalar count## How It Works  - `src/engines_gpl/wave/packages/manager/src/swan_tot.F90`: `read_dummy_scalars_from_precice()`

python3 add_dummy_scalars.py 50



# Generate config for 100 scalars

python3 add_dummy_scalars.py 100### Configuration Script## Configuration Generation



# Disable dummy scalars (production mode)

python3 add_dummy_scalars.py 0

````add_dummy_scalars.py` modifies the base preCICE configuration from `../08_dflowfm_sequential_dwaves/precice_config.xml` by adding:Use the Python script to generate preCICE configuration with desired scalar count:



This script:

- Reads from `precice_config_template.xml` (base FM-SWAN configuration with max-time=90000)

- Adds dummy scalar declarations, meshes, and exchanges1. Dummy scalar declarations (`fm_scalar_0000001`, etc.)```bash

- Writes to `precice_config.xml`

- Removes VTK export to reduce I/O overhead2. Dummy meshes (`fm_dummy_mesh`, `wave_dummy_mesh`)# Generate config for 100 scalars (default)

- Sets `PRECICE_NUM_DUMMY_SCALARS` environment variable

3. FM write operations for all dummy scalarspython3 generate_precice_config.py

## Configuration Structure

4. Wave read operations for all dummy scalars

The script modifies the base configuration by adding:

5. Exchanges in the coupling scheme# Generate config for custom scalar count

1. **Dummy scalar declarations** (`fm_scalar_0000001`, etc.)

2. **Dummy meshes** (`fm_dummy_mesh`, `wave_dummy_mesh`)python3 generate_precice_config.py 500

3. **FM write operations** for all dummy scalars

4. **Wave read operations** for all dummy scalars### Runtime Behavior

5. **Exchanges** in the coupling scheme

6. **Nearest-neighbor mapping** from `fm_dummy_mesh` to `wave_dummy_mesh`# Generate config for 1000 scalars



## Hardcoded Scalar Count**FM**: Generates random values (0-1) and writes to preCICE  python3 generate_precice_config.py 1000



The number of scalars is **hardcoded** in the source files:**Wave**: Reads values and computes average (~0.5 expected)```



- **FM**: `src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_io/fm_precice_state_t.F90`

  ```fortran

  integer :: num_dummy_scalars = 10### Environment VariableThis creates `precice-config.xml` with:

  ```

- All scalar data declarations

- **Wave**: `src/engines_gpl/wave/packages/io/src/wave_precice_state_t.F90`

  ```fortran`PRECICE_NUM_DUMMY_SCALARS` controls the number at runtime (0-10000):- FM and Wave mesh definitions

  integer :: num_dummy_scalars = 10

  ```- Default: 100- Participant configurations (write/read data)



**Important**: The preCICE config must match the hardcoded value:- Set to 0 to disable for production runs- Nearest-neighbor mapping

- If code says `num_dummy_scalars = 10`, run `python3 add_dummy_scalars.py 10`

- After changing the hardcoded value, rebuild: `./build_install_fm_suite_unix.sh --mode precice --build-type debug`- Must match the value used in `add_dummy_scalars.py`- Serial explicit coupling scheme



## Runtime Behavior



**FM Process**:## Usage Examples## Runtime Configuration

1. Registers `fm_dummy_mesh` with 1 vertex at (0,0)

2. Creates N scalar field names

3. Each timestep: Writes all scalars (value = scalar index)

4. Logs: `[FM] Wrote N dummy scalars, last value: N.0`**Test with 500 scalars:**The number of dummy scalars can be configured at runtime using the `PRECICE_NUM_DUMMY_SCALARS` environment variable:



**Wave Process**:```bash

1. Registers `wave_dummy_mesh` with 1 vertex at (0,0)

2. Creates N scalar field names for readingpython3 add_dummy_scalars.py 500```bash

3. Each timestep: Reads all scalars from preCICE

4. No explicit logging (values used for latency measurement only)source ~/.precice_env# Use default (100 scalars)



## Directory Structure./run_precice.shexport PRECICE_NUM_DUMMY_SCALARS=100



``````

08_dflowfm_sequential_dwaves_scalar/

├── add_dummy_scalars.py          # Config augmentation script (ACTIVE)# Test with 500 scalars

├── precice_config_template.xml   # Base FM-SWAN config (max-time=90000)

├── precice_config.xml            # Generated config (augmented from template)**Scaling study:**export PRECICE_NUM_DUMMY_SCALARS=500

├── run_precice.sh                # Test execution script

├── dflowfm/                      # FM model files```bash

├── dwaves/                       # SWAN model files

└── README.md                     # This filefor N in 10 50 100 500 1000; do# Disable dummy mesh (for production runs)

```

    python3 add_dummy_scalars.py $Nexport PRECICE_NUM_DUMMY_SCALARS=0

**Note**: `generate_precice_config.py` (if present) is deprecated and not used.

    source ~/.precice_env```

## Testing Different Scalar Counts

    ./run_precice.sh

To test with different numbers of scalars:

done**Important**: The value must match the number of scalars in your generated `precice-config.xml`.

1. **Update hardcoded values** in both FM and Wave source files

2. **Rebuild** the code```

3. **Regenerate config** to match

4. **Run test**Features:



Example for 50 scalars:**Disable for production:**- **Default**: 100 scalars if not set



```bash```bash- **Range**: 0 to 10000 (enforced with warnings)

# 1. Edit source files - change num_dummy_scalars = 10 to 50

vim src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_io/fm_precice_state_t.F90export PRECICE_NUM_DUMMY_SCALARS=0- **Disabled**: Set to 0 to skip dummy mesh registration

vim src/engines_gpl/wave/packages/io/src/wave_precice_state_t.F90

```- **Validation**: Automatic bounds checking and error handling

# 2. Rebuild

cd /workspaces/Delft3D

./build_install_fm_suite_unix.sh --mode precice --build-type debug

## Expected Output## Running Tests

# 3. Regenerate config

cd examples/dflowfm/08_dflowfm_sequential_dwaves_scalar

python3 add_dummy_scalars.py 50

source ~/.precice_env**FM**: `[FM] Using PRECICE_NUM_DUMMY_SCALARS= 100 from environment`  ### Prerequisites



# 4. Run test**FM**: `[FM] Wrote 100 dummy scalars, last value: 0.XXXX`1. Build Delft3D with preCICE support:

./run_precice.sh

```   ```bash



## Verification**Wave**: `[Wave] Using PRECICE_NUM_DUMMY_SCALARS= 100 from environment`     cd /workspaces/Delft3D



Check log files for confirmation:**Wave**: `[Wave] Read 100 dummy scalars, average value: ~0.5`   ./build_install_fm_suite_unix.sh -DENABLE_PRECICE=ON



```bash   ```

# FM log - should show N scalars written

grep "dummy" dflowfm_*.log## Troubleshooting



# Wave log - should show N scalars read2. Generate preCICE configuration:

grep "dummy" wave_*.log

| Issue | Solution |   ```bash

# preCICE logs - check mapping and data exchange

grep "fm_scalar" *.log|-------|----------|   cd examples/precice_scalar_latency_test

```

| "Data not defined" error | Regenerate config: `python3 add_dummy_scalars.py $PRECICE_NUM_DUMMY_SCALARS` |   python3 generate_precice_config.py 100

Expected output:

```| Average not ~0.5 | Check mesh registration (both at 0,0) |   ```

[FM] Registered dummy mesh with 1 vertex at (0,0)

[FM] Created N dummy scalar field names| Dummy scalars not running | Set and source environment variable |

[FM] Wrote N dummy scalars, last value: N.0

3. Ensure preCICE library is available:

[Wave] Registered dummy mesh with 1 vertex at (0,0)

[Wave] Created N dummy scalar field names for reading## Files   ```bash

```

   export LD_LIBRARY_PATH=/path/to/precice/lib:$LD_LIBRARY_PATH

## Performance Considerations

- `add_dummy_scalars.py` - Configuration augmentation script   ```

### Debug vs Release Build

- `precice_config.xml` - Modified configuration (generated)

- **Debug build**: Higher memory overhead, slower execution

  - Works well with ~10 scalars- `QUICK_REFERENCE.md` - One-page quick start### Execution

  - May crash with 2+ scalars due to preCICE memory overhead

  - `IMPLEMENTATION_SUMMARY.md` - Technical detailsRun FM and Wave in separate terminals with preCICE configuration:

- **Release build**: Lower memory overhead, faster execution

  - Can handle many more scalars

  - Build with: `./build_install_fm_suite_unix.sh --mode precice --build-type release`

**Base case**: `../08_dflowfm_sequential_dwaves/`**Terminal 1 (FM):**

### Memory Overhead

```bash

preCICE allocates internal structures per field. On a 1-vertex mesh:

- Each scalar adds ~100KB+ overhead in debug mode## Referencescd examples/precice_scalar_latency_test

- Overhead is lower in release mode



### Scaling Tests

- preCICE docs: https://precice.org/docs.html# Set number of dummy scalars (must match XML config)

For latency testing with many scalars:

1. Start with release build- Performance: https://precice.org/configuration-acceleration.htmlexport PRECICE_NUM_DUMMY_SCALARS=100

2. Test progressively: 10 → 50 → 100 → 500 → 1000

3. Monitor memory usage and execution timeexport PRECICE_CONFIG=precice-config.xml

4. Log files will grow proportionally with scalar count

/path/to/dflowfm --autostartstop your_fm_model.mdu

## Troubleshooting```



| Issue | Solution |**Terminal 2 (Wave):**

|-------|----------|```bash

| Compilation errors | Check `num_dummy_scalars` matches in both FM and Wave files |cd examples/precice_scalar_latency_test

| "Data not defined" error | Regenerate config: `python3 add_dummy_scalars.py N` (N = hardcoded value) |

| Config/code mismatch | Ensure preCICE config scalar count matches hardcoded value |# Set same number of dummy scalars

| Memory errors (debug) | Try release build or reduce scalar count |export PRECICE_NUM_DUMMY_SCALARS=100

| Crashes with 2+ scalars (debug) | Known issue - use release build or keep N=1 |export PRECICE_CONFIG=precice-config.xml



## Known Limitations/path/to/wave your_wave_input.mdw

```

1. **Debug mode memory**: Debug builds may crash with 2+ scalars due to preCICE's per-field overhead on 1-vertex mesh

2. **Hardcoded values**: Must manually sync code and config (no runtime environment variable support with current approach)### Expected Output

3. **Single vertex only**: Dummy mesh uses 1 point for minimal overhead

**FM Console:**

## Background: Why Hardcoded Approach?```

[FM] PRECICE_NUM_DUMMY_SCALARS not set, using default: 100

Originally used environment variables, but that approach had issues:[FM] Registered dummy mesh with 1 vertex at (0,0)

- Required `allocatable` arrays → initialization problems[FM] Created 100 dummy scalar field names

- Inconsistent array types between FM and Wave...

- Struct initialization errors[FM] Wrote 100 dummy scalars, last value: 0.7234

```

Current hardcoded approach:

- Uses fixed-size arrays `dimension(1)` (not allocatable)Or with environment variable set:

- Consistent between FM and Wave```

- Simpler memory management[FM] Using PRECICE_NUM_DUMMY_SCALARS= 500 from environment

- More reliable, especially in debug mode[FM] Registered dummy mesh with 1 vertex at (0,0)

[FM] Created 500 dummy scalar field names

## References...

```

- Base case: `../08_dflowfm_sequential_dwaves/`

- preCICE documentation: https://precice.org/**Wave Console:**

- Migration notes: `MIGRATION_SUMMARY.md````

- Implementation details: `IMPLEMENTATION_SUMMARY.md`[Wave] PRECICE_NUM_DUMMY_SCALARS not set, using default: 100

[Wave] Registered dummy mesh with 1 vertex at (0,0)
[Wave] Created 100 dummy scalar field names for reading
...
[Wave] Read 100 dummy scalars, average value: 0.5123
```

## Performance Analysis

### Metrics to Collect
1. **Time per coupling iteration** - Total time spent in preCICE operations
2. **Scalar count scaling** - How latency increases with field count
3. **Communication backend** - TCP sockets vs MPI performance
4. **Memory usage** - RAM consumption with many fields

### Instrumentation
Add timing calls around preCICE operations:

```fortran
! In unstruc_api.F90
real(kind=8) :: start_time, end_time
call cpu_time(start_time)
call precice_write_dummy_scalars(precice_state)
call cpu_time(end_time)
print *, '[FM] Dummy scalar write time:', end_time - start_time, 'seconds'
```

### Scaling Studies
Test with various scalar counts to identify overhead:

| Scalars | Expected Use Case | Command |
|---------|------------------|---------|
| 10      | Minimal overhead baseline | `export PRECICE_NUM_DUMMY_SCALARS=10` |
| 50      | Small coupled model | `export PRECICE_NUM_DUMMY_SCALARS=50` |
| 100     | Medium coupled model | `export PRECICE_NUM_DUMMY_SCALARS=100` |
| 500     | Large coupled model | `export PRECICE_NUM_DUMMY_SCALARS=500` |
| 1000    | Stress test | `export PRECICE_NUM_DUMMY_SCALARS=1000` |

**Workflow for scaling studies:**
```bash
# Generate configuration for desired scalar count
python3 generate_precice_config.py 500

# Set environment variable to match
export PRECICE_NUM_DUMMY_SCALARS=500
export PRECICE_CONFIG=precice-config.xml

# Run FM and Wave
# ... measure performance ...
```

## Configuration Parameters

### Runtime Parameters (Environment Variables)
- **PRECICE_NUM_DUMMY_SCALARS**: Number of dummy scalars to exchange
  - Default: 100
  - Range: 0-10000 (enforced)
  - Set to 0 to disable dummy mesh (for production runs)
  - Must match the value used in `generate_precice_config.py`

### Code Parameters (Fixed at Compile Time)
- **Mesh location**: Currently (0,0), can be changed in `dummy_coords` array in source code
- **Random seed**: Currently unseeded, can initialize with `call random_seed()`

### preCICE Configuration (XML)
- **Time window size**: Currently 1.0, adjust `<time-window-size>`
- **Max simulation time**: Currently 100.0, adjust `<max-time>`
- **Communication method**: Currently TCP sockets, can change to `<m2n:mpi>`
- **Mapping method**: Currently nearest-neighbor, can use `rbf` for testing

## Troubleshooting

### No data transfer
- Check mesh names match in code and XML
- Verify scalar field names are identical (FM writes, Wave reads)
- Ensure both participants use same preCICE config file
- **Verify PRECICE_NUM_DUMMY_SCALARS matches XML config**

### Mismatch in scalar count
**Symptom**: preCICE errors about undefined data fields

**Cause**: Environment variable doesn't match generated XML

**Solution**:
```bash
# Regenerate XML to match environment variable
export PRECICE_NUM_DUMMY_SCALARS=500
python3 generate_precice_config.py $PRECICE_NUM_DUMMY_SCALARS
```

### Incorrect values
- Average should be ~0.5 for uniform random 0-1 distribution
- Large deviations indicate mapping or data errors
- Check preCICE logs for warnings

### Performance issues
- Enable preCICE profiling: add `--precice-profiling` flag
- Check if using TCP on same machine (use MPI instead)
- Verify no debug symbols in production builds

## Future Enhancements

1. ~~**Parameterized Scalar Count**: Read from configuration file instead of hardcoding~~ ✅ **DONE** - Use `PRECICE_NUM_DUMMY_SCALARS` environment variable
2. **Timing Instrumentation**: Built-in performance measurement
3. **Multiple Points**: Test with 2D/3D mesh grids
4. **Vector Fields**: Compare scalar vs vector performance
5. **Automated Benchmarking**: Script to run multiple configurations and plot results

## References

- Main preCICE documentation: https://precice.org/docs.html
- Performance optimization: https://precice.org/configuration-acceleration.html
- Profiling guide: https://precice.org/tooling-performance-analysis.html
