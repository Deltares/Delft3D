# preCICE Scalar Latency Testing for Delft3D FM-SWAN Coupling# preCICE Scalar Latency Testing



This example extends the `08_dflowfm_sequential_dwaves` test case with dummy scalar exchange infrastructure for measuring preCICE communication latency.This directory contains infrastructure for measuring preCICE latency when transferring many scalar values between Delft3D-FM and SWAN/Wave models.



## Purpose## Purpose



Measure preCICE performance overhead when transferring many scalar values by:The dummy scalar exchange system isolates preCICE communication overhead by:

- Using minimal dummy meshes (single point at origin)- Using minimal meshes (single point at origin)

- Transferring only scalar values (no complex physics)  - Transferring only scalar values (no complex physics)

- Supporting runtime-configurable scalar counts for scaling tests- Supporting configurable scalar counts for scaling tests

- Providing statistical verification of data integrity- Providing statistical verification of data integrity



## Quick Start## Implementation



```bash### FM Side (Data Provider)

cd /workspaces/Delft3D/examples/dflowfm/08_dflowfm_sequential_dwaves_scalar- **Mesh**: `fm_dummy_mesh` with 1 vertex at (0.0, 0.0)

- **Operation**: Writes N scalars with random values (0-1)

# 1. Add 100 dummy scalars to configuration- **Fields**: `fm_scalar_0000001` through `fm_scalar_NNNNNNN`

python3 add_dummy_scalars.py 100- **Code**: `src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_manager/unstruc_api.F90`

  - `register_dummy_mesh_with_precice()` - mesh registration

# 2. Set environment variable  - `precice_write_dummy_scalars()` - random value generation and writing

source ~/.precice_env

### Wave Side (Data Receiver)

# 3. Run the coupled model- **Mesh**: `wave_dummy_mesh` with 1 vertex at (0.0, 0.0)

cp ../08_dflowfm_sequential_dwaves/run_precice.sh .- **Operation**: Reads N scalars and computes statistics

./run_precice.sh- **Fields**: Same as FM, read from `fm_dummy_mesh`

```- **Code**: 

  - `src/engines_gpl/wave/packages/manager/src/wave_main.F90`: `register_dummy_mesh_wave()`

## How It Works  - `src/engines_gpl/wave/packages/manager/src/swan_tot.F90`: `read_dummy_scalars_from_precice()`



### Configuration Script## Configuration Generation



`add_dummy_scalars.py` modifies the base preCICE configuration from `../08_dflowfm_sequential_dwaves/precice_config.xml` by adding:Use the Python script to generate preCICE configuration with desired scalar count:



1. Dummy scalar declarations (`fm_scalar_0000001`, etc.)```bash

2. Dummy meshes (`fm_dummy_mesh`, `wave_dummy_mesh`)# Generate config for 100 scalars (default)

3. FM write operations for all dummy scalarspython3 generate_precice_config.py

4. Wave read operations for all dummy scalars

5. Exchanges in the coupling scheme# Generate config for custom scalar count

python3 generate_precice_config.py 500

### Runtime Behavior

# Generate config for 1000 scalars

**FM**: Generates random values (0-1) and writes to preCICE  python3 generate_precice_config.py 1000

**Wave**: Reads values and computes average (~0.5 expected)```



### Environment VariableThis creates `precice-config.xml` with:

- All scalar data declarations

`PRECICE_NUM_DUMMY_SCALARS` controls the number at runtime (0-10000):- FM and Wave mesh definitions

- Default: 100- Participant configurations (write/read data)

- Set to 0 to disable for production runs- Nearest-neighbor mapping

- Must match the value used in `add_dummy_scalars.py`- Serial explicit coupling scheme



## Usage Examples## Runtime Configuration



**Test with 500 scalars:**The number of dummy scalars can be configured at runtime using the `PRECICE_NUM_DUMMY_SCALARS` environment variable:

```bash

python3 add_dummy_scalars.py 500```bash

source ~/.precice_env# Use default (100 scalars)

./run_precice.shexport PRECICE_NUM_DUMMY_SCALARS=100

```

# Test with 500 scalars

**Scaling study:**export PRECICE_NUM_DUMMY_SCALARS=500

```bash

for N in 10 50 100 500 1000; do# Disable dummy mesh (for production runs)

    python3 add_dummy_scalars.py $Nexport PRECICE_NUM_DUMMY_SCALARS=0

    source ~/.precice_env```

    ./run_precice.sh

done**Important**: The value must match the number of scalars in your generated `precice-config.xml`.

```

Features:

**Disable for production:**- **Default**: 100 scalars if not set

```bash- **Range**: 0 to 10000 (enforced with warnings)

export PRECICE_NUM_DUMMY_SCALARS=0- **Disabled**: Set to 0 to skip dummy mesh registration

```- **Validation**: Automatic bounds checking and error handling



## Expected Output## Running Tests



**FM**: `[FM] Using PRECICE_NUM_DUMMY_SCALARS= 100 from environment`  ### Prerequisites

**FM**: `[FM] Wrote 100 dummy scalars, last value: 0.XXXX`1. Build Delft3D with preCICE support:

   ```bash

**Wave**: `[Wave] Using PRECICE_NUM_DUMMY_SCALARS= 100 from environment`     cd /workspaces/Delft3D

**Wave**: `[Wave] Read 100 dummy scalars, average value: ~0.5`   ./build_install_fm_suite_unix.sh -DENABLE_PRECICE=ON

   ```

## Troubleshooting

2. Generate preCICE configuration:

| Issue | Solution |   ```bash

|-------|----------|   cd examples/precice_scalar_latency_test

| "Data not defined" error | Regenerate config: `python3 add_dummy_scalars.py $PRECICE_NUM_DUMMY_SCALARS` |   python3 generate_precice_config.py 100

| Average not ~0.5 | Check mesh registration (both at 0,0) |   ```

| Dummy scalars not running | Set and source environment variable |

3. Ensure preCICE library is available:

## Files   ```bash

   export LD_LIBRARY_PATH=/path/to/precice/lib:$LD_LIBRARY_PATH

- `add_dummy_scalars.py` - Configuration augmentation script   ```

- `precice_config.xml` - Modified configuration (generated)

- `QUICK_REFERENCE.md` - One-page quick start### Execution

- `IMPLEMENTATION_SUMMARY.md` - Technical detailsRun FM and Wave in separate terminals with preCICE configuration:



**Base case**: `../08_dflowfm_sequential_dwaves/`**Terminal 1 (FM):**

```bash

## Referencescd examples/precice_scalar_latency_test



- preCICE docs: https://precice.org/docs.html# Set number of dummy scalars (must match XML config)

- Performance: https://precice.org/configuration-acceleration.htmlexport PRECICE_NUM_DUMMY_SCALARS=100

export PRECICE_CONFIG=precice-config.xml

/path/to/dflowfm --autostartstop your_fm_model.mdu
```

**Terminal 2 (Wave):**
```bash
cd examples/precice_scalar_latency_test

# Set same number of dummy scalars
export PRECICE_NUM_DUMMY_SCALARS=100
export PRECICE_CONFIG=precice-config.xml

/path/to/wave your_wave_input.mdw
```

### Expected Output

**FM Console:**
```
[FM] PRECICE_NUM_DUMMY_SCALARS not set, using default: 100
[FM] Registered dummy mesh with 1 vertex at (0,0)
[FM] Created 100 dummy scalar field names
...
[FM] Wrote 100 dummy scalars, last value: 0.7234
```

Or with environment variable set:
```
[FM] Using PRECICE_NUM_DUMMY_SCALARS= 500 from environment
[FM] Registered dummy mesh with 1 vertex at (0,0)
[FM] Created 500 dummy scalar field names
...
```

**Wave Console:**
```
[Wave] PRECICE_NUM_DUMMY_SCALARS not set, using default: 100
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
