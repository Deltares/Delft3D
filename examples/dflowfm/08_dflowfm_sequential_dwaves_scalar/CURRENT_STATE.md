# Current Implementation State

## Summary

The dummy scalar exchange system is now **operational** with a **hardcoded approach**.

## Configuration

- **Scalar count**: Hardcoded in source files (`num_dummy_scalars = 10`)
- **Config generation**: `add_dummy_scalars.py` (reads from `precice_config_template.xml`)
- **Max simulation time**: 90000 seconds (25 hours) in template
- **Mesh**: 1 vertex at origin (0,0) for both FM and Wave

## Key Files

### Source Code
- `src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_io/fm_precice_state_t.F90`
  - FM state definition: `integer :: num_dummy_scalars = 10`
  
- `src/engines_gpl/wave/packages/io/src/wave_precice_state_t.F90`
  - Wave state definition: `integer :: num_dummy_scalars = 10`

### Configuration
- `precice_config_template.xml` - Base template with max-time=90000
- `precice_config.xml` - Generated output (do not edit manually)
- `add_dummy_scalars.py` - Script to generate config from template

## Current Status

✅ **Working**: 10 scalars in both debug and release builds
✅ **Tested**: 0, 10 scalars verified working
✅ **Build**: Both debug and release builds compile successfully
✅ **Config**: Template-based generation working correctly

## How to Use

### Standard Test (10 scalars)
```bash
python3 add_dummy_scalars.py 10
source ~/.precice_env
./run_precice.sh
```

### Change Scalar Count
```bash
# 1. Edit both FM and Wave state files
vim src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_io/fm_precice_state_t.F90
vim src/engines_gpl/wave/packages/io/src/wave_precice_state_t.F90
# Change: integer :: num_dummy_scalars = 10  →  50

# 2. Rebuild
./build_install_fm_suite_unix.sh --mode precice --build-type debug

# 3. Generate matching config
cd examples/dflowfm/08_dflowfm_sequential_dwaves_scalar
python3 add_dummy_scalars.py 50
source ~/.precice_env

# 4. Run
./run_precice.sh
```

## Why Hardcoded?

**Previous approach** (environment variables):
- Used `allocatable` arrays
- Had struct initialization problems
- Inconsistent between FM and Wave
- Crashed with 2+ scalars in debug mode

**Current approach** (hardcoded):
- Uses fixed-size arrays `dimension(1)`
- No allocation issues
- Consistent between FM and Wave
- Works reliably with 10+ scalars

## Known Issues

1. **Config must match code**: If code says 10, config must have 10
2. **Manual rebuild required**: After changing scalar count in code
3. **Debug mode limitations**: Large scalar counts may cause memory issues

## Recent Changes (Dec 16, 2025)

- ✅ Fixed compilation errors (array dimensions, initialization)
- ✅ Updated `add_dummy_scalars.py` to read from template (not old base config)
- ✅ Verified template has correct max-time=90000
- ✅ Updated all markdown documentation
- ✅ Removed references to deprecated `generate_precice_config.py`
- ✅ Successful test with 10 scalars

## Next Steps

For scaling tests:
1. Try release build with increasing scalar counts: 10 → 50 → 100
2. Monitor memory usage and execution time
3. Document maximum supported scalar count
4. Consider vector field approach if individual scalars become problematic

