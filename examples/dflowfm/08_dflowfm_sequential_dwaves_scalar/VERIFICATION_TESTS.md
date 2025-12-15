# Verification Tests for add_dummy_scalars.py

## Test Date
December 15, 2025

## Test Summary

All tests **PASSED** ✓

## Test Cases

### Test 1: Zero Scalars (Identity Test)
**Command**: `python3 add_dummy_scalars.py 0`

**Expected**: Output file should be identical to input file

**Result**: ✓ PASSED
```
✓ Files are identical!
diff reported no differences
```

**Verification**:
```bash
diff ../08_dflowfm_sequential_dwaves/precice_config.xml precice_config.xml
# No output = files are identical
```

### Test 2: Small Number of Scalars
**Command**: `python3 add_dummy_scalars.py 5`

**Expected**: 
- 5 data declarations
- 5 use-data in fm_dummy_mesh
- 5 use-data in wave_dummy_mesh
- 5 write-data in FM participant
- 5 read-data in Wave participant
- 5 exchanges in coupling scheme
- Total: 30 occurrences of "fm_scalar_"

**Result**: ✓ PASSED
```
grep -c "fm_scalar_" precice_config.xml
30
```

**Sample output**:
```xml
<data:scalar name="fm_scalar_0000001" />
<write-data name="fm_scalar_0000001" mesh="fm_dummy_mesh" />
<read-data name="fm_scalar_0000001" mesh="fm_dummy_mesh" />
<exchange data="fm_scalar_0000001" mesh="fm_dummy_mesh" from="fm" to="wave" />
```

### Test 3: Medium Number of Scalars  
**Command**: `python3 add_dummy_scalars.py 10`

**Result**: ✓ PASSED
```
grep -c "fm_scalar_" precice_config.xml
60  (10 × 6 sections)
```

### Test 4: Large Number of Scalars
**Command**: `python3 add_dummy_scalars.py 50000`

**Expected**: Should complete successfully, generate large config file

**Result**: ✓ PASSED
```
wc -l precice_config.xml
300314 precice_config.xml

grep -c "fm_scalar_" precice_config.xml
300000  (50000 × 6 sections)
```

**Performance**: Generated in <5 seconds

### Test 5: Upper Limit Enforcement
**Command**: `python3 add_dummy_scalars.py 1500000`

**Expected**: Should cap at 1000000 with warning message

**Result**: ✓ PASSED
```
Warning: num_scalars is very large (1500000), capping at 1000000
PRECICE_NUM_DUMMY_SCALARS=1000000
```

### Test 6: Negative Number
**Command**: `python3 add_dummy_scalars.py -10`

**Expected**: Should error and exit

**Result**: ✓ PASSED
```
Error: num_scalars must be non-negative, got -10
Exit code: 1
```

## Range Validation

| Test Value | Expected Behavior | Result |
|------------|-------------------|--------|
| -1 | Error | ✓ PASSED |
| 0 | Copy unchanged | ✓ PASSED |
| 1 | Add 1 scalar | ✓ PASSED |
| 5 | Add 5 scalars | ✓ PASSED |
| 100 | Add 100 scalars (default) | ✓ PASSED |
| 10000 | Add 10000 scalars | ✓ PASSED |
| 50000 | Add 50000 scalars | ✓ PASSED |
| 1000000 | Add 1000000 scalars (max) | ✓ PASSED |
| 1500000 | Cap at 1000000 with warning | ✓ PASSED |

## Structure Validation

For 5 scalars, verified presence of:

✓ Data declarations: `<data:scalar name="fm_scalar_0000001" />`  
✓ FM dummy mesh: `<mesh name="fm_dummy_mesh" dimensions="2">`  
✓ Wave dummy mesh: `<mesh name="wave_dummy_mesh" dimensions="2">`  
✓ FM provide-mesh: `<provide-mesh name="fm_dummy_mesh" />`  
✓ FM write-data: `<write-data name="fm_scalar_0000001" mesh="fm_dummy_mesh" />`  
✓ Wave receive-mesh: `<receive-mesh name="fm_dummy_mesh" from="fm" />`  
✓ Wave read-data: `<read-data name="fm_scalar_0000001" mesh="fm_dummy_mesh" />`  
✓ Wave mapping: `<mapping:nearest-neighbor ... from="fm_dummy_mesh" to="wave_dummy_mesh" .../>`  
✓ Exchanges: `<exchange data="fm_scalar_0000001" mesh="fm_dummy_mesh" from="fm" to="wave" />`  

## Environment Variable Setting

✓ Creates `~/.precice_env` file  
✓ Sets correct value in environment file  
✓ Provides instructions to source the file  

Example output:
```
Environment variable set:
  PRECICE_NUM_DUMMY_SCALARS=5

To use in your shell, run:
  source /home/dev/.precice_env
Or manually:
  export PRECICE_NUM_DUMMY_SCALARS=5
```

## XML Format Preservation

✓ Indentation preserved  
✓ Comments added appropriately  
✓ Original FM-SWAN coupling configuration unchanged  
✓ Pseudo-namespaces (data:scalar, mapping:nearest-neighbor) handled correctly  

## Edge Cases

| Case | Result |
|------|--------|
| Input file not found | ✓ Error with clear message |
| Output same as input | ✓ Overwrites safely |
| Very large files (50k+ scalars) | ✓ Completes successfully |
| Zero scalars | ✓ Produces identical copy |
| --set-env-only flag | ✓ Sets env without modifying config |

## Conclusion

The `add_dummy_scalars.py` script has been thoroughly tested and verified to:

1. ✓ Handle the full range of 0 to 1,000,000 scalars
2. ✓ Produce identical output for 0 scalars (bit-for-bit match)
3. ✓ Generate correct XML structure for all scalar counts
4. ✓ Enforce upper limits with appropriate warnings
5. ✓ Set environment variables correctly
6. ✓ Preserve original configuration formatting
7. ✓ Handle edge cases gracefully

**All tests passed successfully.**
