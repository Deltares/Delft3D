# Mapmerge

`dfmoutput mapmerge --infile model_0000_map.nc model_0001_map.nc --outfile model_merged_map.nc`
merges partitioned map output, including output restricted with `MapOutputPolygonFile`.

Alternatively, supply the base (unpartitioned) MDU name:

```powershell
dfmoutput.exe -d mapmerge --infile fm_model_new.mdu --outfile fm_model_new_merged_map.nc
```

There are two workflows:

- **From the output directory:** the MDU need not exist. Its name is used as a
  template to discover `<modelname>_NNNN_map.nc` files in that directory. A path
  to a nonexistent MDU searches beside that path instead.
- **Using an existing MDU:** mapmerge reads `[output] OutputDir`, resolved relative
  to the MDU, or defaults to `DFM_OUTPUT_<modelname>`. For example, from the output
  directory you can supply `--infile ..\fm_model_new.mdu`.

Discovery includes gaps in four-digit partition numbers and excludes sequential
or previously merged maps. An existing MDU with a custom `MapFile` setting
requires an explicit input file list. An MDU name cannot be combined with other inputs.
Explicit relative output filenames are still resolved against the working directory.

- Keep the original `_NNNN_map.nc` partition suffixes. Input order does not matter.
- Partitions outside the output polygons need not have files. Supply either the
  existing files or the full filename list; nonexistent files are skipped with a
  warning. Existing files that cannot be opened remain errors.
- One existing partition is sufficient. Its ghost cells are still removed.
- Every retained ghost cell/node must have an owned copy among the supplied files.
  Missing owning partitions are reported as errors.
- Global-ID lookups account for the original model size (`flowelem_globalnr`'s
  `valid_max`, or the largest supplied global ID when that attribute is absent).
- Retained 1D nodes are numbered contiguously in original global-ID order.
  Connectivity and contact references use the compact output indices; the
  `flowelem_globalnr` and `flowelem_domain` variables retain their original values.
- Per-node arrays, including 1D cell-contour coordinates, follow the same compact
  numbering and ghost removal as scalar node values.

The merged mesh represents the retained geometry and data. Its entity ordering
and partition metadata need not match a sequential cropped map file.

## Regression tests

The executable tests generate small NetCDF inputs and require Python, NumPy,
and netCDF4. Run from the repository root with a built executable and its runtime
libraries on `PATH`, for example in PowerShell:

```powershell
$env:DFMOUTPUT_EXE = (Resolve-Path .\build_dflowfm\dfmoutput\Debug\dfmoutput.exe).Path
$env:PATH = (Resolve-Path .\install_dflowfm\bin).Path + ';' + $env:PATH
python .\src\tools_gpl\dfmoutput\test\test_mapmerge.py -v
```
