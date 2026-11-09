# Mapmerge

`dfmoutput mapmerge --infile model_0000_map.nc model_0001_map.nc --outfile model_merged_map.nc`
merges partitioned map output, including output restricted with `MapOutputPolygonFile`.

- Keep the original `_NNNN_map.nc` partition suffixes. Input order does not matter.
- Partitions outside the output polygons need not have files. Supply either the
  existing files or the full filename list; nonexistent files are skipped with a
  warning. Existing files that cannot be opened remain errors.
- One existing partition is sufficient. Its ghost cells are still removed.


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
