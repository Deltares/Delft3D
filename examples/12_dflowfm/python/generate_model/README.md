# D-Flow FM model generator

This script generate a set of D-Flow FM model input files for 2D rainfall
runoff modelling. The required input is a set of spatial input files,
which come as polygon files and Arcinfo raster files.

## Getting Started

The top part of `generate_model.py` contains settings that you should
edit to match your system paths and input file.

### Prerequisites

The model generator script requires:
* Python (2) with modules `mako`, `optparse`.
* D-Flow FM commandline interface executable `dflowfm-cli` (`.exe` on
  Windows).
* GDAL commandline utilities (`gdal_translate` and `gdal_calc.py`).

The required model source input data is documented inside the
``generate_model.py`` script itself.

### Installing

This script is not yet a Python package or module. Therefore, place this
script inside your working directory where all model source data resides.

## Running

```python generate_model.py --[no]overwrite```

