# Repository Guidelines for Agents

## Project

This repo hosts the _Delft3D Flexible Mesh_ and _Delft3D 4_ simulation engines.
- Source code: `/src` (Mostly Fortran and a bit of C/C++)
- TestBench: `/test/deltares_testbench` (Python)

## Build and test

- Building and installing dependencies: Use the `build-delft3d` skill.
- Run unit tests: Use the `run-delft3d-unit-test` skill.
- Run the TestBench (integration tests): Use the `run-testbench` skill.

## Toolchain

- Build system: CMake 4
- Compilers: `ifx` for Fortran, on Windows: MSVC for C/C++, on Linux: `g++` and `icx`
- Package management: Conan 2
- Fortran formatter: `fprettify`

## Command line tools

### Windows
- Git
- Powershell
- Python: 3.12

### Linux (devcontainer)
- Git
- Use `rg`/`fd` over `grep`/`find`.
- For querying / manipulating data:
  - NetCDF: `ncdump`, `nccopy`, `ncgen`
  - XML: `xmllint`, `xmlstarlet`
  - JSON: `jq`
- Python: `uv` with Python 3.12 installed
