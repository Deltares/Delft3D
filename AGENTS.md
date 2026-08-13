# Repository Guidelines for Agents

## Project

This repo hosts the _Delft3D Flexible Mesh_ and _Delft3D 4_ simulation engines / kernels.
- Public GitHub repo: https://github.com/Deltares/Delft3D
- Source code: `/src` (Mostly Fortran and a bit of C/C++)
- TestBench: `/test/deltares_testbench` (Python)
- TeamCity CI configuration and tools: `/ci` (Python, Dockerfile, TeamCity Kotlin DSL)

## Toolchain

- Build system: CMake 4
- Compilers: `ifx` for Fortran, on Windows: MSVC for C/C++, on Linux: `icx` for C, `g++` for C++
- Package management: Conan 2
- Fortran formatter: `fprettify`

## Additional tools

### Windows
- Git
- Powershell
- Python: 3.12

### Linux (devcontainer)
- Git: `git` and `gh` (GitHub CLI tool)
- Use `rg`/`fd` over `grep`/`find`.
- For querying / manipulating data:
  - NetCDF: `ncdump`, `nccopy`, `ncgen`
  - XML: `xmllint`, `xmlstarlet`
  - JSON: `jq`
- Python: `uv` with Python 3.12 installed
- CI: `teamcity` (TeamCity CLI tool)
- Debugger: `gdb-oneapi` (GDB like interface, but used for `ifx` compiled Fortran binaries)
