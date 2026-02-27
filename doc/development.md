# Getting started with code compilation and development

## General intro
The repository includes the source code of both Delft3D 4 and Delft3D FM.
The CMake `CONFIGURATION_TYPE` allows you to select whether you want to build the binaries for Delft3D 4 (`d3d4-suite`), Delft3D FM (`fm-suite`) or both (`all`).
The Delft3D 4 suite consists of the following binaries:
- `d_hydro` and `flow2d3d` for Delft3D-FLOW (including sediment transport and morphology)
- `wave` for Delft3D-WAVE
- `rtc` for Real-Time Control
- `waq` for Delft3D-WAQ
- `part` for Delft3D-PART

The Delft3D FM suite consists of the following binaries:
- `dimr` for the Deltares Integrated Model Runner
- `dflowfm` for D-Flow FM (including sediment transport and morphology)
- `wave` for D-Waves
- `fbc` for D-Real Time Control (Feed-Back Control)
- `waq` for D-Water Quality
- `rr` for D-Rainfall Runoff (part of the Delft3D FM 1D2D suite)

The combined set of Delft3D FM binaries is frequently referred to as `dimrset` based on the central role the DIMR coupling program.
This is sometimes also used for the full set of binaries of both Delft3D suites together.

## Detailed guidance
See the following sub-pages:

- Prerequisites and compilation steps for **Windows** can be found [here](compiling_Windows.md).
- Prerequisites and compilation steps for **Linux** can be found [here](compiling_Linux.md).
- The build steps are described [here](compiling_Linux.md).
- How to **debug** the code on [Windows](debugging.md) and Linux (to be added).
- Various ways of testing the functionality: [**Unit testing**](unit-testing.md), **regression testing** and **validation** (latter sections to be added).
- How to [**contribute**](contributing.md) to the development?
