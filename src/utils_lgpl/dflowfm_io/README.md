# dflowfm_io

A unified library for reading, writing, validating, and migrating D-Flow Flexible Mesh (FM) kernel files, part of the Delft3D modeling suite. The library provides a single, consistent source of truth for handling D-Flow FM input files (such as MDU) across the Hydrodynamics & Morphology (HDM) product line, replacing multiple custom implementations with one maintainable approach.

The library offers functionality for:

- Reading files with built-in automatic migration to the latest file version.
- Writing files with configurable formatting, comment preservation, and structure preservation.
- Validating files with clear, consistent, and detailed messages.
- Creating new files from scratch, initialized with sensible defaults.
- Editing strongly typed data models, even when the resulting model is invalid.

JSON specification files serve as the single source of truth, defining the file sections, keywords, value types, and validation rules. Currently the following specification files are available, and this list may be extended in the future:

- MDU: [mdu.json](./json/mdu.json)

Parts of the dflowfm_io C++ core and the language bindings are auto-generated from these specifications, ensuring consistency across all products and documentation.


## Architecture Overview

**Core Design**

- Core library implemented in C++.
- Centralized data model always containing the latest data.
- Separation of concerns (I/O, validation, migration).

**Interoperability**

A C ABI layer provides cross-language compatibility, with bindings for:

- C#
- Fortran
- Python

**Code Generation**

JSON specification files define the rules of a D-Flow FM input file (allowed keywords, section membership, value types, etc.).
This specification is the single source of truth and is used to auto-generate large parts of the code, including the
data model and metadata, before compilation.

## Python wrapper

The Python binding (`dflowfm_io_api/wrappers/python`) is a ctypes package, `dflowfm_io`, over the C ABI. It is
distributed as a platform wheel that bundles the native library, so consumers only need to `pip install` it — no build
or compiler on their side. This is the binding through which **hydrolib-core** consumes `dflowfm_io`.

Over an opened MDU document (`MduDocument`) it exposes two typed layers:

- **Layer 1 — `MduModel`**: typed get/set of any property by its dotted `section.keyword` key.
- **Layer 2 — `MduSchema`**: a typed, discoverable surface with one class per INI section and one typed property per
  keyword, each delegating to Layer 1.

`mdu/schema.py` (Layer 2) is generated from `json/mdu.json` at build time; the rest of the package is hand-maintained.
See [`dflowfm_io_api/wrappers/python/README.md`](./dflowfm_io_api/wrappers/python/README.md) for the package layout,
building the wheel, and regenerating the generated code.

## Building dflowfm_io

Prerequisites:

- CMake 3.19 or higher (required)
- A C++20 compatible compiler (required)
- Git (required)
- Python 3 (required for code generation and Python bindings)
- A Fortran compiler (optional; required only to build the Fortran bindings)
- .NET 10 SDK (optional; required only to build the C# bindings)



### Building standalone

Follow the steps below to configure and build dflowfm_io standalone.

**Steps**

1. To configure under Windows with Visual Studio, a solution is generated using:

```cmd
cmake -S <path-to-source-dir> -B <path-to-build-dir> -G "Visual Studio 17 2022"
  ```

  where

- `<path-to-source-dir>` is the path to the dflowfm_io source directory.
- `<path-to-build-dir>` is the path to the directory where the library is to be built.

2. To build the project's targets, use:

```cmd
cmake --build <path-to-build-dir>
 ```

The build produces the C++ core and, for each language binding whose required tools are installed, the corresponding wrapper.

### Building as part of Delft3D

dflowfm_io is also built (and tested) as part of the Delft3D cmake build. For more information, refer to the build instructions in the Delft3D repository.

### Building the wrapper packages

The wrapper packages — the Python wheel (`dflowfm_io`, bundling the native library) and the NuGet package
(`DFlowFM.IO.NET`, bundling the C# bindings) — are not part of the default build. To build both as part of the
default build instead, configure with `-D DFLOWFM_IO_BUILD_WRAPPER_PACKAGES=ON`.

Alternatively, build one or both wrapper packages on demand via their individual targets, `dflowfm_io_wheel` and
`dflowfm_io_csharp_pack` respectively:

```cmd
cmake --build <path-to-build-dir> --target dflowfm_io_wheel --config Release
cmake --build <path-to-build-dir> --target dflowfm_io_csharp_pack --config Release
```

`dflowfm_io_wheel` compiles and stages the native library, generates the Python code, then runs `pip wheel`. The
resulting wheel is written to `<path-to-build-dir>/dflowfm_io_api/wrappers/python/wheel/` (e.g.
`dflowfm_io-0.1.0-py3-none-win_amd64.whl`). See
[`dflowfm_io_api/wrappers/python/README.md`](./dflowfm_io_api/wrappers/python/README.md) for details.

`dflowfm_io_csharp_pack` builds the C# solution, then packs it via `dotnet pack`. The resulting package is written
alongside the C# build output,
`<path-to-build-dir>/dflowfm_io_api/wrappers/csharp/bin/<cfg>/` (e.g. `DFlowFM.IO.NET.0.1.0.nupkg`).


## Testing

The library is verified through an automated test suite. Running the tests also executes the language binding tests for any bindings that were built.

To run all tests after building, use:

```cmd
ctest --test-dir <path-to-build-dir> --build-config <cfg>
```

where

- `<path-to-build-dir>` is the path to the directory where the library was built.
- `<cfg>` is the build type used during the build (e.g. `Debug` or `Release`).

To see more detailed output on failure, add `--output-on-failure`:

```cmd
ctest --test-dir <path-to-build-dir> --build-config <cfg> --output-on-failure
```