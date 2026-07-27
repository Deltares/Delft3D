# dflowfm_io

Python bindings for the shared D-Flow FM IO library (`dflowfm_io_api`). The native library is bundled
inside the wheel (`dflowfm_io/_lib/`) and loaded automatically, so consumers only need to
`pip install` the platform wheel.

## Generated code

Three modules in this package are **generated** from the single sources of truth (the C header and
`mdu.json`) — do **not** hand-edit them. Two generator scripts under `scripts/` produce them:

| Generator (`scripts/`)   | Source of truth                                  | Generates                     |
|--------------------------|--------------------------------------------------|-------------------------------|
| `generate_bindings.py`   | `../../include/dflowfm_io_api/dflowfm_io_api.h`  | `src/dflowfm_io/base/bindings.py` — ctypes `argtypes`/`restype`, the `mdu_severity_t` enum, and the `mdu_issue_t` struct |
| `generate_bindings.py`   | *(same header)*                                  | `src/dflowfm_io/mdu/model.py` — the typed `MduModel` get/set accessors (one per `mdu_get_*`/`mdu_set_*`) |
| `generate_schema.py`     | `../../../json/mdu.json`                          | `src/dflowfm_io/mdu/schema.py` — the typed `MduSchema` (one class per INI section, one typed property per keyword) |

The generated files are committed (so the package imports without a build) **and** rebuilt during the
CMake build, so they cannot drift from the ABI or the schema.

### Regenerating manually

The generators need only Python 3 (standard library — no third-party dependencies). From this
directory (`wrappers/python`):

```bash
python scripts/generate_bindings.py   # -> base/bindings.py + mdu/model.py
python scripts/generate_schema.py      # -> mdu/schema.py
```

Each prints what it wrote (e.g. `Wrote 30 function signatures ...`, `Wrote 21 sections, 694 typed
properties ...`). Both fail closed: an unknown C enum form, an unmapped `mdu.json` value type, or a
name collision raises instead of silently dropping output.

### Regenerating through CMake

The `dflowfm_io_bindings` CMake target regenerates all three files; it depends on the header, the
JSON, and the generator scripts, and the `dflowfm_io_wheel` target depends on it. So editing the C
header or `mdu.json` and rebuilding is enough — the Python side is regenerated automatically:

```bash
cmake --build <build-dir> --target dflowfm_io_bindings --config Release
```

**When the C ABI or `mdu.json` changes**, edit the header / `mdu.json` (never the generated files),
then regenerate (either command above). If a change adds a new C-API function, also update the
Fortran and C# wrappers to keep the shared contract in sync.

## Building the wheel

The wheel is built through CMake (the `dflowfm_io_wheel` target), which first compiles the native
library and stages it into `src/dflowfm_io/_lib/`, then runs `pip wheel`. Configure the standalone
`dflowfm_io` project once, then build the wheel target:

```bash
# 1. Configure (once) — -S is the dflowfm_io project root, -B is any build directory
cmake -S <path-to>/src/utils_lgpl/dflowfm_io \
      -B <path-to>/src/utils_lgpl/dflowfm_io/out/build/x64-Release \
      -G "Visual Studio 17 2022" -A x64

# 2. Build the wheel (compiles the native library first)
cmake --build <path-to>/src/utils_lgpl/dflowfm_io/out/build/x64-Release \
      --target dflowfm_io_wheel --config Release
```

The wheel is written to `<build-dir>/dflowfm_io_api/wrappers/python/wheel/`, e.g.
`dflowfm_io-0.1.0-py3-none-win_amd64.whl`.

Notes:

- `--config Release` matters with the Visual Studio (multi-config) generator; a Debug wheel bundles a
  debug-CRT library that will not load on machines without the debug runtime.
- To build the wheel as part of the normal build (no explicit `--target`), configure with
  `-D DFLOWFM_IO_BUILD_PYTHON_WHEEL=ON`.
- Building the wheel with an empty `src/dflowfm_io/_lib/` fails on purpose (the wheel would be missing
  its native library); build the `dflowfm_io_api` target first, which the wheel target does
  automatically.
