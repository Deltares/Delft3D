# dflowfm_io

Python bindings for the shared D-Flow FM IO library (`dflowfm_io_api`). The native library is bundled
inside the wheel (`dflowfm_io/_lib/`) and loaded automatically, so consumers only need to
`pip install` the platform wheel.

## Generated code

The two typed layers of the package are **generated** from the single sources of truth (the C header
and `mdu.json`) — do **not** hand-edit them. Two generator scripts under `scripts/` produce them:

| Generator (`scripts/`)  | Source of truth                                 | Generates                     |
|-------------------------|-------------------------------------------------|-------------------------------|
| `generate_model.py`     | `../../include/dflowfm_io_api/dflowfm_io_api.h` | `src/dflowfm_io/mdu/model.py` — the typed Layer-1 `MduModel` get/set accessors (one per `mdu_get_*`/`mdu_set_*`) |
| `generate_schema.py`    | `../../../json/mdu.json`                         | `src/dflowfm_io/mdu/schema.py` — the typed Layer-2 `MduSchema` (one class per INI section, one typed property per keyword) plus a `KNOWN_PROPERTIES` registry of every dotted key |

Everything else is hand-written, including `base/bindings.py` (a small, rarely-changing ctypes ABI
mirror: `argtypes`/`restype`, the `mdu_severity_t` constants, and the `mdu_issue_t` struct), the
`MduDocument` lifecycle, and the `Issue`/`Severity` value types.

**The generated files are not committed.** They are git-ignored and produced by the build — a build
is needed to use the package anyway, since it also stages the native library into `_lib/`. The CMake
`dflowfm_io_bindings` target regenerates them from the header and `mdu.json`, so they can never drift
from the ABI or the schema; a golden test asserts the on-disk files equal a fresh regeneration.

### Regenerating manually

The generators need only Python 3 (standard library — no third-party dependencies). From this
directory (`wrappers/python`):

```bash
python scripts/generate_model.py    # -> mdu/model.py
python scripts/generate_schema.py   # -> mdu/schema.py
```

Each prints what it wrote (e.g. `Wrote 22 MduModel accessors ...`, `Wrote 22 sections, 703 typed
properties ...`). Both fail closed: an unmapped C accessor suffix, an unknown `mdu.json` value type,
or a name collision raises instead of silently dropping output.

### Regenerating through CMake

The `dflowfm_io_bindings` CMake target regenerates both files; it depends on the header, the JSON,
and the generator scripts, and the `dflowfm_io_wheel` target depends on it. So editing the C header
or `mdu.json` and rebuilding is enough — the Python side is regenerated automatically:

```bash
cmake --build <build-dir> --target dflowfm_io_bindings --config Release
```

When the C ABI or `mdu.json` changes, edit the header / `mdu.json` (never the generated files), then
regenerate (either command above).

## Building the wheel

The wheel is built through CMake (the `dflowfm_io_wheel` target), which first compiles the native
library, stages it into `src/dflowfm_io/_lib/` (via the `dflowfm_io_stage_lib` target), and
regenerates the Python code, then runs `pip wheel`.

Configure the standalone `dflowfm_io` project once (see the top-level `dflowfm_io` README for the
configure step), then build the wheel target:

```bash
cmake --build <build-dir> --target dflowfm_io_wheel --config Release
```

The wheel is written to `<build-dir>/dflowfm_io_api/wrappers/python/wheel/`, e.g.
`dflowfm_io-0.1.0-py3-none-win_amd64.whl`.

Notes:

- `--config Release` matters with the Visual Studio (multi-config) generator; a Debug wheel bundles a
  debug-CRT library that will not load on machines without the debug runtime.
- The wheel is **not** part of the default build. Build the `dflowfm_io_wheel` target explicitly, or
  opt it into the default build by configuring with `-D DFLOWFM_IO_BUILD_WRAPPER_PACKAGES=ON` (the
  option is declared in `dflowfm_io/cmake/options.cmake`).
- The wheel target builds and stages the native library first, so `_lib/` is always populated before
  `pip wheel` packages it.
