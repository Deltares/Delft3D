# dflowfm_io

Python bindings for the shared D-Flow FM IO library (`dflowfm_io_api`). The native library is bundled
inside the wheel (`dflowfm_io/_lib/`) and loaded automatically, so consumers only need to
`pip install` the platform wheel.

## Generated code

The typed Layer-2 `MduSchema` is **generated** from `json/mdu.json` — do **not** hand-edit it:

| Generator (`scripts/`)  | Source of truth          | Generates                     |
|-------------------------|--------------------------|-------------------------------|
| `generate_schema.py`    | `../../../json/mdu.json` | `src/dflowfm_io/mdu/schema.py` — the typed Layer-2 `MduSchema` (one class per INI section, one typed property per keyword) plus a `KNOWN_PROPERTIES` registry of every dotted key |

Everything else is hand-written, including `base/bindings.py` (the ctypes ABI mirror: `argtypes`/
`restype`, the `mdu_severity_t` constants, and the `mdu_issue_t` struct), `mdu/model.py` (the typed
Layer-1 `MduModel` accessors) — both are small and rarely change, kept in sync with the C header by
hand — the `MduDocument` lifecycle, and the `Issue`/`Severity` value types.

**`schema.py` is not committed.** It is git-ignored and produced by the build — a build is needed to
use the package anyway, since it also stages the native library into `_lib/`. The CMake
`dflowfm_io_bindings` target regenerates it from `mdu.json`, so it can never drift from the schema; a
golden test asserts the on-disk file equals a fresh regeneration.

### Regenerating manually

The generator needs only Python 3 (standard library — no third-party dependencies). From this
directory (`wrappers/python`):

```bash
python scripts/generate_schema.py   # -> mdu/schema.py
```

It prints what it wrote (e.g. `Wrote 22 sections, 703 typed properties ...`) and fails closed: an
unknown `mdu.json` value type or a name collision raises instead of silently dropping output.

### Regenerating through CMake

The `dflowfm_io_bindings` CMake target regenerates `schema.py`; it depends on `mdu.json` and the
generator, and the `dflowfm_io_wheel` target depends on it. So editing `mdu.json` and rebuilding is
enough — `schema.py` is regenerated automatically:

```bash
cmake --build <build-dir> --target dflowfm_io_bindings --config Release
```

When `mdu.json` changes, edit it (never `schema.py`), then regenerate (either command above).

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
