# dflowfm_io

Python bindings for the shared D-Flow FM IO library (`dflowfm_io_api`). The native library is bundled
inside the wheel (`dflowfm_io/_lib/`) and loaded automatically, so consumers only need to
`pip install` the platform wheel.

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
