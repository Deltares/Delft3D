# NVHPC devcontainer (D-Flow FM PoC)

This devcontainer targets the nvfortran proof-of-concept flow and is intended to be reproducible out of the box.

## What is included in recipes

The following is now installed by the tracked container recipes:
- NVHPC toolchain from base image (`nvfortran`, OpenMPI wrappers)
- GNU C/C++ compilers (`gcc`, `g++`) for mixed-toolchain builds
- `python3.14` (via `uv`, linked as `python3.14`)
- `conan` (via `uv tool install`)
- modern `cmake` (via `uv tool install`, not Ubuntu's older apt version)
- default shell environment exports:
  - `DELFT3D_CONAN_PROFILE=delft3d_ubuntu22_nvhpc`
  - `OMPI_CC=gcc`, `OMPI_CXX=g++`, `OMPI_FC=nvfortran`
- post-create bootstrap of VS Code files (if absent):
  - `.vscode/tasks.json`
  - `.vscode/settings.json`
  - `.vscode/launch.json`

## What remains local/custom by design

- The local `.vscode/` directory is git-ignored in this repository.
- Personal edits to `.vscode/*.json` are not shared unless copied to tracked examples under:
  - `.devcontainer/delft3d-nvhpc/examples/.vscode-example/`
- Existing build directories (`build_*`) and Conan cache contents are local state.

## One-time prerequisite

Build the NVHPC third-party base image first:

```bash
docker build -f ci/dockerfiles/linux/third-party-libs-nvhpc.Dockerfile -t delft3d-third-party-libs:nvhpc .
```

Then reopen/rebuild the devcontainer using `.devcontainer/delft3d-nvhpc/devcontainer.json`.

## Repro steps in a fresh container

1. Open repository in the NVHPC devcontainer and let `postCreateCommand` finish.
2. Run task `nvfortran: conan install (build deps from source)`.
3. Run task `nvfortran: cmake configure dflowfm Debug` (or Release).
4. Run task `nvfortran: build dflowfm debug` (or Release).
5. Run tests through CMake Test Explorer / CTest.

## Notes

- Build tasks source `build_*/conan/generators/conanrun.sh` before `cmake --build` so test discovery/runtime can find shared libs.
- If you update local `.vscode` behavior and want to share it, mirror changes into `.devcontainer/delft3d-nvhpc/examples/.vscode-example/`.
