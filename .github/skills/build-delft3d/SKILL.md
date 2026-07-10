---
name: build-delft3d
description: 'Run the CMake build'
argument-hint: '[config] [build-type] [configure-only] [keep-build] [build-target]'
---

# Run Delft3D builds

## What this skill does
It uses `build.py` to run the build. `build.py` uses `conan` to install dependenies, and then `cmake` to
configure and build the software. It is a python script, which is designed to work on both Windows and 
Linux.
The output of `build.py` is the _build_ directory and (in most cases) the _install_ directory. 
Unfortunately the locations of these directories depends on the platform:
- Linux
  - build directory: `/build_<config>_<build-type>`
  - install directory: `/build_<config>_<build-type>/install`
- Windows
  - build directory: `/build_<config>`
  - install directory: `/install_<config>`

Notice that the names of the directories depend on the `config` and (on Linux) the `build-type` arguments.
On Windows, `cmake` generates Visual Studio solution files, which unlike makefiles, are 
"Build type agnostic".

The user may specify `configure-only` if they do not want to perform a build yet. In this case do not use
the `--build` option in `build.py`. The user may ask for a build later with a specific `build-target`.

If the `build-target` argument is not explicitly specified when the user requests a build: Just use
`build.py` with the `--build` option to install dependencies, _configure_, _build_ and _install_ in one go.
`build.py` lacks functionality to specify a CMake build target. If the user explicitly
specifies a build-target, use `build.py` without the `--build` option to skip the _build_ and _install_
phases. Then use the `cmake --build <build-directory> --target <build-target>` to build a single target.
If there is no such build target, or only a description is given: Use 
`cmake --build <build-directory> --target help` to list targets and let the user select the one they want.

## When to use
- When changes have been made (and saved) to the source code and we want to know if the product still
  builds, the unit tests still pass, or a model (a set of input files) for the product works as expected.

## Preconditions

1. The compiler toolchain should be installed and the compilers and tools should be in the `PATH`
   regardless of the platform. We currently use Intel OneAPI as our toolchain, and an easy way to
   check if the toolchain is loaded in the environment is to check if the environment variable
   ONEAPI_ROOT is set to some path on the system.
2. `python`, `conan` and `cmake` are assumed to be installed in the environment regardless of the 
   platform. If not there are instructions to install the toolchain for 
   [Windows](/doc/compiling_Windows.md) and [Linux](/doc/compiling_Linux.md).
3. `conan` can be configured to install the dependencies from a remote repository, which requires access
   rights and credentials, or from the source code using the recipes. If you run into errors relating to 
   auth errors or missing profiles while running `build.py`, something is most likely misconfigured and 
   you can point people to the installation instructions mentioned above.

## Command anatomy

```
python build.py [--config CONFIG] [--build-type {Debug,Release,RelWithDebInfo}] [--build] [--keep-build]
```

- **`--config`**: What "product" to build. The default is `fm-suite`, which includes most software and
  should be chosen if not explicitly specified. Other options can be found by invoking `python build.py --help`
- **`--build-type`**: The build type passed to `CMake`. The default is `Debug` which is clearly preferred
  for development because it adds debug symbols and turns off optimizations for faster builds. `RelWithDebInfo` is sometimes useful if developers want to run models that run too slow with `Debug` binaries.
- **`--build`**: By default `python build.py` only runs the `conan` install and the `cmake` configure.
  It skips the actual `cmake` build (which takes the longest). But the `--build` flag tells `build.py` to
  also run the _build_ and _install_ phases.
- **`--keep-build`**: By default build.py removes the _build_ and _install_ directories. So it runs a
  "clean build". Builds can take pretty long, so this is actually kind of wasteful. If the build and
  install directories exist, please default to passing the `--keep-build` flag unless explicitly told
  not to.
