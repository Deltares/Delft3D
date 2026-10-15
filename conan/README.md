# Adding a third-party package to Conan

Delft3D resolves its third-party dependencies with [Conan 2](https://docs.conan.io/2/).
This document describes how to add a new package, using the migration of preCICE from a
vendored binary drop to a Conan package as the worked example.

## Layout

```text
conanfile.py                 Consumer recipe: lists all requirements and dependency options
conan.lock                   Pinned recipe revisions of the whole dependency graph
run_conan.py                 Wrapper around the conan CLI (initialize/update-lockfile/install/upload)
build.py                     Runs run_conan.py + CMake configure/build/install
conan/config/                Profiles, settings and remotes, installed by `run_conan.py initialize`
conan/recipes/<pkg>/         Local recipe
    config.yml               Maps version -> recipe folder
    all/conanfile.py         The recipe
    all/conandata.yml        Source URL + sha256 per version
```

`conan/recipes` is registered as a `local-recipes-index` remote named `local-recipes` with
the highest priority, so a recipe placed there overrides the same package on Nexus /
ConanCenter.

## Steps

### 1. Get a starting point for the recipe

Copy the recipe for the package from `conan-center-index` (or from an existing recipe in
`conan/recipes/`) into a new folder:

```text
conan/recipes/<pkg>/config.yml
conan/recipes/<pkg>/all/conanfile.py
conan/recipes/<pkg>/all/conandata.yml
```

`config.yml`:

Using precice as an example.

```yaml
versions:
  "3.4.1":
    folder: "all"
```

### 2. Fill in `conandata.yml`

Paste the URL of the source tarball and its checksum (Using precice as an example):

```yaml
sources:
  "3.4.1":
    url: "https://github.com/precice/precice/archive/refs/tags/v3.4.1.tar.gz"
    sha256: "ef4713c938a1b2000d0b071175e1b45f9ec55c7aec4bbe7b65c3992edcc74ac7"
```

Compute the checksum after downloading the archive:

```pwsh
Get-FileHash .\v3.4.1.tar.gz -Algorithm SHA256     # Windows
```

```bash
sha256sum v3.4.1.tar.gz                            # Linux
```

### 3. Adapt `conanfile.py`

Start from an existing recipe (`petsc`, `triangle`, `precice`) and let Copilot cross-check
the upstream `CMakeLists.txt` and the existing Delft3D CMake code, so that the produced
package matches how Delft3D consumes it. Points to check:

- **`settings`** — add `"fortran_compiler"` if the package builds Fortran sources or
  Fortran bindings. The custom setting is defined in `conan/config/settings_user.yml`.
- **`package_type`** — e.g. `"shared-library"`, `"header-library"`.
- **`options` / `default_options`** — only expose what Delft3D actually needs; disable
  optional back-ends to keep the graph small.
- **`requirements()`** — dependencies of the package itself.
- **`generate()`** — set the CMake cache variables of the upstream project. Always set
  `CMAKE_INSTALL_LIBDIR=lib` and `CMAKE_INSTALL_BINDIR=bin`, and disable tests.
- **`package_info()`** — the file name, target name and library list must match the
  `find_package()` / `target_link_libraries()` calls in `src/cmake`:

  ```python
  self.cpp_info.set_property("cmake_file_name", "precice")
  self.cpp_info.set_property("cmake_target_name", "precice::precice")
  self.cpp_info.libs = ["precice"]
  ```

- **Line endings must be LF.** See [Line endings](#line-endings) below.

### 4. Add the package to the consumer recipe

Both the requirement and the *options of that dependency* belong in the root
[conanfile.py](../conanfile.py) — options set in a dependency's `default_options` are
ignored when the root recipe requires that dependency directly:

```python
def requirements(self):
    ...
    self.requires("precice/3.4.1")

def configure(self):
    ...
    # preCICE is compiled with BOOST_ALL_DYN_LINK, so Boost must be shared.
    self.options["boost"].shared = True
    # libxml2 is only used by preCICE, which parses UTF-8 only.
    self.options["libxml2"].iconv = False
```

Anything that has to end up next to the executables (DLLs, .so files) must come from a
package that is in `requirements()`; there is no separate "artifacts" list.

### 5. Set up Conan (once per machine)

```pwsh
python ./run_conan.py initialize deltares      # profiles, settings, Nexus remotes, local recipes
```

Use `initialize external` when Nexus is not reachable; only the local recipes remain.

> **Windows:** run every Conan and CMake command from the *Intel oneAPI command prompt*.
> MPI is not a Conan package; it is picked up from the oneAPI installation.

### 6. Regenerate the lockfile

```pwsh
python run_conan.py update-lockfile
```

This resolves the graph in a throw-away `CONAN_HOME` so the local cache cannot leak
revisions into `conan.lock`, and rewrites the lockfile. Commit the result.

### 7. Install and build

```pwsh
python run_conan.py install --build-missing --output-folder build
```

or let `build.py` do the install, configure, build and install in one go:

```pwsh
python build.py --config FM-SUITE --build-type Release
```

### 8. Wire the package into CMake

Add a `find_package` next to the other Conan packages in
[src/cmake/CMakeLists.txt](../src/cmake/CMakeLists.txt) (the Conan generators folder is
already prepended to `CMAKE_PREFIX_PATH`):

```cmake
find_package(precice CONFIG REQUIRED)
```

Then link the imported target from the component that needs it:

```cmake
target_link_libraries(precicef PRIVATE precice::precice)
```

Remove the vendored copy of the library (`src/third_party_open/<pkg>`), its wrapper
`CMakeLists.txt`, the `<pkg>_module` variable in
[src/cmake/modules/third_parties.cmake](../src/cmake/modules/third_parties.cmake) and the
`add_subdirectory` calls in the configuration files. Also drop the corresponding build
stage from [ci/dockerfiles/linux/third-party-libs.Dockerfile](../ci/dockerfiles/linux/third-party-libs.Dockerfile),
so the Linux CI image no longer builds it from source.

Runtime files need no special handling:

- **Windows** — `CMakeDeps` creates `SHARED IMPORTED` targets with `IMPORTED_LOCATION`
  set to the DLL, so the existing
  `install(FILES $<TARGET_RUNTIME_DLLS:${target}> DESTINATION bin)` picks them up.
  Do **not** use `get_target_property(... LOCATION)`: the aggregate `Pkg::Pkg` targets are
  `INTERFACE IMPORTED` and return `NOTFOUND`.
- **Linux** — `src/cmake/install_linux_libs` runs `copy_libs.sh` over the Conan generators
  folder and resolves the shared libraries with `ldd`.

### 9. Publish the binaries

Binaries are published from TeamCity, not from a developer machine. Push the branch with
the new recipe, then run the **Conan packages** build configuration for both platforms:

- *Delft3D → Windows → Conan packages*
- *Delft3D → Linux → Conan packages*

Open the configuration, select your own branch in the **Branch** selector and press
**Run**. The build builds all Conan packages from source and pushes them to the Deltares
Nexus remote, so the package becomes available to everyone else without a local rebuild.

Packages that are already on the remote with the same recipe revision and package id are
skipped, so re-running the build only uploads what actually changed.

> Because the recipe revision is part of the package identity, a recipe with the wrong
> line endings uploads a *different* package than the one your colleagues resolve. Do the
> [line endings](#line-endings) check before triggering the build.

## Line endings

Conan hashes the **raw bytes** of the recipe files to compute the recipe revision. A
recipe with CRLF line endings therefore gets a different revision on Windows than the same
recipe checked out with LF on Linux, and `conan.lock` will differ between the two
platforms:

```diff
-        "precice/3.4.1#f0f63a7fbdf6841391e7f9f75f508238%1785402166.8474548",
+        "precice/3.4.1#c5e2e4403fca029627a1bccfb308713b%1785744440.9276698",
```

`.gitattributes` sets `text eol=lf`, but git *normalizes on comparison*, so a CRLF working
copy is still reported as unmodified by `git status`. Verify with a fresh checkout:

```bash
git ls-files --eol conan/recipes/<pkg>
```

All recipe files must show `i/lf    w/lf`.

## Notes and pitfalls

- **`conan.lock` timestamps always change.** `update-lockfile` re-exports every recipe, so
  the `%<timestamp>` suffix of all entries is rewritten. Only the part after `#` (the
  recipe revision) is meaningful when comparing lockfiles.
- **Multi-config generators and `try_compile`.** On Windows the Visual Studio generator
  runs `try_compile` in `Debug` by default, while `CMakeDeps` guards its properties with
  `$<$<CONFIG:Release>:...>`. Dependency checks in upstream projects then fail with
  "Cannot open include file". Fix it in the recipe:

  ```python
  tc.cache_variables["CMAKE_TRY_COMPILE_CONFIGURATION"] = str(self.settings.build_type)
  ```

- **Boost: "These libraries were built, but were not used in any boost module".** b2 builds
  the transitive dependencies declared in each `libs/<name>/build.jam`, which the Conan
  recipe's `dependencies-<version>.yml` does not always model. Disabling such a module with
  `without_<lib>=True` triggers this error; re-enable the module.
- **CMake configure aborts on a stale stamp.** The build compares the timestamps of
  `conan.lock` / `conanfile.py` against `<build-dir>/conan/generators/conan.stamp`. After
  editing either file, re-run the Conan install before configuring.
