---
name: add-vscode-debug-config
description: 'Add a debug configuration to the `.vscode/launch.json` to debug a `TestBench` test case'
argument-hint: '[testcase-name] [build-config] [build-type]'
---

# Add VSCode debug config to `launch.json`

## When to use
- "Please add a debug config for test case X"

## What this skill does
It makes sure the user can quickly start debugging a `TestBench` model. Setting this up by hand requires
many steps and is error prone.

## Preconditions
1. The _test case input_ files should be downloaded. Use the skill `download-testbench-testcase` and
   pass the name of the test case. The _test case input_ files should appear in the _cases directory_:
   `/test/deltares_testbench/data/cases/<full-testcase-name>`
2. The relevant Delft3D _engine_ / _kernel_ binaries should be built and installed. Use the skill
   `build-delft3d`. If the `build-config` is not specified, build and install `fm-suite`. This should
   build all of them. If the `build-type` is not specified, use `Debug`. The user may also choose
   `RelWithDebInfo` instead because the resulting program runs much faster. Avoid building in `Release`
   mode, because then the debug symbols will be missing. The binaries should appear in the
   _install directory_:
   - Linux: `/build_<build-config>_<build-type>/install/`
   - Windows: `/install_<build-config>/`

## Templates

Four ready-made VS Code launch-configuration templates live next to this `SKILL.md`:

- [dimr-lnx-template.json](dimr-lnx-template.json) — Linux, uses `cppdbg` (`gdb`) to debug the native `dimr`
  binary from the Linux build tree.
- [dimr-win-template.json](dimr-win-template.json) — Windows, uses `cppvsdbg` (MSVC debugger) to debug
  `dimr.exe` from the Windows install tree.
- [dflowfm-lnx-template.json](dflowfm-lnx-template.json) — Linux, uses `cppdbg` (`gdb`) to debug the native `dflowfm-cli`
  binary from the Linux build tree.
- [dflowfm-win-template.json](dflowfm-win-template.json) — Windows, uses `cppvsdbg` (MSVC debugger) to debug
  `dflowfm-cli.exe` from the Windows install tree.

Each template is a single VS Code launch configuration object (not a full `launch.json`).
If the _test case input_ contains a `dimr_config.xml` or a `dimr.xml` file: *Always* prefer the `dimr` templates.
If the test case does not contain a `dimr` config, but does contain a `.mdu` file, opt for the `dflowfm` templates.

### Placeholders

Agents must replace every `{{PLACEHOLDER}}` in the chosen template before inserting the configuration
into `launch.json`:

- `{{TESTCASE_NAME}}`: Full test case name; must match the directory under `data/cases/`. (e.g. `e02_f091_c040_rws_waal`)
- `{{BUILD_CONFIG}}`: `--config` value used with `build.py`. (e.g. `fm-suite`)
- `{{BUILD_TYPE}}`: `--build-type` value used with `build.py`. (e.g. `Debug`)
- `{{DIMR_CONFIG_FILE}}`: Name of the DIMR file inside the test case. Usually `dimr_config.xml` or `dimr.xml`. (e.g. `dimr_config.xml`)
- `{{MDU_FILE}}`: Name of the DFlow FM MDU file. (e.g. `my-model.mdu`)

Sanity-check that the resulting `program` path exists before writing the config, e.g.
`build_fm-suite_debug/install/bin/dimr` on Linux or `install_fm-suite\x64\bin\dimr.exe` on Windows.

## Where to write the configuration

Add the populated template as a new entry in the `configurations` array of the
`Delft3D` workspace folder's `/.vscode/launch.json`. Create that file with the standard
skeleton if it does not yet exist:

```json
{
    "version": "0.2.0",
    "configurations": []
}
```

Preserve any existing entries and JSON comments; append the new configuration at the end of
`configurations`. If a configuration with the same `name` already exists, replace it in place instead
of adding a duplicate.
