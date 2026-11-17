\page pre_c_sumo_readme Doxygen setup

# Doxygen setup

This directory contains the documentation sources for preC-SUMO, including the Doxygen pages, diagrams, and supporting files used to explain the runtime flow and configuration.

## Doxygen setup and generation

Install the following software tools:

- Doxygen 1.13.2 (installed in `C:\Program Files\doxygen`, by default)
- DrawIO v26.2.15 (installed from the Microsoft Store)
- Graphviz 12.2.1 (installed in `C:\Program Files\Graphviz`, by default)

The project documentation layout is:

- `docs/source/` — contains the Doxygen source pages, such as the main page and additional `.dox` files
- `docs/<project>-doxyfile.dxg` — the Doxygen configuration for the project
- `docs/source/<project>-mainpage.dox` — the project main page
- `docs/include/` — contains Graphviz `.dot` files, images, and other media used by the documentation
- `docs/header/` — contains project-specific logos or header graphics
- `docs/result_doc/` — generated Doxygen output; this is not part of git

The main files used for this module are:

- `src/tools_gpl/pre_c_sumo/docs/source/prec-SUMO-mainpage.dox` — high-level overview and navigation page
- `src/tools_gpl/pre_c_sumo/docs/include/preC_SUMO_Core_Workflow.dot` — top-level workflow diagram
- `src/tools_gpl/pre_c_sumo/docs/include/preC_SUMO_Swimlanes.dot` — detailed runtime swimlane diagram
- `src/tools_gpl/pre_c_sumo/docs/preC-SUMO-doxyfile.dxg` — Doxygen configuration used to generate the docs

### Generate the documentation

#### Option 1: Doxygen GUI

1. Start Doxygen.
2. Open the configuration file:
   `src/tools_gpl/pre_c_sumo/docs/preC-SUMO-doxyfile.dxg`
3. Click Run to generate the documentation.
4. Open the generated result in:
   `src/tools_gpl/pre_c_sumo/docs/result_doc/html`

#### Option 2: PowerShell command line

```powershell
& "C:\Program Files\doxygen\bin\doxygen.exe" "C:\checkouts\Delft3D\src\tools_gpl\pre_c_sumo\docs\preC-SUMO-doxyfile.dxg"
```

This reads the project Doxygen configuration and writes the generated output to the `result_doc` folder.

For the official Doxygen user guide, see: https://www.doxygen.nl/manual/starting.html

## Recommended reading path

- Start with the main page in `docs/source/prec-SUMO-mainpage.dox` for the high-level story.
- Use the core workflow diagram to understand the end-to-end runtime sequence.
- Use the detailed swimlane diagram when you need the operational call flow and timing details.
- Refresh the generated HTML after changing diagrams or Doxygen pages.

## Related documentation

- [clang-format and clang-tidy setup](include/clang-format-and-tidy.md) — editor configuration for formatting and static analysis
- [development guidelines](include/guidelines.md) — project conventions and coding expectations

## Exchange responsibilities

- [csumo_data.hpp](../include/csumo_data.hpp) defines C-SUMO settings, FF2NF input,
  and NF2FF result records. Settings own the XML document referenced by their raw
  settings nodes; keep the settings alive until FF2NF XML generation completes.
- [precice_data.hpp](../include/precice_data.hpp) defines preCICE mesh mappings,
  interleaved buffers, quantity identifiers, and source/sink attribute arrays.
  Neither data header depends on the preCICE SDK or on a reader/writer.
- [precice_to_csumo.hpp](../include/precice_to_csumo.hpp) converts received mesh
  buffers into FF2NF data. [csumo_to_precice.hpp](../include/csumo_to_precice.hpp)
  creates meshes from sampling locations and converts NF2FF records into
  source/sink buffers. These conversions do not parse XML or communicate.
- [csumo_diffuser.hpp](../include/csumo_diffuser.hpp) handles DESA geometry,
  independently of transport and XML parsing.
- [csumo_settings_reader.hpp](../include/csumo_settings_reader.hpp) and
  [NF2FF_reader.hpp](../include/NF2FF_reader.hpp) parse XML strings into data.
  [FF2NF_writer.hpp](../include/FF2NF_writer.hpp) validates and generates XML only.
- [exchange_file_io.hpp](../include/exchange_file_io.hpp) reads text and writes
  already-serialized content. Writing creates parent directories and refuses to
  overwrite existing files. [csumo_file_paths.hpp](../include/csumo_file_paths.hpp)
  provides the exchange filename policy without accessing the filesystem.
- [precice_exchange.hpp](../include/precice_exchange.hpp) handles preCICE reads
  and writes. The source/sink administrator only maintains aligned buffers;
  transport clears those buffers after a successful write.

The workflow in [coupling_steps.cpp](../src/coupling_steps.cpp) explicitly performs
conversion, XML generation, and file writing as separate steps. NF2FF read/parse
errors are returned to the caller rather than skipping a diffuser and shifting
the remaining mappings.
