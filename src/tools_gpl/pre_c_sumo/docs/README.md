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

With `doxygen` and `dot` on `PATH`, run this from the `docs` directory:

```powershell
doxygen .\preC-SUMO-doxyfile.dxg
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

Utility headers live in `include/utils/`, with corresponding implementations in
`src/utils/`. They are part of `pre_c_sumo_lib`; for example,
[utils/file_io.hpp](../include/utils/file_io.hpp) is implemented in
[utils/file_io.cpp](../src/utils/file_io.cpp). Their unit tests are in `test/utils/`.

Project code uses `pre_c_sumo`, with C-SUMO and preCICE components in
`pre_c_sumo::c_sumo` and `pre_c_sumo::precice`, respectively. Conversions
between them remain in `pre_c_sumo`. Shared helpers and I/O error types belong
to `pre_c_sumo::utils`, with file operations in `pre_c_sumo::utils::file_io`
and error types in `pre_c_sumo::utils::io_errors`. Helpers used only within one
source file use anonymous namespaces inside their owning project namespace;
shared interfaces are declared in headers.
The C-SUMO and preCICE headers, sources and tests are grouped in corresponding
`csumo/` and `precice/` directories.

- [csumo/data.hpp](../include/csumo/data.hpp) defines C-SUMO settings, FF2NF input,
  and NF2FF result records. Settings XML passed through to FF2NF is stored as owned text.
- [precice/data.hpp](../include/precice/data.hpp) defines preCICE mesh mappings,
  interleaved buffers, quantity identifiers, and sink/source mesh registration data.
  Neither data header depends on the preCICE SDK or on a reader/writer.
  Their types live in `pre_c_sumo::c_sumo` and `pre_c_sumo::precice`, respectively.
- [conversions.hpp](../include/conversions.hpp) converts received mesh
  buffers into FF2NF data, creates meshes from sampling locations, and converts NF2FF records into
  sink/source buffers. These conversions do not parse XML or communicate, and
  report invalid mappings or sample buffers as conversion errors.
- [csumo/diffuser.hpp](../include/csumo/diffuser.hpp) handles DESA geometry,
  independently of transport and XML parsing.
- [csumo/settings_reader.hpp](../include/csumo/settings_reader.hpp) and
  [csumo/nf2ff_reader.hpp](../include/csumo/nf2ff_reader.hpp) parse XML strings into data.
  [csumo/ff2nf_writer.hpp](../include/csumo/ff2nf_writer.hpp) validates and generates
  XML in memory, and writes FF2NF files using the shared file I/O helper.
- [file_io.hpp](../include/utils/file_io.hpp) reads text and writes
  already-serialized content. Writing creates parent directories and refuses to
  overwrite existing files. The existence check and creation are not atomic;
  concurrent writers to the same path are not supported.
  [csumo/file_paths.hpp](../include/csumo/file_paths.hpp)
  provides the exchange filename policy without accessing the filesystem.
- [xml_parsing.hpp](../include/utils/xml_parsing.hpp) provides shared XML parsing helpers;
  C-SUMO-specific parsing belongs to [csumo/xml.hpp](../include/csumo/xml.hpp).
- [precice/exchange.hpp](../include/precice/exchange.hpp) handles preCICE reads
  and writes. [precice/sink_source_connections.hpp](../include/precice/sink_source_connections.hpp)
  maintains aligned sink/source buffers;
  transport clears those buffers after a successful write.

The workflow in [pre_c_sumo_lib.cpp](../src/pre_c_sumo_lib.cpp) converts preCICE
data before passing C-SUMO records to [csumo/ff2nf_writer.cpp](../src/csumo/ff2nf_writer.cpp)
for XML generation and file writing. C-SUMO and preCICE components do not depend
on each other; cross-domain coordination stays at the project level. NF2FF
read/parse errors are returned rather than skipping a diffuser and shifting the
remaining mappings.
