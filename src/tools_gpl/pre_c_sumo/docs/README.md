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
