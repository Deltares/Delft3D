# Delft3D FM Input File Specifications

The directory contains the specifications for Delft3D FM kernel input files. The main purpose of these specifications is to improve clarity on the latest file formats and transparency on changes.

**Table of contents**

1. [Supported Files](#supported-files)
2. [Changelogs](#changelogs)
3. [Repository Contents](#repository-contents)

## Supported Files

The following Delft3D FM kernel files have a specification:

- [MDU file](specifications/dflowfm/mdu.md)

## Changelogs

- [MDU changelog (2027.01 - main)](changelogs/dflowfm/2027.01_to_main_mdu_changelog.md)
- [MDU changelog (2026.02 - 2027.01)](changelogs/dflowfm/2026.02_to_2027.01_mdu_changelog.md)
- [MDU changelog (2026.01 - 2026.02)](changelogs/dflowfm/2026.01_to_2026.02_mdu_changelog.md)

## Repository Contents

| Path | Contents |
| --- | --- |
| `specifications/dflowfm/` | D-Flow FM kernel file specifications. Each file type has a JSON specification (for example, `mdu.json`) and corresponding Markdown documentation. |
| `specifications/json-schemas/` | JSON Schemas that define and validate the specification format. `ini.json.schema` is used for INI-based files such as MDU files. |
| `changelogs/dflowfm/` | JSON and Markdown changelogs describing specification changes between versions. Filenames identify the source and target versions. |
| `scripts/` | Utilities to compare specifications, generate Markdown documentation and changelogs, and validate JSON files. |
