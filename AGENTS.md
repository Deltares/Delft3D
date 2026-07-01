# Repository Guidelines for Agents

## Available CLI Tools

The devcontainer ships with these tools on `PATH`. **Prefer them over writing throwaway
Python/shell scripts** — a readable one-liner is easier for the user to review and approve.

### Structured data

- **`jq`** — query and transform JSON. Use for API responses, `compile_commands.json`,
  Conan output, and any `.json` file.
- **`xq`** — jq-style querying for XML (from the `yq` package). Prefer for read-only
  extraction from DIMR configs and other XML.
- **`xmlstarlet`** — XML editing, validation, and XPath. Use for XSD validation
  (`xmlstarlet val --xsd schema.xsd file.xml`) and in-place XML edits.
- **`yq`** — jq-style querying for YAML.
- **`tomlq`** — jq-style querying for TOML (e.g. `pyproject.toml`).

### NetCDF

- **`ncdump`** — inspect NetCDF files. Use `ncdump -h file.nc` for header/metadata,
  `ncdump -v <var> file.nc` for a specific variable. Prefer over writing Python
  with `netCDF4`/`xarray` for quick inspection.

### Search and navigation

- **`rg`** (ripgrep) — fast, gitignore-aware text search. Prefer over `grep -r` / `find … -exec grep`.
- **`fd`** — fast, gitignore-aware file finder. Prefer over `find` for name-based lookups.
- **`bat`** — `cat` with syntax highlighting and line numbers. Useful for showing file
  excerpts to the user; pass `--paging=never` in non-interactive contexts.

### Guidance

- Reach for these tools before writing a script. If a task can be done in one `jq` /
  `xmlstarlet` / `ncdump` invocation, do that instead of generating a script the user
  has to review.
- Use `rg` and `fd` for search; they respect `.gitignore` and skip build directories
  like `build_fm-suite_*/` automatically.
