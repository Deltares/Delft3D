---
name: run-testbench
description: 'Run the Deltares Testbench (TestBench.py) for D-Hydro / DIMR / Delft3D FM regression tests. Use when the user asks to run the testbench, run a testbench compare, run a testcase, run a smoke test, produce reference results, list testcases, filter tests, re-run failed tests, or inspect testbench results and logs.'
argument-hint: '<list|compare|reference|inspect> [config-name] [testcase-filter]'
---

# Run Deltares Testbench

## What this skill does

Runs `test/deltares_testbench/TestBench.py` — the black-box regression test runner
used by D-Hydro / DIMR / Delft3D FM. Covers four workflows:

| Mode | Flag | Effect |
|------|------|--------|
| list-testcases | `-t` | Lists testcases matching the filter. Safe, no execution. **Use this to preview.** |
| list-filters | `-l` | Prints available filter parameter syntax (help). Rarely needed. |
| compare | `-c` | Runs testcases and compares output to reference results. Normal dev flow. |
| reference | `-r` | **Overwrites** reference results with fresh output. Destructive; ask first. |
| inspect | *(none)* | Post-hoc: read logs, failure CSVs, and result files from a previous run. |

## When to Use

- "Run the testbench" / "run a testbench case" / "run a smoke test"
- "Run a compare on config X" / "run testcase Y"
- "Which testcases match filter Z?" / "list testcases"
- "Produce/update reference results for testcase Y"
- "Why did testcase Y fail?" / "look at the testbench logs / results"
- "Re-run the failed tests from the last run"

## Preconditions (always verify first)

Run these checks before invoking `TestBench.py`. Fix issues before proceeding.

1. **Working directory must be `test/deltares_testbench/`.** All paths in configs are relative to it.
2. **Virtual environment `.venv` exists and is activated.** Created by the devcontainer's
   `post_create_command.sh`. If missing:
   ```bash
   cd test/deltares_testbench && uv venv --python=3.12 .venv && uv pip sync pip/lnx-dev-requirements.txt
   ```
3. **Binaries are installed** at `data/engines/teamcity_artifacts/lnx64/bin/`
   (this is a symlink to `build_fm-suite_release/install/` created by post-create).
   Check with:
   ```bash
   ls data/engines/teamcity_artifacts/lnx64/bin/ 2>/dev/null | head
   ```
   If empty or missing, the FM suite hasn't been built. Ask the user to run the
   "Delft3D: Build" VS Code task (Release config) before continuing. Do NOT trigger
   the build yourself — it's long and the user should decide when to run it.
4. **Config XML exists.** Configs live under `test/deltares_testbench/configs/`:
   - `configs/dimr/` — DIMR-based configs (D-Flow FM + coupled models). Most common.
   - `configs/delft3d4/` — Delft3D 4 configs.
   - `configs/dwaq_dpart/` — DELWAQ / DELPAR.
   - `configs/smoke_tests/` — quick smoke tests.
   Filenames follow a pattern like `dimr_dflowfm_3d_lnx64.xml`. Use `fd` to search:
   ```bash
   fd -t f '.*lnx64.*\.xml$' configs/
   ```

## Command anatomy

```bash
python TestBench.py <MODE> --config <path/to/config.xml> [--filter testcase=<name>[,<name2>]] [flags]
```

- **Mode** (required, mutually exclusive): `-t` (list testcases), `-l` (list filter syntax), `-c` (compare), `-r` (reference).
- **`--config`**: path to XML config (relative to `test/deltares_testbench/`).
- **`--filter`**: e.g. `testcase=e02_f017_c010_sourcesink_2D` to run one case.
  Comma-separate names for multiple cases.
- **`--parallel`**: run multiple test cases concurrently.
- **`--skip-download`**: `cases`, `references`, `dependency`, or `all`. Useful if local
  test data / references already present and you want to avoid S3 downloads.
- **`--skip-run`**: skip the actual program run (useful for testing config parsing).
- **`--skip-post-processing`**: skip result comparison / post-processing.
- **`--copy-failed-cases`**: copy input+output of failures aside for inspection.
- **`--filter-tc-csv <path>`**: use a previous run's failure CSV to re-run only failed tests.
- **`--log-level`**: `DEBUG`, `INFO`, `WARNING`, `ERROR`, `CRITICAL`.

## Procedure

### 1. Determine mode from user's request
- Wants a preview / dry-run / "which tests match" → `-t` (testcase list). **Not `-l`** — that prints filter syntax help.
- Wants to actually run + compare → `-c` (compare)
- Wants to (re)generate reference results → `-r` (reference). **STOP AND CONFIRM
  with the user first** — this overwrites reference data.
- Wants to understand past results → `inspect` (skip to Inspecting Results below)

If the user gave just a testcase name with no mode, default to `-t` first to verify
the match, then ask whether they want a compare run.

### 2. Choose config file
If the user names a config, use it. Otherwise infer from context:
- Mentions D-Flow FM 3D → `configs/dimr/dimr_dflowfm_3d_lnx64.xml`
- Mentions D-Flow FM 1D → `configs/dimr/dimr_dflowfm_1d_lnx64.xml`
- Mentions DELWAQ → look under `configs/dwaq_dpart/`
- Smoke test → `configs/smoke_tests/apptainer_lnx64.xml`
- Prefer `*_lnx64.xml` files (Windows configs won't run in the devcontainer).

If ambiguous, ask which config or use `-l` first to preview.

### 3. Build the filter (optional but recommended)
Full runs can take a very long time. Prefer running one or a handful of testcases.
Use `-t` to list matching testcases without executing anything:
```bash
python TestBench.py -t --config configs/dimr/dimr_dflowfm_3d_lnx64.xml \
  --filter testcase=<partial_or_exact_name> --skip-download all
```
Then use `--filter testcase=<exact_name>` in the actual `-c` run.

Filter syntax (from `TestBench.py -l`):
`--filter "program=<names>:testcase=<names>:maxruntime=<expr>:startat=<name>"`
(colon between categories, comma between names within a category).

### 4. Execute
- Always run from `test/deltares_testbench/` with the venv activated.
- For long runs, prefer async terminal mode so the user isn't blocked.
- Example commands:
  ```bash
  # List testcases in a config (add --filter testcase=... to narrow)
  python TestBench.py -t --config configs/dimr/dimr_dflowfm_3d_lnx64.xml --skip-download all

  # Compare-run a specific testcase
  python TestBench.py -c --config configs/dimr/dimr_dflowfm_3d_lnx64.xml \
    --filter testcase=e02_f017_c010_sourcesink_2D --copy-failed-cases

  # Re-run only tests that failed in a previous CSV
  python TestBench.py -c --config configs/dimr/dimr_dflowfm_3d_lnx64.xml \
    --filter-tc-csv data/Test_auto-generated_fm_3d_all-testbench_*-tests.csv

  # Reference run (ONLY after user confirms — this overwrites references)
  python TestBench.py -r --config configs/dimr/dimr_dflowfm_3d_lnx64.xml \
    --filter testcase=<name>
  ```

### 5. Interpret exit code
- Exit 0 = all cases passed the comparison.
- Non-zero = at least one case failed. Proceed to "Inspecting Results".

## Inspecting Results

After a run, artifacts are located here (relative to `test/deltares_testbench/`):

| Path | What's there |
|------|--------------|
| `data/cases/<name>/` | Input files (as downloaded from S3 or symlinked). |
| `data/cases/<name>_work/` | Working dir with the run's output files. |
| `data/reference_results/lnx64/<name>/` | Expected reference output for Linux runs. |
| `data/reference_results/win64/<name>/` | Reference output produced on Windows. |
| `logs/` | Testbench top-level logs (if present). |
| `data/*tests.csv` | Per-run failure summary CSV — feed back into `--filter-tc-csv`. |

Useful inspection commands (prefer these one-liners over writing scripts):

```bash
# Peek a NetCDF output file's header
ncdump -h data/cases/<name>_work/output/<file>.nc

# Diff a specific variable's dump against the reference
diff <(ncdump -v <var> data/cases/<name>_work/output/<file>.nc) \
     <(ncdump -v <var> data/reference_results/lnx64/<name>/<file>.nc)

# Find all failed testcases in the most recent CSV
fd -e csv 'tests\.csv$' data/ | head -1 \
  | xargs -I{} awk -F, 'NR>1 && $NF=="Failure" {print $1}' {}

# Search all test logs for a specific error
rg -i 'error|failed|abort' data/cases/<name>_work/
```

## Safety Notes

- **Reference mode (`-r`) is destructive** — it overwrites reference results that
  other developers rely on. Always confirm with the user, and confirm which
  testcase(s) will be regenerated.
- **Full-config runs are long** (can be many hours). Always prefer running with
  `--filter testcase=...` unless the user explicitly asked for a full run.
- **Don't build the FM suite from this skill.** If binaries are missing, tell the
  user which task to run and stop.

## Related tools (available on PATH — use them for inspection)

`jq`, `xq`, `xmlstarlet`, `yq`, `ncdump`, `rg`, `fd`, `bat`. See [AGENTS.md](../../../AGENTS.md).
