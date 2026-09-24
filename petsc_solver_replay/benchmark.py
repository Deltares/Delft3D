# /// script
# requires-python = ">=3.10"
# dependencies = [
#   "matplotlib>=3.8",
# ]
# ///

"""Benchmark PETSc solver configurations with petsc-solver-replay."""

from __future__ import annotations

import argparse
import csv
import json
import random
import re
import shlex
import statistics
import subprocess
import sys
import tempfile
import time
from collections.abc import Mapping, Sequence
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import TypedDict

import matplotlib
import matplotlib.pyplot as plt

matplotlib.use("Agg")


# Edit this list to select the PETSc configurations to compare. Any PETSc
# command-line option can be included in ``options``.
class SolverConfiguration(TypedDict):
    name: str
    options: list[str]


def subdomain_factor_configuration(
    name: str,
    ksp_type: str,
    pc_type: str,
    sub_pc_type: str,
    *,
    factor_levels: int | None = None,
    overlap: int | None = None,
    asm_type: str | None = None,
    ordering: str | None = None,
) -> SolverConfiguration:
    options = ["-ksp_type", ksp_type, "-pc_type", pc_type]
    if overlap is not None:
        options.extend(["-pc_asm_overlap", str(overlap)])
    if asm_type is not None:
        options.extend(["-pc_asm_type", asm_type])
    options.extend(["-sub_ksp_type", "preonly", "-sub_pc_type", sub_pc_type])
    if factor_levels is not None:
        options.extend(["-sub_pc_factor_levels", str(factor_levels)])
    if ordering is not None:
        options.extend(["-sub_pc_factor_mat_ordering_type", ordering])
    return {"name": name, "options": options}


class ReplaySummary(TypedDict):
    recorded_solves: int
    load_seconds: float
    setup_seconds: float
    solve_seconds: float
    total_iterations: int
    maximum_iterations: int
    maximum_relative_error: float


class SummaryRow(TypedDict):
    configuration: str
    successful_runs: int
    wall_mean_s: float
    wall_stdev_s: float
    load_mean_s: float
    load_stdev_s: float
    setup_mean_s: float
    setup_stdev_s: float
    solve_mean_s: float
    solve_stdev_s: float
    iterations_per_solve_mean: float
    iterations_per_solve_stdev: float
    maximum_iterations: float
    maximum_relative_error: float


SOLVER_CONFIGURATIONS: list[SolverConfiguration] = [
    {"name": "CG + block Jacobi", "options": ["-ksp_type", "cg", "-pc_type", "bjacobi"]},
    {"name": "CG + point Jacobi", "options": ["-ksp_type", "cg", "-pc_type", "jacobi"]},
    *[
        subdomain_factor_configuration(
            f"CG + block ICC({level})", "cg", "bjacobi", "icc", factor_levels=level
        )
        for level in (1, 2, 3)
    ],
    *[
        subdomain_factor_configuration(
            f"BiCGStab + block ILU({level})", "bcgs", "bjacobi", "ilu", factor_levels=level
        )
        for level in (1, 2)
    ],
    *[
        subdomain_factor_configuration(
            f"GMRES + block ILU({level})", "gmres", "bjacobi", "ilu", factor_levels=level
        )
        for level in (1, 2)
    ],
    *[
        subdomain_factor_configuration(
            f"GMRES + ASM ILU({level}), overlap 1",
            "gmres",
            "asm",
            "ilu",
            factor_levels=level,
            overlap=1,
        )
        for level in (1, 2)
    ],
]

SUMMARY_PATTERN = re.compile(
    r"recorded solves:\s*(?P<solves>\d+).*?"
    r"total load/setup/solve:\s*"
    r"(?P<load>[0-9.eE+-]+)\s*/\s*(?P<setup>[0-9.eE+-]+)\s*/\s*"
    r"(?P<solve>[0-9.eE+-]+)\s*s.*?"
    r"total iterations:\s*(?P<iterations>\d+).*?"
    r"maximum iterations:\s*(?P<maximum_iterations>\d+).*?"
    r"maximum relative reference error:\s*(?P<error>[0-9.eE+-]+)",
    re.DOTALL,
)


@dataclass
class RunResult:
    configuration: str
    run: int
    success: bool
    return_code: int
    wall_seconds: float
    recorded_solves: int | None
    load_seconds: float | None
    setup_seconds: float | None
    solve_seconds: float | None
    total_iterations: int | None
    maximum_iterations: int | None
    maximum_relative_error: float | None
    command: str
    log_file: str


def parse_arguments() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Benchmark hardcoded PETSc solver configurations using replay dumps."
    )
    parser.add_argument("--executable", type=Path, required=True, help="Path to petsc-solver-replay")
    parser.add_argument(
        "--replay-file",
        type=Path,
        required=True,
        help="Dump prefix, for example petsc_dump/flow_solve",
    )
    parser.add_argument("--runs", type=int, default=5, help="Measured runs per configuration (default: 5)")
    parser.add_argument("--mpi-processes", type=int, default=1, help="MPI ranks (default: 1)")
    parser.add_argument("--mpiexec", default="mpiexec", help="MPI launcher (default: mpiexec)")
    parser.add_argument("--node-owners", type=Path, help="Optional replay owner-ranks file")
    parser.add_argument(
        "--rebuild-preconditioner",
        type=int,
        default=1,
        help="Replay preconditioner reuse count (default: 1)",
    )
    parser.add_argument(
        "--petsc-options",
        default="",
        help='Extra options shared by all configurations, e.g. "-ksp_rtol 1e-12"',
    )
    parser.add_argument("--output", type=Path, default=Path("benchmark_results"))
    parser.add_argument("--seed", type=int, default=20260921, help="Run-order randomization seed")
    arguments = parser.parse_args()
    if arguments.runs < 1:
        parser.error("--runs must be positive")
    if arguments.mpi_processes < 1:
        parser.error("--mpi-processes must be positive")
    if arguments.rebuild_preconditioner < 0:
        parser.error("--rebuild-preconditioner must be nonnegative")
    return arguments


def parse_replay_summary(output: str) -> ReplaySummary | None:
    match = SUMMARY_PATTERN.search(output)
    if match is None:
        return None
    return {
        "recorded_solves": int(match.group("solves")),
        "load_seconds": float(match.group("load")),
        "setup_seconds": float(match.group("setup")),
        "solve_seconds": float(match.group("solve")),
        "total_iterations": int(match.group("iterations")),
        "maximum_iterations": int(match.group("maximum_iterations")),
        "maximum_relative_error": float(match.group("error")),
    }


def display_command(command: Sequence[str]) -> str:
    return subprocess.list2cmdline(command) if sys.platform == "win32" else shlex.join(command)


def run_once(command: list[str], configuration: str, run: int, log_file: Path) -> RunResult:
    with tempfile.TemporaryFile() as output_file:
        started = time.perf_counter()
        completed = subprocess.run(
            command,
            stdout=output_file,
            stderr=subprocess.STDOUT,
            stdin=subprocess.DEVNULL,
            check=False,
        )
        wall_seconds = time.perf_counter() - started
        output_file.seek(0)
        output = output_file.read().decode("utf-8", errors="replace")

    log_file.write_text(output, encoding="utf-8")
    summary = parse_replay_summary(output)
    success = completed.returncode == 0 and summary is not None
    return RunResult(
        configuration=configuration,
        run=run,
        success=success,
        return_code=completed.returncode,
        wall_seconds=wall_seconds,
        recorded_solves=summary["recorded_solves"] if summary else None,
        load_seconds=summary["load_seconds"] if summary else None,
        setup_seconds=summary["setup_seconds"] if summary else None,
        solve_seconds=summary["solve_seconds"] if summary else None,
        total_iterations=summary["total_iterations"] if summary else None,
        maximum_iterations=summary["maximum_iterations"] if summary else None,
        maximum_relative_error=summary["maximum_relative_error"] if summary else None,
        command=display_command(command),
        log_file=str(log_file),
    )


def mean_stdev(values: list[float]) -> tuple[float, float]:
    return statistics.mean(values), statistics.stdev(values) if len(values) > 1 else 0.0


def summarize(results: list[RunResult]) -> list[SummaryRow]:
    rows: list[SummaryRow] = []
    for configuration in SOLVER_CONFIGURATIONS:
        name = configuration["name"]
        selected = [result for result in results if result.configuration == name and result.success]
        if not selected:
            rows.append(
                {
                    "configuration": name,
                    "successful_runs": 0,
                    "wall_mean_s": float("nan"),
                    "wall_stdev_s": float("nan"),
                    "load_mean_s": float("nan"),
                    "load_stdev_s": float("nan"),
                    "setup_mean_s": float("nan"),
                    "setup_stdev_s": float("nan"),
                    "solve_mean_s": float("nan"),
                    "solve_stdev_s": float("nan"),
                    "iterations_per_solve_mean": float("nan"),
                    "iterations_per_solve_stdev": float("nan"),
                    "maximum_iterations": float("nan"),
                    "maximum_relative_error": float("nan"),
                }
            )
            continue
        wall_mean, wall_stdev = mean_stdev([result.wall_seconds for result in selected])
        solve_mean, solve_stdev = mean_stdev([result.solve_seconds for result in selected if result.solve_seconds is not None])
        setup_mean, setup_stdev = mean_stdev([result.setup_seconds for result in selected if result.setup_seconds is not None])
        load_mean, load_stdev = mean_stdev([result.load_seconds for result in selected if result.load_seconds is not None])
        iterations_mean, iterations_stdev = mean_stdev(
            [
                result.total_iterations / result.recorded_solves
                for result in selected
                if result.total_iterations is not None and result.recorded_solves
            ]
        )
        rows.append(
            {
                "configuration": name,
                "successful_runs": len(selected),
                "wall_mean_s": wall_mean,
                "wall_stdev_s": wall_stdev,
                "load_mean_s": load_mean,
                "load_stdev_s": load_stdev,
                "setup_mean_s": setup_mean,
                "setup_stdev_s": setup_stdev,
                "solve_mean_s": solve_mean,
                "solve_stdev_s": solve_stdev,
                "iterations_per_solve_mean": iterations_mean,
                "iterations_per_solve_stdev": iterations_stdev,
                "maximum_iterations": float(
                    max(result.maximum_iterations for result in selected if result.maximum_iterations is not None)
                ),
                "maximum_relative_error": max(
                    result.maximum_relative_error
                    for result in selected
                    if result.maximum_relative_error is not None
                ),
            }
        )
    return rows


def write_csv(path: Path, rows: Sequence[Mapping[str, object]]) -> None:
    fieldnames = list(rows[0])
    with path.open("w", newline="", encoding="utf-8") as output:
        writer = csv.DictWriter(output, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)


def write_markdown(path: Path, summary: Sequence[SummaryRow], failures: int, arguments: argparse.Namespace) -> None:
    lines = [
        "# PETSc solver replay benchmark",
        "",
        f"- Replay prefix: `{arguments.replay_file}`",
        f"- MPI processes: {arguments.mpi_processes}",
        f"- Measured runs per configuration: {arguments.runs}",
        f"- Failed measured runs: {failures}",
        "",
    ]
    table = [["Configuration", "Runs", "Wall (s)", "Setup (s)", "Solve (s)", "Iterations/solve", "Max iterations", "Max rel. error"]]
    for row in summary:
        if row["successful_runs"] == 0:
            table.append([str(row["configuration"]), "0", *(["failed"] * 6)])
            continue
        table.append([
            str(row["configuration"]),
            str(row["successful_runs"]),
            f"{row['wall_mean_s']:.4f} +/- {row['wall_stdev_s']:.4f}",
            f"{row['setup_mean_s']:.4f} +/- {row['setup_stdev_s']:.4f}",
            f"{row['solve_mean_s']:.4f} +/- {row['solve_stdev_s']:.4f}",
            f"{row['iterations_per_solve_mean']:.2f} +/- {row['iterations_per_solve_stdev']:.2f}",
            f"{row['maximum_iterations']:.0f}",
            f"{row['maximum_relative_error']:.3e}",
        ])
    widths = [max(3, *(len(cells[index]) for cells in table)) for index in range(len(table[0]))]
    lines.append("| " + " | ".join(cell.ljust(widths[index]) if index == 0 else cell.rjust(widths[index]) for index, cell in enumerate(table[0])) + " |")
    lines.append("|" + "|".join("-" * (width + 2) if index == 0 else "-" * (width + 1) + ":" for index, width in enumerate(widths)) + "|")
    for table_row in table[1:]:
        lines.append("| " + " | ".join(cell.ljust(widths[index]) if index == 0 else cell.rjust(widths[index]) for index, cell in enumerate(table_row)) + " |")
    lines.extend(
        [
            "",
            "Values are arithmetic mean +/- sample standard deviation. See `runs.csv` and `logs/` for raw results.",
            "",
        ]
    )
    path.write_text("\n".join(lines), encoding="utf-8")


def write_plot(path: Path, summary: Sequence[SummaryRow]) -> None:
    successful = [row for row in summary if row["successful_runs"]]
    if not successful:
        return
    names = [str(row["configuration"]) for row in successful]
    figure, axis = plt.subplots(figsize=(max(7, len(names) * 1.6), 5), layout="constrained")
    axis.bar(names, [row["solve_mean_s"] for row in successful], yerr=[row["solve_stdev_s"] for row in successful], capsize=4)
    axis.set_title("PETSc solve time")
    axis.set_ylabel("Seconds, mean +/- stdev")
    axis.tick_params(axis="x", rotation=25)
    axis.grid(axis="y", alpha=0.25)
    figure.savefig(path, dpi=160)
    plt.close(figure)


def build_command(arguments: argparse.Namespace, options: list[str]) -> list[str]:
    command = []
    if arguments.mpi_processes > 1:
        command.extend([arguments.mpiexec, "-n", str(arguments.mpi_processes)])
    command.extend(
        [
            str(arguments.executable.resolve()),
            "-replay_file",
            str(arguments.replay_file.resolve()),
            "-replay_rebuild_preconditioner",
            str(arguments.rebuild_preconditioner),
        ]
    )
    if arguments.node_owners:
        command.extend(["-replay_node_owners", str(arguments.node_owners.resolve())])
    command.extend(shlex.split(arguments.petsc_options, posix=sys.platform != "win32"))
    command.extend(options)
    return command


def main() -> int:
    arguments = parse_arguments()
    if not arguments.executable.is_file():
        raise SystemExit(f"Replay executable not found: {arguments.executable}")
    if not Path(f"{arguments.replay_file}_global_node_ids.bin").is_file():
        raise SystemExit(f"Replay metadata not found for prefix: {arguments.replay_file}")

    arguments.output.mkdir(parents=True, exist_ok=True)
    log_directory = arguments.output / "logs"
    log_directory.mkdir(exist_ok=True)
    jobs = [
        (configuration, run)
        for run in range(1, arguments.runs + 1)
        for configuration in SOLVER_CONFIGURATIONS
    ]
    random.Random(arguments.seed).shuffle(jobs)

    measured_results = []
    for index, (configuration, run) in enumerate(jobs, start=1):
        print(f"[{index}/{len(jobs)}] {configuration['name']}: run {run}/{arguments.runs}", flush=True)
        command = build_command(arguments, configuration["options"])
        log_name = re.sub(r"[^a-z0-9]+", "-", configuration["name"].lower()).strip("-")
        log_file = log_directory / f"{log_name}_run-{run}.log"
        result = run_once(command, configuration["name"], run, log_file)
        if not result.success:
            print(f"  failed with exit code {result.return_code}; see {log_file}", file=sys.stderr)
        measured_results.append(result)

    raw_rows = [asdict(result) for result in measured_results]
    summary = summarize(measured_results)
    write_csv(arguments.output / "runs.csv", raw_rows)
    write_csv(arguments.output / "summary.csv", summary)
    failures = sum(not result.success for result in measured_results)
    write_markdown(arguments.output / "report.md", summary, failures, arguments)
    write_plot(arguments.output / "comparison.png", summary)
    (arguments.output / "configurations.json").write_text(
        json.dumps(SOLVER_CONFIGURATIONS, indent=2) + "\n", encoding="utf-8"
    )
    print(f"\nReport: {(arguments.output / 'report.md').resolve()}")
    return 1 if failures else 0


if __name__ == "__main__":
    raise SystemExit(main())