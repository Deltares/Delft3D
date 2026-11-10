import argparse
import logging
import sys
from concurrent.futures import ProcessPoolExecutor, as_completed
from datetime import datetime, timezone
from pathlib import Path

from ci_tools.documentation.documentation_builder import DocumentationBuilder
from ci_tools.documentation.table_of_contents_writer import TableOfContentsWriter
from ci_tools.teamcity.log import TeamCityFormatter


def _validate_dir(path_str: str) -> Path:
    result = Path(path_str)
    if not result.is_dir():
        raise NotADirectoryError(result)
    return result


def _init_worker(use_teamcity_formatter: bool = False) -> None:
    logger = logging.getLogger()
    logger.setLevel(logging.INFO)
    # Avoid duplicate handlers when reusing a process.
    if not logger.handlers:
        handler = logging.StreamHandler(sys.stdout)
        if use_teamcity_formatter:
            handler.setFormatter(TeamCityFormatter())
        logger.addHandler(handler)


def _build_tex(tex_file: Path) -> Path:
    """Build a single tex file. Returns the path on success; raises on failure."""
    DocumentationBuilder(logger=logging.getLogger()).build(tex_file)
    return tex_file


def _collect_jobs(engine_dir: Path, overview_func_doc: Path) -> list[Path]:
    jobs = [overview_func_doc]
    jobs.extend(sorted(engine_dir.glob("f*/doc/functionality_report.tex")))
    return jobs


def _run_sequential(jobs: list[Path]) -> list[tuple[Path, BaseException]]:
    failures: list[tuple[Path, BaseException]] = []
    for tex_file in jobs:
        try:
            _build_tex(tex_file)
        except BaseException as exc:  # noqa: BLE001 - collect all job failures
            failures.append((tex_file, exc))
            print(f"[FAILED] {tex_file}: {exc}", file=sys.stderr)
    return failures


def _run_parallel(jobs: list[Path], max_workers: int, use_teamcity_formatter: bool) -> list[tuple[Path, BaseException]]:
    """Run builds in a process pool.

    Warning: concurrent pdflatex on Windows MiKTeX races on the shared FNDB
    (Windows API error 32). Prefer max_workers=1 on TeamCity agents.
    """
    failures: list[tuple[Path, BaseException]] = []
    with ProcessPoolExecutor(
        max_workers=max_workers,
        initializer=_init_worker,
        initargs=(use_teamcity_formatter,),
    ) as executor:
        future_map = {executor.submit(_build_tex, tex_file): tex_file for tex_file in jobs}
        for future in as_completed(future_map):
            tex_file = future_map[future]
            try:
                future.result()
            except BaseException as exc:  # noqa: BLE001 - collect all job failures
                failures.append((tex_file, exc))
                print(f"[FAILED] {tex_file}: {exc}", file=sys.stderr)
    return failures


if __name__ == "__main__":
    tzinfo = datetime.now(timezone.utc).astimezone().tzinfo
    start_time = datetime.now(tz=tzinfo)

    print("Start: %s\n" % start_time)

    parser = argparse.ArgumentParser(description="Batch process to generate functionality documentation")
    parser.add_argument("--teamcity", action="store_true", help="Log using TeamCity service messages.")
    parser.add_argument(
        "--max-workers",
        default=1,
        type=int,
        help=(
            "Number of parallel LaTeX builds. Default is 1. "
            "Values >1 race on shared MiKTeX user data on Windows agents."
        ),
    )
    parser.add_argument(
        "--engine-dir", type=_validate_dir, required=True, help="Path to the directory of the engine, ex. e106_dflow1d"
    )
    args = parser.parse_args()
    engine_dir: Path = args.engine_dir
    use_teamcity_formatter: bool = args.teamcity
    max_workers: int = max(1, args.max_workers)

    engine_number, engine_name = engine_dir.name.split("_", maxsplit=1)
    overview_func_doc = engine_dir / "doc" / "functionalities" / f"{engine_name}_functionalities_doc.tex"
    if not overview_func_doc.is_file():
        print(f"File not found: {overview_func_doc}", file=sys.stderr)
        raise SystemExit(1)

    # Auto-generate table of contents files (testcases.tex).
    toc_generator = TableOfContentsWriter.from_engine_directory(engine_dir)
    toc_generator.write_table_of_contents()

    jobs = _collect_jobs(engine_dir, overview_func_doc)
    print(f"Building {len(jobs)} latex document(s) with max_workers={max_workers}")

    _init_worker(use_teamcity_formatter)
    if max_workers == 1:
        failures = _run_sequential(jobs)
    else:
        failures = _run_parallel(jobs, max_workers, use_teamcity_formatter)

    print("\nStart: %s" % start_time)
    print("End: %s" % datetime.now(tzinfo))

    if failures:
        print(f"\n{len(failures)} document(s) failed:", file=sys.stderr)
        for tex_file, exc in failures:
            print(f"  - {tex_file}: {exc}", file=sys.stderr)
        raise SystemExit(1)

    print("Done")
