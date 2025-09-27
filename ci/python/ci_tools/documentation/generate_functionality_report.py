import argparse
import functools
import logging
import sys
from concurrent.futures import ProcessPoolExecutor
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
    handler = logging.StreamHandler(sys.stdout)
    if use_teamcity_formatter:
        handler.setFormatter(TeamCityFormatter())
    logger.addHandler(handler)


if __name__ == "__main__":
    tzinfo = datetime.now(timezone.utc).astimezone().tzinfo
    start_time = datetime.now(tz=tzinfo)

    print("Start: %s\n" % start_time)

    parser = argparse.ArgumentParser(description="Batch process to generate functionality documentation")
    parser.add_argument("--teamcity", action="store_true", help="Log using TeamCity service messages.")
    parser.add_argument("--max-workers", default=8, type=int, help="Number of workers to use.")
    parser.add_argument(
        "--engine-dir", type=_validate_dir, required=True, help="Path to the directory of the engine, ex. e106_dflow1d"
    )
    args = parser.parse_args()
    engine_dir: Path = args.engine_dir
    use_teamcity_formatter: bool = args.teamcity
    max_workers: int = args.max_workers

    engine_number, engine_name = engine_dir.name.split("_")
    overview_func_doc = engine_dir / "doc" / "functionalities" / f"{engine_name}_functionalities_doc.tex"
    if not overview_func_doc.is_file():
        print(f"File not found: {overview_func_doc}", file=sys.stderr)
        exit(1)

    # Auto-generate table of contents files (testcases.tex).
    toc_generator = TableOfContentsWriter.from_engine_directory(engine_dir)
    toc_generator.write_table_of_contents()

    with ProcessPoolExecutor(
        max_workers=max_workers,
        initializer=_init_worker,
        initargs=(use_teamcity_formatter,),
    ) as executor:
        latex_generator = DocumentationBuilder(logger=logging.getLogger())
        # Generate the overview functionalities document.
        executor.submit(functools.partial(latex_generator.build, overview_func_doc))

        # Generate the individual functionality documents.
        for func_doc in sorted(engine_dir.glob("f[0-9][0-9]*/doc/functionality_report.tex")):
            executor.submit(functools.partial(latex_generator.build, func_doc))

    print("\nStart: %s" % start_time)
    print("End: %s" % datetime.now(tzinfo))
    print("Done")
