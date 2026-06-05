import argparse
import logging
import sys
from datetime import datetime, timezone
from pathlib import Path

from ci_tools.documentation.documentation_builder import DocumentationBuilder
from ci_tools.teamcity.log import TeamCityFormatter


def _validate_file(path_str: str) -> Path:
    result = Path(path_str)
    if not result.is_file():
        raise FileNotFoundError(result)
    return result


def _make_logger(use_teamcity_formatter: bool = False) -> logging.Logger:
    logger = logging.getLogger()
    logger.setLevel(logging.INFO)
    handler = logging.StreamHandler(sys.stdout)
    if use_teamcity_formatter:
        handler.setFormatter(TeamCityFormatter())
    logger.addHandler(handler)
    return logger


if __name__ == "__main__":
    tzinfo = datetime.now(timezone.utc).astimezone().tzinfo
    start_time = datetime.now(tz=tzinfo)

    print("Start: %s\n" % start_time)

    parser = argparse.ArgumentParser(description="Batch process to generate validation document")
    parser.add_argument("--teamcity", action="store_true")
    parser.add_argument(
        "-t",
        "--tex-file",
        type=_validate_file,
        required=True,
        help="Name of the tex-file to generate a document from",
    )
    args = parser.parse_args()
    use_teamcity_formatter: bool = args.teamcity
    tex_file: Path = args.tex_file

    # Generate the validation document
    latex_generator = DocumentationBuilder(logger=_make_logger(use_teamcity_formatter))
    latex_generator.build(tex_file)

    print("\nStart: %s" % start_time)
    print("End: %s" % datetime.now(tzinfo))
    print("Done")
