"""Command line entry point for the TestBench timeout report."""

from __future__ import annotations

import argparse
import logging
import os
from pathlib import Path

import pandas as pd

from ci_tools.teamcity.client import TeamcityClient
from ci_tools.testbench_timeout_report.catalog import load_catalog
from ci_tools.testbench_timeout_report.report import summarize, write_artifacts
from ci_tools.testbench_timeout_report.teamcity import BUILD_TYPES, fetch_runs, list_test_names

LOGGER = logging.getLogger(__name__)


def create_parser() -> argparse.ArgumentParser:
    """Build the command line parser."""
    parser = argparse.ArgumentParser(description="Report TestBench case durations against XML maxRunTime.")
    parser.add_argument("--server", required=True, help="TeamCity hostname or URL.")
    parser.add_argument("--token", help="TeamCity bearer token.")
    parser.add_argument("--username", help="TeamCity username. Defaults to TEAMCITY_USERNAME.")
    parser.add_argument("--password", help="TeamCity password. Defaults to TEAMCITY_PASSWORD.")
    parser.add_argument("--last-n", type=int, default=100, help="Successful runs to keep per case.")
    parser.add_argument("--top-n", type=int, default=40, help="Cases to draw in each boxplot.")
    parser.add_argument("--output-dir", type=Path, default=Path("timeout-report"), help="Artifact directory.")
    parser.add_argument(
        "--configs-root",
        type=Path,
        default=Path("test/deltares_testbench/configs"),
        help="TestBench XML config directory.",
    )
    parser.add_argument(
        "--csv",
        type=Path,
        default=Path("ci/teamcity/Delft3D/vars/dimr_testbench_table.csv"),
        help="TestBench config CSV.",
    )
    parser.add_argument("--report-url", default="", help="TeamCity build URL to embed in the report.")
    parser.add_argument("--full-report-url", default="", help="Direct URL to the HTML report artifact.")
    parser.add_argument("--max-cases", type=int, default=0, help="Cap catalog cases per platform. 0 means no cap.")
    parser.add_argument(
        "--verify", action=argparse.BooleanOptionalAction, default=True, help="Verify TLS certificates."
    )
    return parser


def build_client(arguments: argparse.Namespace) -> TeamcityClient:
    """Create a TeamCity client from CLI arguments and environment."""
    token = arguments.token or os.environ.get("TEAMCITY_TOKEN")
    if token:
        return TeamcityClient.with_bearer_token_auth(
            token=token, server=arguments.server, verify=arguments.verify, timeout=60.0
        )
    username = arguments.username or os.environ.get("TEAMCITY_USERNAME")
    password = arguments.password or os.environ.get("TEAMCITY_PASSWORD")
    if username and password:
        return TeamcityClient.with_basic_auth(
            username=username, password=password, server=arguments.server, verify=arguments.verify
        )
    raise ValueError("Provide --token or --username/--password (or TEAMCITY_TOKEN / TEAMCITY_USERNAME+PASSWORD).")


def run(arguments: argparse.Namespace) -> int:
    """Load catalog, fetch TeamCity history, and write artifacts."""
    catalog = load_catalog(arguments.csv, arguments.configs_root)
    LOGGER.info("Loaded %s catalog cases", len(catalog))
    catalog = limit_catalog(catalog, arguments.max_cases)
    if arguments.max_cases > 0:
        LOGGER.info("Limited catalog to %s cases (max %s per platform)", len(catalog), arguments.max_cases)

    client = build_client(arguments)
    teamcity_names = {platform: list_test_names(client, build_type) for platform, build_type in BUILD_TYPES.items()}
    for platform, names in teamcity_names.items():
        LOGGER.info("TeamCity %s has %s tests in recent builds", platform, len(names))

    runs, failures = fetch_runs(client, catalog, last_n=arguments.last_n)
    stats = summarize(catalog, runs, failures)
    write_artifacts(
        stats=stats,
        runs=runs,
        catalog=catalog,
        teamcity_names=teamcity_names,
        output_dir=arguments.output_dir,
        top_n=arguments.top_n,
        report_url=arguments.report_url,
        full_report_url=arguments.full_report_url,
    )
    LOGGER.info("Wrote report to %s", arguments.output_dir)
    return 0


def main(argv: list[str] | None = None) -> int:
    """CLI entry point."""
    logging.basicConfig(level=logging.INFO, format="%(levelname)s %(message)s")
    logging.getLogger("httpx").setLevel(logging.WARNING)
    logging.getLogger("httpcore").setLevel(logging.WARNING)
    return run(create_parser().parse_args(argv))


def limit_catalog(catalog: pd.DataFrame, max_cases: int) -> pd.DataFrame:
    """Keep the first N rows per platform. ``0`` means no cap."""
    if max_cases <= 0:
        return catalog
    return catalog.groupby("platform", group_keys=False).head(max_cases)
