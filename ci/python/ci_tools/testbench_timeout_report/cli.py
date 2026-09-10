"""Command line entry point for the TestBench timeout report."""

from __future__ import annotations

import argparse
import logging
import os
from pathlib import Path

from ci_tools.teamcity.client import TeamcityClient
from ci_tools.testbench_timeout_report.aggregate import CaseStats, aggregate_case_stats
from ci_tools.testbench_timeout_report.catalog import CaseTimeout, load_case_timeouts
from ci_tools.testbench_timeout_report.plot import plot_ratio_boxplots
from ci_tools.testbench_timeout_report.report import UnresolvedNames, write_csv, write_email_html, write_html_report
from ci_tools.testbench_timeout_report.teamcity_history import (
    PLATFORM_BUILD_TYPES,
    fetch_case_histories,
    list_test_names,
)

LOGGER = logging.getLogger(__name__)


def create_parser() -> argparse.ArgumentParser:
    """Build the command line parser.

    Returns
    -------
    argparse.ArgumentParser
        Parser for the report command.
    """
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
    parser.add_argument(
        "--max-cases",
        type=int,
        default=0,
        help="Cap catalog cases per platform for a shorter run. 0 means no cap.",
    )
    parser.add_argument(
        "--verify",
        action=argparse.BooleanOptionalAction,
        default=True,
        help="Verify TLS certificates.",
    )
    return parser


def build_client(arguments: argparse.Namespace) -> TeamcityClient:
    """Create a TeamCity client from CLI arguments and environment.

    Parameters
    ----------
    arguments : argparse.Namespace
        Parsed arguments.

    Returns
    -------
    TeamcityClient
        Authenticated client.

    Raises
    ------
    ValueError
        If neither a token nor username/password is available.
    """
    token = arguments.token or os.environ.get("TEAMCITY_TOKEN")
    if token:
        return TeamcityClient.with_bearer_token_auth(
            token=token, server=arguments.server, verify=arguments.verify, timeout=60.0
        )

    username = arguments.username or os.environ.get("TEAMCITY_USERNAME")
    password = arguments.password or os.environ.get("TEAMCITY_PASSWORD")
    if username and password:
        return TeamcityClient.with_basic_auth(
            username=username,
            password=password,
            server=arguments.server,
            verify=arguments.verify,
        )
    raise ValueError("Provide --token or --username/--password (or TEAMCITY_TOKEN / TEAMCITY_USERNAME+PASSWORD).")


def _limit_catalog(catalog: list[CaseTimeout], max_cases: int) -> list[CaseTimeout]:
    """Optionally keep only the first N cases per platform.

    Parameters
    ----------
    catalog : list[CaseTimeout]
        Full XML catalog.
    max_cases : int
        Per-platform cap. ``0`` means no cap.

    Returns
    -------
    list[CaseTimeout]
        Possibly truncated catalog.
    """
    if max_cases <= 0:
        return catalog
    counts: dict[str, int] = {}
    limited: list[CaseTimeout] = []
    for case in catalog:
        used = counts.get(case.platform, 0)
        if used >= max_cases:
            continue
        counts[case.platform] = used + 1
        limited.append(case)
    LOGGER.info("Limited catalog to %s cases (max %s per platform)", len(limited), max_cases)
    return limited


def unresolved_names(
    catalog: list[CaseTimeout],
    teamcity_names: dict[str, set[str]],
) -> dict[str, UnresolvedNames]:
    """Compute catalog-only and TeamCity-only names per platform.

    Parameters
    ----------
    catalog : list[CaseTimeout]
        XML catalog.
    teamcity_names : dict[str, set[str]]
        TeamCity test names per platform.

    Returns
    -------
    dict[str, UnresolvedNames]
        Unresolved names per platform.
    """
    result: dict[str, UnresolvedNames] = {}
    for platform in PLATFORM_BUILD_TYPES:
        catalog_names = {case.name for case in catalog if case.platform == platform}
        known = teamcity_names.get(platform, set())
        result[platform] = UnresolvedNames(
            catalog_only=tuple(sorted(catalog_names - known)),
            teamcity_only=tuple(sorted(known - catalog_names)),
        )
    return result


def generate_report(
    stats: list[CaseStats],
    unresolved: dict[str, UnresolvedNames],
    output_dir: Path,
    top_n: int,
    report_url: str,
) -> None:
    """Write PNG, HTML, CSV, and email artifacts.

    Parameters
    ----------
    stats : list[CaseStats]
        Aggregated statistics.
    unresolved : dict[str, UnresolvedNames]
        Unresolved names per platform.
    output_dir : Path
        Artifact directory.
    top_n : int
        Cases per boxplot.
    report_url : str
        TeamCity build URL.
    """
    output_dir.mkdir(parents=True, exist_ok=True)
    plot_names: dict[str, str] = {}
    for platform in PLATFORM_BUILD_TYPES:
        filename = f"{platform}.png"
        platform_stats = [item for item in stats if item.platform == platform]
        plot_ratio_boxplots(
            platform_stats,
            output_dir / filename,
            title=f"{platform} duration / maxRunTime (top {top_n})",
            top_n=top_n,
        )
        plot_names[platform] = filename

    write_csv(stats, output_dir / "cases.csv")
    write_html_report(
        stats=stats,
        unresolved=unresolved,
        plot_names=plot_names,
        path=output_dir / "report.html",
        report_url=report_url,
    )
    write_email_html(stats=stats, path=output_dir / "email.html", report_url=report_url)


def run(arguments: argparse.Namespace) -> int:
    """Run the report pipeline.

    Parameters
    ----------
    arguments : argparse.Namespace
        Parsed arguments.

    Returns
    -------
    int
        Process exit code.
    """
    catalog = load_case_timeouts(arguments.csv, arguments.configs_root)
    LOGGER.info("Loaded %s catalog cases", len(catalog))
    catalog = _limit_catalog(catalog, arguments.max_cases)
    client = build_client(arguments)

    teamcity_names: dict[str, set[str]] = {}
    fetch_pairs: list[tuple[str, str]] = []
    for platform, build_type_id in PLATFORM_BUILD_TYPES.items():
        names = list_test_names(client, build_type_id)
        teamcity_names[platform] = names
        LOGGER.info("TeamCity %s has %s tests in recent builds", platform, len(names))
        for case in catalog:
            if case.platform == platform:
                fetch_pairs.append((platform, case.name))

    histories = fetch_case_histories(client, fetch_pairs, last_n=arguments.last_n)
    stats = aggregate_case_stats(catalog, histories)
    unresolved = unresolved_names(catalog, teamcity_names)
    generate_report(
        stats=stats,
        unresolved=unresolved,
        output_dir=arguments.output_dir,
        top_n=arguments.top_n,
        report_url=arguments.report_url,
    )
    LOGGER.info("Wrote report to %s", arguments.output_dir)
    return 0


def main(argv: list[str] | None = None) -> int:
    """CLI entry point.

    Parameters
    ----------
    argv : list[str] | None, optional
        Command line arguments.

    Returns
    -------
    int
        Process exit code.
    """
    logging.basicConfig(level=logging.INFO, format="%(levelname)s %(message)s")
    logging.getLogger("httpx").setLevel(logging.WARNING)
    logging.getLogger("httpcore").setLevel(logging.WARNING)
    return run(create_parser().parse_args(argv))
