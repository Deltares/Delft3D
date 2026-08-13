#!/usr/bin/env python3
"""Script to migrate XML testcases from MinIO S3 storage to DVC storage."""

import argparse
import io
import sys
from logging.handlers import RotatingFileHandler
from pathlib import Path
from typing import TextIO

from dvc.repo import Repo
from dvc.scm import NoSCM

from tools.minio_dvc_migration.dvc_utils import find_dvc_root_in_parent_directories, push_dvc_files_to_remote
from tools.minio_dvc_migration.s3_client import setup_minio_rewinder
from tools.minio_dvc_migration.tc_xml_utils import load_teamcity_xml_files
from tools.minio_dvc_migration.testcase_data import extract_testcase_data
from tools.minio_dvc_migration.xml_file_with_testcase_data import (
    XmlFileWithTestCaseData,
    apply_dependency_version_map,
    build_dependency_version_map,
    filter_cases_to_migrate,
)

BASE_URL = "https://s3.deltares.nl"
S3_BUCKET = "dsc-testbench"
# Path to the TeamCity CSV relative to the DVC repository root (.dvc folder).
# The DVC root is derived from this file's location (via `__file__`)
TEAMCITY_CSV_RELATIVE_PATH = Path("ci") / "teamcity" / "Delft3D" / "vars" / "dimr_testbench_table.csv"
LOG_FILE = Path("migrate_xmls.log")


class TeeStream(io.TextIOBase):
    """Write to multiple streams simultaneously (e.g. stdout + log file)."""

    def __init__(self, *streams: TextIO) -> None:
        self.streams = streams

    def write(self, data: str) -> int:
        for stream in self.streams:
            stream.write(data)
            stream.flush()
        return len(data)

    def flush(self) -> None:
        for stream in self.streams:
            stream.flush()


def parse_arguments() -> argparse.Namespace:
    """Parse command line arguments.

    Returns
    -------
    argparse.Namespace
        Parsed command line arguments.
    """
    parser = argparse.ArgumentParser(
        description="XML analysis tool for extracting testcase data",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""Examples:
  %(prog)s                                    # Use default CSV file for XML list
  %(prog)s --xmls file1.xml,file2.xml        # Process specific XML files
  %(prog)s --xmls "*.xml"                    # Process XML files matching pattern""",
    )
    parser.add_argument(
        "--xmls", type=str, help="Comma-separated list of XML file paths to process (overrides CSV parsing)"
    )
    parser.add_argument(
        "--dry-run", action="store_true", help="Check if MinIO files exist without downloading or pushing to DVC"
    )
    return parser.parse_args()


def determine_xml_files_to_process(args: argparse.Namespace) -> list[Path]:
    """
    Get list of XML files to process.

    Based on command-line-arguments, determine which XML files to process. Either from the CSV or from the command line.
    """
    if args.xmls:
        print("Loading TeamCity XML list from command line arguments...")
        xml_files = [xml.strip() for xml in args.xmls.split(",")]
        xml_files = [Path(xml).resolve() if not Path(xml).is_absolute() else Path(xml) for xml in xml_files]
    else:
        # Use default CSV parsing
        dvc_repo_root = find_dvc_root_in_parent_directories(Path(__file__).resolve())
        teamcity_csv_path = dvc_repo_root / TEAMCITY_CSV_RELATIVE_PATH
        print("Loading TeamCity XML list from CSV...")
        xml_files = load_teamcity_xml_files(str(teamcity_csv_path))
    print(f"Found {len(xml_files)} XML files")
    return xml_files


def extract_data_from_xml_files(xml_files: list[Path]) -> list[XmlFileWithTestCaseData]:
    """Loop over xml files and parse testcase data."""
    print("Extracting testcase data from XML files...")
    parsed_xmls = []
    for i, xml_file in enumerate(xml_files, start=1):
        testcases = extract_testcase_data(xml_file, BASE_URL, S3_BUCKET)
        parsed_xmls.append(XmlFileWithTestCaseData(xml_file=xml_file, testcases=testcases))
        print(f"Processed {i}/{len(xml_files)}: {Path(xml_file).name}, found {len(testcases)} testcases")
    return parsed_xmls


def run_migration(xmls_to_migrate: list[XmlFileWithTestCaseData], dry_run: bool = False) -> None:
    """Execute the migration workflow.

    When dry_run is True, only checks MinIO for file existence without
    downloading, adding to DVC, pushing, or rewriting XMLs.
    """
    rewinder = setup_minio_rewinder(BASE_URL)

    # Always validate DVC repo and remote configuration
    repo_root = find_dvc_root_in_parent_directories(Path(__file__).resolve())
    print(f"Found DVC repo at: {repo_root}")

    config_path = repo_root / ".dvc" / "config"
    if not config_path.exists():
        raise FileNotFoundError(f"DVC config not found: {config_path}")
    config_text = config_path.read_text()
    if 'remote "storage"' not in config_text:
        raise ValueError("DVC remote 'storage' is not configured in .dvc/config")
    print("DVC remote 'storage' is configured")

    repo = Repo(str(repo_root), scm=NoSCM())
    print("DVC repository initialized successfully")

    if not dry_run:
        version_map = build_dependency_version_map(xmls_to_migrate)
        if version_map:
            print(f"Dependency version conflicts detected; applying suffixes for {len(version_map)} entries")
            apply_dependency_version_map(xmls_to_migrate, version_map)

    total_found = 0
    total_missing = 0
    total_errors = 0
    total_doc_moves = 0
    total_skipped = 0
    failed_xmls: list[str] = []

    for i, xml_file in enumerate(xmls_to_migrate, start=1):
        print(f"\n[{i}/{len(xmls_to_migrate)}] {xml_file.xml_file.name} ({len(xml_file.testcases)} testcases)")

        if dry_run:
            counts = xml_file.check_minio_presence(rewinder)
            total_found += counts["found"]
            total_missing += counts["missing"]
            total_errors += counts["errors"]
            total_doc_moves += counts["doc_moves"]
        else:
            try:
                skipped = xml_file.verify_and_filter_testcases(rewinder)
                total_skipped += len(skipped)
                if not xml_file.testcases:
                    print("  WARNING: All testcases missing on MinIO, skipping XML entirely")
                    continue

                xml_file.download_from_minio_in_new_folder_structure(rewinder=rewinder)
                xml_file.move_testcases_doc_folder_to_parent()
                dvc_files = []
                dvc_files.extend(xml_file.add_to_dvc(repo=repo))
                push_dvc_files_to_remote(repo, dvc_files)
                xml_file.migrate_xml_to_dvc()
            except Exception as e:
                print(f"  WARNING: ERROR migrating {xml_file.xml_file.name}: {e}")
                failed_xmls.append(xml_file.xml_file.name)

    if dry_run:
        print(f"\n{'=' * 60}")
        print(f"Dry run summary: {total_found} found, {total_missing} missing, {total_errors} errors")
        print(f"  Doc folder moves: {total_doc_moves}")
        print(f"{'=' * 60}")
    else:
        print(f"\n{'=' * 60}")
        print(f"Migration summary: {len(xmls_to_migrate)} XMLs processed")
        if total_skipped:
            print(f"  Skipped testcases (missing on MinIO): {total_skipped}")
        if failed_xmls:
            print(f"  Failed XMLs ({len(failed_xmls)}):")
            for name in failed_xmls:
                print(f"    - {name}")
        print(f"{'=' * 60}")


def _rotate_log_file(log_path: Path) -> None:
    """Rotate existing log file using the same pattern as testbench (RotatingFileHandler)."""
    if log_path.is_file():
        handler = RotatingFileHandler(str(log_path), backupCount=10)
        handler.doRollover()
        handler.close()


def main() -> None:
    """Execute main functionality for the minio to DVC migration tool."""
    _rotate_log_file(LOG_FILE)
    with open(LOG_FILE, "w", encoding="utf-8") as log_file:
        original_stdout = sys.__stdout__ or sys.stdout
        sys.stdout = TeeStream(original_stdout, log_file)
        try:
            print(f"\n{'=' * 60}")
            print("Migration started")
            print(f"{'=' * 60}")

            args = parse_arguments()

            xml_files = determine_xml_files_to_process(args)
            xml_files_with_all_testcases = extract_data_from_xml_files(xml_files)

            xml_files_with_testcases_to_migrate = filter_cases_to_migrate(xml_files_with_all_testcases)

            run_migration(xml_files_with_testcases_to_migrate, dry_run=args.dry_run)
        finally:
            sys.stdout = original_stdout


if __name__ == "__main__":
    main()
