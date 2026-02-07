#!/usr/bin/env python3
"""Script to migrate XML testcases from MinIO S3 storage to DVC storage."""

import argparse
from pathlib import Path

from dvc.repo import Repo

from tools.minio_dvc_migration.dvc_utils import find_dvc_root_in_parent_directories, push_dvc_files_to_remote
from tools.minio_dvc_migration.s3_client import setup_minio_rewinder
from tools.minio_dvc_migration.tc_xml_utils import load_teamcity_xml_files
from tools.minio_dvc_migration.testcase_data import extract_testcase_data
from tools.minio_dvc_migration.xml_file_with_testcase_data import XmlFileWithTestCaseData, filter_cases_to_migrate

BASE_URL = "https://s3.deltares.nl"
S3_BUCKET = "dsc-testbench"
# Path to the TeamCity CSV relative to the DVC repository root (.dvc folder).
# The DVC root is derived from this file's location (via `__file__`)
TEAMCITY_CSV_RELATIVE_PATH = Path("ci") / "teamcity" / "Delft3D" / "vars" / "dimr_testbench_table.csv"


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


def main() -> None:
    """Execute main functionality for the minio to DVC migration tool."""
    args = parse_arguments()

    xml_files = determine_xml_files_to_process(args)
    xml_files_with_all_testcases = extract_data_from_xml_files(xml_files)

    xml_files_with_testcases_to_migrate = filter_cases_to_migrate(xml_files_with_all_testcases)

    rewinder = setup_minio_rewinder(BASE_URL)

    repo_root = find_dvc_root_in_parent_directories(Path(__file__).resolve())
    print(f"Found existing DVC repo at: {repo_root}")
    repo = Repo(str(repo_root))

    # First download all cases and references then move doc folders and add to DVC. This will speed up the process.
    for xml_file in xml_files_with_testcases_to_migrate:
        xml_file.download_from_minio_in_new_folder_structure(rewinder=rewinder)

    for xml_file in xml_files_with_testcases_to_migrate:
        xml_file.move_testcases_doc_folder_to_parent()

    dvc_files = []
    for i, xml_file in enumerate(xml_files_with_testcases_to_migrate, start=1):
        print(f"Add testcases {xml_file.xml_file.name} to dvc - {i}/{len(xml_files_with_testcases_to_migrate)} xml's")
        dvc_files.extend(xml_file.add_to_dvc(repo=repo))

    push_dvc_files_to_remote(repo, dvc_files)

    for xml_file in xml_files_with_testcases_to_migrate:
        xml_file.migrate_xml_to_dvc()


if __name__ == "__main__":
    main()
