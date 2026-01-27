"""XML parsing functionality for extracting testcase data."""

import shutil
from dataclasses import dataclass, field
from pathlib import Path
from typing import List

from dvc.repo import Repo

from src.config.types.path_type import PathType
from src.suite.command_line_settings import CommandLineSettings
from src.utils.logging.logger import Logger
from src.utils.minio_rewinder import Rewinder
from src.utils.xml_config_parser import XmlConfigParser
from tools.minio_dvc_migration.dvc_utils import add_directory_to_dvc
from tools.minio_dvc_migration.s3_url_info import S3UrlInfo, rewind_timestep_2_datetime


@dataclass
class TestCaseDataResult:
    """Result data for a test case validation."""

    case: str = ""
    reference: str = ""
    is_in_tc_csv: bool = False


@dataclass
class TestCaseData:
    """Test case data for XML parsing."""

    __test__ = False

    xml_file: str = ""
    name: str = ""
    result: TestCaseDataResult = field(default_factory=TestCaseDataResult)
    reference_platform: str = ""
    version: str = ""
    case: S3UrlInfo = field(default_factory=S3UrlInfo)
    reference: S3UrlInfo = field(default_factory=S3UrlInfo)

    def download(self, rewinder: Rewinder) -> None:
        """Download case and reference data from S3 using the Rewinder.

        Parameters
        ----------
        rewinder : Rewinder
            The Rewinder instance for S3 operations.
        """
        rewind_timestamp = None
        if self.version and self.version != "NO VERSION":
            rewind_timestamp = rewind_timestep_2_datetime(self.version)

        case_local_dir = self.case.to_local()
        reference_local_dir = self.reference.to_local()

        print(f"Downloading case from {self.case.bucket}/{self.case.path} to {case_local_dir}")
        rewinder.download(self.case.bucket, self.case.path, case_local_dir, rewind_timestamp)

        print(f"Downloading reference from {self.reference.bucket}/{self.reference.path} to {reference_local_dir}")
        rewinder.download(self.reference.bucket, self.reference.path, reference_local_dir, rewind_timestamp)

    def add_to_dvc(self, repo: Repo) -> List[Path]:
        """Add downloaded case and reference data to DVC tracking."""
        dvc_files: List[Path] = []
        case_path = self.case.to_local()
        print(f"Adding case to DVC: {case_path}")
        result = add_directory_to_dvc(case_path, repo)
        if result:
            dvc_files.extend(result)
        else:
            raise RuntimeError(f"Failed to add case to DVC: {case_path}")

        reference_path = self.reference.to_local()
        print(f"Adding reference to DVC: {reference_path}")
        result = add_directory_to_dvc(reference_path, repo)
        if result:
            dvc_files.extend(result)
        else:
            raise RuntimeError(f"Failed to add reference to DVC: {reference_path}")

        case_doc_folder = Path(case_path).parent / "doc"
        print(f"Adding doc folder to DVC: {case_doc_folder}")
        result = add_directory_to_dvc(case_doc_folder, repo)
        if result:
            dvc_files.extend(result)
        else:
            raise RuntimeError(f"Failed to add doc folder to DVC: {case_doc_folder}")

        return dvc_files


def extract_testcase_data(xml_file_path: Path, base_url: str, s3_bucket: str) -> List[TestCaseData]:
    """Extract testcase data with access to default test cases from other files using TestBench XML parser."""
    xml_file_full_path = str(xml_file_path)
    testcase_data = []

    try:
        # Create minimal settings for XML parser
        settings = CommandLineSettings()
        settings.config_file = str(xml_file_path)
        settings.server_base_url = f"{base_url}/{s3_bucket}"
        settings.credentials.name = "commandline"

        # Create logger
        logger = Logger(settings.log_level, settings.teamcity)

        # Use TestBench XML parser
        xml_parser = XmlConfigParser()

        try:
            xml_config = xml_parser.load(settings, logger)

            # Extract data from parsed test cases
            for test_case_config in xml_config.testcase_configs:
                new_testcase_data = TestCaseData(
                    xml_file=xml_file_full_path,
                    name=test_case_config.name,
                    version=test_case_config.path.version
                    if test_case_config.path and test_case_config.path.version
                    else "",
                    case=S3UrlInfo(
                        hostname=base_url,
                        bucket=s3_bucket,
                        path=f"cases/{test_case_config.path.path.strip()}" if test_case_config.path else "",
                    ),
                )

                # Look for reference location to determine platform
                for location in test_case_config.locations:
                    if hasattr(location, "type") and location.type.name == PathType.REFERENCE.name:
                        if hasattr(location, "from_path") and location.from_path:
                            new_testcase_data.reference_platform = location.from_path.strip()
                            new_testcase_data.reference = S3UrlInfo(
                                hostname=base_url,
                                bucket=s3_bucket,
                                path=f"references/{new_testcase_data.reference_platform}/{test_case_config.path.path.strip()}",
                            )
                            break
                        else:
                            print(
                                f"  Warning: Reference location without from_path in {xml_file_path} "
                                f"for testcase {test_case_config.name}"
                            )

                testcase_data.append(new_testcase_data)

        except Exception as parse_error:
            # If TestBench parser fails, fall back to basic file name extraction
            print(f"TestBench parser failed for {xml_file_path}: {parse_error}")

    except Exception as e:
        print(f"Error processing {xml_file_path}: {e}")

    return testcase_data


def is_case_with_doc_folder(directory: Path) -> bool:
    """Return True if the given directory is a 'case' folder."""
    glob_pattern = "data/cases/[eE]*/[fF]*/[cC]*/input"

    matches_pattern = directory.match(glob_pattern) or directory.match(f"**/{glob_pattern}")
    if not matches_pattern:
        return False

    if not directory.exists() or not directory.is_dir():
        return False

    doc_folder = directory / "doc"
    return doc_folder.exists() and doc_folder.is_dir()


def move_doc_folder_to_parent(directory: Path) -> Path:
    """Move doc folder to parent directory if it exists.

    Parameters
    ----------
    directory : Path
        The local directory path containing the case data.
    """
    case_doc_folder = Path(directory) / "doc"

    parent_dir = Path(directory).parent
    target_doc_folder = parent_dir / "doc"

    # If target already exists, remove it first
    if target_doc_folder.exists():
        shutil.rmtree(target_doc_folder)

    shutil.move(str(case_doc_folder), str(target_doc_folder))
    print(f"Moved doc folder from {case_doc_folder} to {target_doc_folder}")
    return target_doc_folder
