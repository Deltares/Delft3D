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


def _check_s3_prefix(rewinder: Rewinder, bucket: str, path: str, timestamp=None) -> str:
    """Check if objects exist at an S3 prefix. Returns 'found', 'missing', or 'error'."""
    from s3_path_wrangler.paths import S3Path

    if not path:
        return "missing"
    try:
        prefix = S3Path.from_bucket(bucket) / path
        first = next(iter(rewinder.list_objects(prefix, timestamp)), None)
        return "found" if first is not None else "missing"
    except Exception as e:
        print(f" [ERROR checking {path}: {e}]", end="")
        return "error"


@dataclass(frozen=True)
class DependencyKey:
    """Identifies a unique dependency by its S3 path and version."""

    s3_path: str
    version: str


@dataclass
class TestCaseDataResult:
    """Result data for a test case validation."""

    case: str = ""
    reference: str = ""
    is_in_tc_csv: bool = False


def rename_dependency_path_for_dvc(cases_path: str, version_suffix: str = "") -> str:
    """Rename dependency folder to DVC convention by appending a version suffix if needed."""
    parts = cases_path.strip("/").split("/")
    parts[-1] += version_suffix
    return "/".join(parts)


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
    dependency: S3UrlInfo = field(default_factory=S3UrlInfo)
    dependency_version: str = ""
    dependency_s3_path: str = ""

    @property
    def has_dependency(self) -> bool:
        """Return True if this testcase has a dependency."""
        return bool(self.dependency.path)

    @property
    def dependency_key(self) -> DependencyKey:
        """Return a hashable key identifying this testcase's dependency."""
        return DependencyKey(s3_path=self.dependency_s3_path, version=self.dependency_version)

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

    def download_dependency(self, rewinder: Rewinder) -> None:
        """Download dependency data from S3 using the Rewinder."""
        if not self.has_dependency:
            return
        rewind_timestamp = None
        if self.dependency_version and self.dependency_version != "NO VERSION":
            rewind_timestamp = rewind_timestep_2_datetime(self.dependency_version)

        local_dir = self.dependency.to_local()
        print(f"Downloading dependency from {self.dependency.bucket}/{self.dependency_s3_path} to {local_dir}")
        rewinder.download(self.dependency.bucket, self.dependency_s3_path, local_dir, rewind_timestamp)

    def check_minio_presence(self, rewinder: Rewinder) -> dict[str, str]:
        """Check if case and reference data exist on MinIO without downloading.

        Returns
        -------
        dict[str, str]
            A dict with keys 'case' and 'reference', each mapped to 'found', 'missing', or 'error'.
        """
        rewind_timestamp = None
        if self.version and self.version != "NO VERSION":
            rewind_timestamp = rewind_timestep_2_datetime(self.version)

        return {
            "case": _check_s3_prefix(rewinder, self.case.bucket, self.case.path, rewind_timestamp),
            "reference": _check_s3_prefix(rewinder, self.reference.bucket, self.reference.path, rewind_timestamp),
        }

    def check_dependency_minio_presence(self, rewinder: Rewinder) -> str:
        """Check if dependency data exists on MinIO without downloading.

        Returns
        -------
        str
            'found', 'missing', or 'error'.
        """
        if not self.has_dependency:
            return "found"
        rewind_timestamp = None
        if self.dependency_version and self.dependency_version != "NO VERSION":
            rewind_timestamp = rewind_timestep_2_datetime(self.dependency_version)

        return _check_s3_prefix(rewinder, self.dependency.bucket, self.dependency_s3_path, rewind_timestamp)

    def check_doc_folder_minio_presence(self, rewinder: Rewinder) -> str | None:
        """Check if a doc folder exists on MinIO for cases matching eNN/fNN/cNN pattern.

        Returns
        -------
        str | None
            'found' or 'missing' if the case matches the doc pattern, None otherwise.
        """
        parts = self.case.path.strip("/").split("/")
        # Expected: cases/eXXX/fXXX/cXXX
        if len(parts) < 4 or parts[0].lower() != "cases":
            return None
        if not (
            parts[1].lower().startswith("e") and parts[2].lower().startswith("f") and parts[3].lower().startswith("c")
        ):
            return None

        doc_path = self.case.path.rstrip("/") + "/doc"
        return _check_s3_prefix(rewinder, self.case.bucket, doc_path)

    def add_dependency_to_dvc(self, repo: Repo) -> List[Path]:
        """Add downloaded dependency data to DVC tracking."""
        if not self.has_dependency:
            return []
        dvc_files: List[Path] = []
        dep_path = self.dependency.to_local()
        print(f"Adding dependency to DVC: {dep_path}")
        result = add_directory_to_dvc(dep_path, repo)
        if result:
            dvc_files.extend(result)
        else:
            raise RuntimeError(f"Failed to add dependency to DVC: {dep_path}")
        return dvc_files

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
        if case_doc_folder.exists():
            print(f"Adding doc folder to DVC: {case_doc_folder}")
            result = add_directory_to_dvc(case_doc_folder, repo)
            if result:
                dvc_files.extend(result)
            else:
                raise RuntimeError(f"Failed to add doc folder to DVC: {case_doc_folder}")
        else:
            print(f"No doc folder found at {case_doc_folder}, skipping.")

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

                if test_case_config.dependency:
                    dep = test_case_config.dependency
                    dep_cases_path = dep.cases_path.strip()
                    dvc_cases_path = rename_dependency_path_for_dvc(dep_cases_path)

                    new_testcase_data.dependency = S3UrlInfo(
                        hostname=base_url,
                        bucket=s3_bucket,
                        path=f"cases/{dvc_cases_path}",
                    )
                    new_testcase_data.dependency_s3_path = f"cases/{dep_cases_path}"
                    new_testcase_data.dependency_version = dep.version or ""

                testcase_data.append(new_testcase_data)

        except Exception as parse_error:
            # If TestBench parser fails, fall back to basic file name extraction
            print(f"TestBench parser failed for {xml_file_path}: {parse_error}")

    except Exception as e:
        print(f"Error processing {xml_file_path}: {e}")

    return testcase_data


def is_case_with_doc_folder(directory: Path) -> bool:
    """Return True if the given directory is a 'case' folder."""
    parts = directory.parts

    # Must end with 'input' and contain at least: data/cases/eXX/fXX/cXX/input
    if len(parts) < 6 or parts[-1] != "input":
        return False

    try:
        data_idx = parts.index("data")
    except ValueError:
        return False

    if data_idx + 4 >= len(parts) or parts[data_idx + 1] != "cases":
        return False

    e_part = parts[data_idx + 2]
    f_part = parts[data_idx + 3]
    c_part = parts[data_idx + 4]

    if not (e_part.lower().startswith("e") and f_part.lower().startswith("f") and c_part.lower().startswith("c")):
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
