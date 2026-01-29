import difflib
import textwrap
from datetime import datetime, timezone
from pathlib import Path
from typing import List, Optional, Sequence, Tuple
from unittest.mock import Mock
from uuid import uuid4

from minio.commonconfig import Tags
from minio.datatypes import Object as MinioObject
from s3_path_wrangler.paths import S3Path

from src.config.credentials import Credentials
from src.config.file_check import FileCheck
from src.config.local_paths import LocalPaths
from src.config.location import Location
from src.config.parameter import Parameter
from src.config.test_case_config import TestCaseConfig
from src.config.test_case_path import TestCasePath
from src.config.types.file_type import FileType
from src.config.types.path_type import PathType
from src.suite.command_line_settings import CommandLineSettings
from src.utils.logging.console_logger import ConsoleLogger
from src.utils.logging.i_main_logger import IMainLogger
from src.utils.logging.log_level import LogLevel
from src.utils.minio_rewinder import Rewinder
from src.utils.xml_config_parser import XmlConfigParser
from tools.minio.config import ConfigParser, DefaultTestCaseData, TestCaseData, TestCaseIndex, TestCaseWriter
from tools.minio.minio_tool import MinioTool
from tools.minio.prompt import Prompt


def make_object(
    object_name: str,
    bucket_name: str = "my-bucket",
    version_id: Optional[str] = None,
    last_modified: Optional[datetime] = None,
    is_delete_marker: bool = False,
    etag: Optional[str] = None,
    size: Optional[int] = None,
    tags: Optional[dict[str, str]] = None,
) -> MinioObject:
    version_id = version_id or uuid4().hex
    minio_tags = None
    if tags is not None:
        minio_tags = Tags()
        minio_tags.update(tags)
    last_modified = last_modified or datetime.min.replace(tzinfo=timezone.utc)

    return MinioObject(
        bucket_name=bucket_name,
        object_name=object_name,
        version_id=version_id,
        last_modified=last_modified,
        is_delete_marker=is_delete_marker,
        etag=etag,
        size=size,
        tags=minio_tags,
    )


def make_location(
    type_: PathType,
    root: str = "s3://my-bucket",
    name: str = "",
    from_path: Optional[str] = None,
) -> Location:
    location = Location()
    location.type = type_
    location.root = root.rstrip("/") + ("/cases" if type_ == PathType.INPUT else "/references")
    location.name = name if name else type_.name
    location.from_path = from_path or "."
    return location


def make_test_case_config(
    name: str,
    prefix: str,
    root: str = "s3://my-bucket",
    version: Optional[datetime] = None,
    cases_from_path: str = ".",
    reference_from_path: str = "lnx64",
    max_run_time: float = 1500.0,
) -> TestCaseConfig:
    config = TestCaseConfig()
    config.name = name
    version_str = None if version is None else version.replace(microsecond=0).isoformat()
    config.path = TestCasePath(prefix=prefix, version=version_str)
    config.max_run_time = max_run_time
    config.locations = [
        make_location(PathType.INPUT, root=root, from_path=cases_from_path),
        make_location(PathType.REFERENCE, root=root, from_path=reference_from_path),
    ]
    return config


def make_local_paths(
    cases_path: str = "./data/cases",
    reference_path: str = "./data/references",
    engines_path: str = "./data/engines",
) -> LocalPaths:
    paths = LocalPaths()
    paths.cases_path = cases_path
    paths.reference_path = reference_path
    paths.engines_path = engines_path
    return paths


def make_test_case(
    name: str,
    case_dir: Path | None = None,
    reference_dir: Path | None = None,
    case_prefix: S3Path | None = None,
    reference_prefix: S3Path | None = None,
    version: datetime | None = None,
    max_run_time: float = 1500.0,
    file_checks: list[FileCheck] | None = None,
    default_testcase: str = "default",
) -> TestCaseData:
    case_dir = case_dir or Path("data/cases") / name
    reference_dir = reference_dir or Path("data/reference_results") / name
    case_prefix = case_prefix or S3Path("s3://my-bucket/cases") / name
    reference_prefix = reference_prefix or S3Path("s3://my-bucket/references/win64") / name
    return TestCaseData(
        name=name,
        case_dir=case_dir,
        reference_dir=reference_dir,
        case_prefix=case_prefix,
        reference_prefix=reference_prefix,
        version=version,
        max_run_time=max_run_time,
        file_checks=file_checks or [],
        default_test_case=default_testcase,
    )


def make_default_test_case(name: str, max_run_time: float = 1500.0) -> DefaultTestCaseData:
    return DefaultTestCaseData(
        name=name,
        max_run_time=max_run_time,
    )


def make_config_parser(
    settings: CommandLineSettings | None = None,
    xml_parser: XmlConfigParser | None = None,
    logger: IMainLogger | None = None,
) -> ConfigParser:
    """Create a ConfigParser with default settings and logger."""
    if settings is None:
        credentials = Credentials()
        credentials.name = "commandline"
        settings = CommandLineSettings()
        settings.server_base_url = ConfigParser.DEFAULT_SERVER_BASE_URL
        settings.credentials = credentials
        settings.override_paths = ""
    if xml_parser is None:
        xml_parser = XmlConfigParser()
    if logger is None:
        logger = ConsoleLogger(log_level=LogLevel.ERROR)
    return ConfigParser(settings=settings, xml_parser=xml_parser, logger=logger)


def make_config_content(name_version_pairs: List[Tuple[str, Optional[datetime]]]) -> str:
    """Generate simple XML config with some test cases with versioned paths."""
    content_list = []
    for name, version in name_version_pairs:
        if version is None:
            path = f"<path>{name}</path>"
        else:
            version_str = version.replace(microsecond=0).isoformat().split("+")[0]
            path = f'<path version="{version_str}">{name}</path>'

        content_list.append(
            textwrap.dedent(f"""
                <testCase name="{name}" ref="default">
                    {path}
                </testCase>
                """).strip()
        )

    header = textwrap.dedent("""
        <?xml version="1.0"?>
        <deltaresTestbench_v3>
            <testCases>
        """).lstrip()
    trailer = textwrap.dedent("""
            </testCases>
        </deltaresTestbench_v3>
        """).rstrip()

    return header + textwrap.indent("\n".join(content_list), 8 * " ") + trailer


def get_added_and_removed_lines(
    src: Sequence[str], dst: Sequence[str]
) -> Tuple[List[Tuple[int, str]], List[Tuple[int, str]]]:
    added_lines: List[Tuple[int, str]] = []
    removed_lines: List[Tuple[int, str]] = []
    differ = difflib.Differ()

    old_line_nr = 1
    new_line_nr = 1
    for diff_line in differ.compare(src, dst):
        prefix, line = diff_line[:2], diff_line[2:]

        if prefix == "- ":  # Line in old file, but not in new file.
            removed_lines.append((old_line_nr, line))
            old_line_nr += 1
        elif prefix == "+ ":  # Line not in old file, but in new file.
            added_lines.append((new_line_nr, line))
            new_line_nr += 1
        elif prefix == "? ":  # Info line, not present in old or new file, ignore.
            pass
        else:  # Line both in old file and in new file.
            old_line_nr += 1
            new_line_nr += 1

    return added_lines, removed_lines


def make_file_check(name: str, parameters: dict[str, float]) -> FileCheck:
    file_check = FileCheck()
    file_check.name = name
    file_check.type = FileType.NETCDF
    param_dict: dict[str, list[Parameter]] = {}
    for param_name, tolerance in parameters.items():
        param = Parameter()
        param.name = param_name
        param.tolerance_absolute = tolerance
        param_dict[param_name] = [param]
    file_check.parameters = param_dict
    return file_check


def make_minio_tool(
    bucket: S3Path | None = None,
    rewinder: Rewinder | None = None,
    test_case_index: TestCaseIndex | None = None,
    test_case_writer: TestCaseWriter | None = None,
    prompt: Prompt | None = None,
    tags: dict[str, str] | None = None,
    color: bool = False,
) -> MinioTool:
    """Create a MinioTool instance with optional mocked components."""
    minio_tags = None
    if tags is not None:
        minio_tags = Tags()
        minio_tags.update(tags)
    return MinioTool(
        bucket=bucket or S3Path.from_bucket("my-bucket"),
        rewinder=rewinder or Mock(spec=Rewinder),
        test_case_index=test_case_index or Mock(spec=TestCaseIndex),
        test_case_writer=test_case_writer or Mock(spec=TestCaseWriter),
        prompt=prompt or Mock(spec=Prompt),
        tags=minio_tags,
        color=color,
    )
