import abc
import io
import re
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import ClassVar, Dict, Iterable, Mapping, Optional, TextIO, Tuple

from s3_path_wrangler.paths import S3Path

from src.config.credentials import Credentials
from src.config.local_paths import LocalPaths
from src.config.location import Location
from src.config.test_case_config import TestCaseConfig
from src.config.types.path_type import PathType
from src.suite.test_bench_settings import TestBenchSettings
from src.utils.logging.console_logger import ConsoleLogger
from src.utils.logging.log_level import LogLevel
from src.utils.xml_config_parser import XmlConfigParser
from tools.minio import utils


@dataclass
class TestCaseData:
    __test__: ClassVar[bool] = False

    name: str
    case_dir: Path
    reference_dir: Path
    case_prefix: S3Path
    reference_prefix: S3Path
    version: Optional[datetime] = None

    def get_default_dir_and_prefix(self, path_type: PathType) -> Tuple[Path, S3Path]:
        """Get the default local directory and remote MinIO prefix based on the `path_type`."""
        if path_type == PathType.INPUT:
            return self.case_dir, self.case_prefix
        elif path_type == PathType.REFERENCE:
            return self.reference_dir, self.reference_prefix
        else:
            raise ValueError(f"Unsupported path type: {path_type.name}")

    @classmethod
    def from_config(cls, config: TestCaseConfig, local_paths: LocalPaths) -> "TestCaseData":
        """Make `TestCaseData` out of `TestCaseConfig` and `LocalPaths`."""
        if config.path is None:
            raise ValueError("Invalid test case config: Missing path")

        reference_dir = Path(utils.to_unix_path(local_paths.reference_path))
        case_dir = Path(utils.to_unix_path(local_paths.cases_path))

        rewind_timestamp = None
        if config.path.version is not None:
            rewind_timestamp = datetime.fromisoformat(config.path.version).replace(tzinfo=timezone.utc)

        case_loc = cls.__get_remote_location(config, PathType.INPUT)
        reference_loc = cls.__get_remote_location(config, PathType.REFERENCE)

        # Create test_case data
        return TestCaseData(
            name=config.name,
            case_dir=case_dir / utils.to_unix_path(case_loc.from_path) / config.name,
            reference_dir=reference_dir / utils.to_unix_path(reference_loc.from_path) / config.name,
            case_prefix=cls.__to_s3_path(case_loc) / config.path.prefix,
            reference_prefix=cls.__to_s3_path(reference_loc) / config.path.prefix,
            version=rewind_timestamp,
        )

    @staticmethod
    def __get_remote_location(config: TestCaseConfig, path_type: PathType) -> Location:
        """Get the first location with the desired `path_type`."""
        cases = (loc for loc in config.locations if loc.type == path_type and loc.root.startswith("s3://"))
        result = next(cases, None)
        if not result:
            raise ValueError(f"Invalid test case config: Missing {path_type.name} location")
        return result

    @classmethod
    def __to_s3_path(cls, loc: Location) -> S3Path:
        """Get rid of relative path fragments ('.' and '..') and create a valid S3 path."""
        abs_path = S3Path(loc.root) / utils.to_unix_path(loc.from_path or ".")
        return S3Path.from_bucket(abs_path.bucket) / utils.resolve_relative(abs_path.key)


class TestCaseLoader(abc.ABC):
    __test__: ClassVar[bool] = False

    @abc.abstractmethod
    def get_test_cases(self, filter: Optional[str] = None) -> Iterable[TestCaseData]:
        """Get test cases with an optional filter string.

        Parameters
        ----------
        filter : Optional[str], optional
            Filter test cases on the `name` property, if set.
            The `name` matches the filter if it passes a simple `filter in name` check.

        Returns
        -------
        Iterable[TestCaseData]
            The `TestCaseData` of matching test cases.
        """


class TestCaseWriter(abc.ABC):
    """Makes updates to test bench configs."""

    __test__: ClassVar[bool] = False

    @abc.abstractmethod
    def config_updates(self, updates: Mapping[str, datetime]) -> Mapping[Path, TextIO]:
        """Generate new config files based on updates test cases.

        Parameters
        ----------
        updates : Mapping[str, datetime]
            Maps test case names to new timestamps.
            All of the test cases with a matching test case name should
            have the `version` attribute in their path elements updated
            in the config files. Several config files may be updated in
            the process because config files can 'include' other config files.

        Returns
        -------
        Mapping[Path, TextIO]
            A map from files (`Path`s) to their new content, after the
            config updates have been applied. Since config files can 'include'
            other config files, several config files may be affected.
        """


class TestBenchConfigLoader(TestCaseLoader):
    """Loads test bench configs from a test bench config XML files."""

    __test__: ClassVar[bool] = False

    def __init__(
        self,
        path: Path,
        config_parser: Optional[XmlConfigParser] = None,
        credentials: Optional[Credentials] = None,
        server_base_url: str = "s3://dsc-testbench",
    ) -> None:
        self._path = path
        self._config_parser = config_parser or XmlConfigParser()
        if credentials is None:
            credentials = Credentials()
            credentials.name = "commandline"  # Trick to make the XmlConfigParser not throw an error.
        self._credentials = credentials
        self._server_base_url = server_base_url

    def get_test_cases(self, filter: Optional[str] = None) -> Iterable[TestCaseData]:
        settings = TestBenchSettings()
        settings.server_base_url = self._server_base_url
        settings.credentials = self._credentials
        settings.override_paths = ""
        settings.config_file = str(self._path)
        logger = ConsoleLogger(LogLevel.DEBUG)
        local_paths, _, test_case_configs = self._config_parser.load(settings, logger)

        return [
            TestCaseData.from_config(config, local_paths)
            for config in test_case_configs
            if (filter or "") in config.name
        ]


class TestBenchConfigWriter(TestCaseWriter):
    """Accursed unutterable regex matching XML config writer.

    Reads XML config files line by line, in search of `<path>` elements
    inside `<testCase>` elements which need their `version` attribute to
    be updated.
    I would have preferred using proper XML parsing for this, but I couldn't
    get the `lxml` library to keep the whitespace of the existing configs intact
    while editing the `<path>` elements. Maybe this is totally possible and I
    just missed this functionality in the documentation. If this is the case
    I'd be happy to replace this code. It's only merits are that it's
    not that large, and seems to work on all of the config files we have.
    """

    __test__: ClassVar[bool] = False

    PATH_PATTERN: ClassVar[re.Pattern] = re.compile(
        r"""
            (?P<space> \s* )
            (?: <path (?: \s+ version=\"(?P<version>[-.+/:\w]+)\" \s* )? > )
            (?P<path>[-/.()+\w]+)
            </path>
        """,
        re.VERBOSE,
    )

    def __init__(self, path: Path) -> None:
        self._path = path

    def config_updates(self, updates: Mapping[str, datetime]) -> Mapping[Path, TextIO]:
        return self.__update_config(self._path, updates)

    def __update_config(self, config_path: Path, updates: Mapping[str, datetime]) -> Mapping[Path, TextIO]:
        result: Dict[Path, TextIO] = {}
        out_handle = io.StringIO()
        with open(config_path, "r") as config_handle:
            for line in config_handle:
                out_handle.write(line)  # Copy line
                if re.search(r"<testCases>", line):
                    self.__update_test_cases(config_handle, out_handle, updates)
                elif mo := re.search(r"<xi:include\s+href=\"(?P<path>[-./\\:\s\w]+)\"\s*/>", line):
                    include_path = config_path.parent / mo.group("path")
                    result.update(self.__update_config(include_path, updates))
        out_handle.seek(0)
        result[config_path] = out_handle
        return result

    def __update_test_cases(
        self,
        config_handle: TextIO,
        out_handle: TextIO,
        updates: Mapping[str, datetime],
    ) -> None:
        for line in config_handle:
            out_handle.write(line)  # Copy line
            if re.search(r"</testCases>", line):  # We're done updating the test cases.
                return

            if mo := re.search(r"<testCase(?:\s+name=[\"'](?P<name>[-.()+\s\w]+)[\"'])?", line):
                name = mo.group("name")
                if name is None:
                    line = next(config_handle)  # Skip line
                    out_handle.write(line)  # Copy line
                    mo = re.search(r"name=\"(?P<name>[-.()+\s\w]+)\"", line)
                    if mo is None:
                        continue

                new_version = updates.get(mo.group("name"))
                if new_version is not None:
                    self.__update_test_case(config_handle, out_handle, new_version)

    def __update_test_case(
        self,
        config_handle: TextIO,
        out_handle: TextIO,
        new_timestamp: datetime,
    ) -> None:
        for line in config_handle:
            if re.search(r"</testCase>", line):  # We're done updating the test case.
                out_handle.write(line)
                return

            if mo := self.PATH_PATTERN.search(line):
                space, path = mo.group("space"), mo.group("path")
                new_version = new_timestamp.isoformat().split("+")[0]
                # Universal line endings take care of converting \n characters to \r\n on windows.
                out_handle.write(f'{space}<path version="{new_version}">{path}</path>\n')
            else:
                out_handle.write(line)
