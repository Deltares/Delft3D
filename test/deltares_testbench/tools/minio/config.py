import abc
import io
import itertools
import logging
import re
import textwrap
from collections import defaultdict
from dataclasses import dataclass, field
from datetime import datetime, timezone
from pathlib import Path
from typing import ClassVar, Iterable, Mapping, Sequence, TextIO, Tuple

from s3_path_wrangler.paths import S3Path
from typing_extensions import override

from src.config.credentials import Credentials
from src.config.file_check import FileCheck
from src.config.local_paths import LocalPaths
from src.config.location import Location
from src.config.parameter import Parameter
from src.config.test_case_config import TestCaseConfig
from src.config.types.path_type import PathType
from src.suite.command_line_settings import CommandLineSettings
from src.utils.logging.console_logger import ConsoleLogger
from src.utils.logging.i_main_logger import IMainLogger
from src.utils.logging.log_level import LogLevel
from src.utils.xml_config_parser import XmlConfigParser
from tools.minio import utils
from tools.minio.error import MinioToolError


@dataclass
class DefaultTestCaseData:
    """Data of a "default" test case in the testbench config.

    In the testbench config, default test cases are used to set properties
    that are shared by many test cases. A test case can specify a 'default'
    test case and automatically inherit all of its properties.
    """

    name: str
    max_run_time: float

    @classmethod
    def from_config(cls, config: TestCaseConfig) -> "DefaultTestCaseData":
        """Convert a `TestCaseConfig` to `DefaultTestCaseData`."""
        return cls(name=config.name, max_run_time=config.max_run_time)


@dataclass
class TestCaseData:
    """Data of a test case in the testbench config."""

    __test__: ClassVar[bool] = False

    name: str
    case_dir: Path
    reference_dir: Path
    case_prefix: S3Path
    reference_prefix: S3Path
    version: datetime | None = None
    file_checks: list[FileCheck] = field(default_factory=list)
    max_run_time: float = 1500.0
    default_test_case: str | None = None

    def get_default_local_directory(self, path_type: PathType) -> Path:
        """Get the default local directory based on the `path_type`."""
        if path_type == PathType.INPUT:
            return self.case_dir
        elif path_type == PathType.REFERENCE:
            return self.reference_dir
        else:
            raise ValueError(f"Unsupported path type: {path_type.name}")

    def get_remote_prefix(self, path_type: PathType) -> S3Path:
        """Get the remote MinIO prefix based on the `path_type`."""
        if path_type == PathType.INPUT:
            return self.case_prefix
        elif path_type == PathType.REFERENCE:
            return self.reference_prefix
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
            file_checks=config.checks,
            max_run_time=config.max_run_time,
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

    def to_xml(self) -> str:
        """Convert the `TestCaseData` to an XML string."""
        if not self.version or self.version.tzinfo != timezone.utc:
            raise ValueError("Test case version must be defined and have a UTC timezone")
        version = self.version.isoformat().split("+")[0]

        case_prefix = self.case_prefix
        path = utils.remove_prefix(case_prefix, S3Path.from_bucket(case_prefix.bucket) / "cases")

        checks = "\n".join(self.__format_file_check(check) for check in self.file_checks)

        template = textwrap.dedent("""
            <testCase name="{name}" ref="{default_test_case}">
                <path version="{version}">{path}</path>
                <maxRunTime>{max_runtime}</maxRunTime>
                <checks>
                    {checks}
                </checks>
            </testCase>
            """).strip()

        return template.format(
            name=self.name,
            default_test_case=self.default_test_case,
            version=version,
            path=str(path),
            max_runtime=self.max_run_time,
            checks=self.__indent(checks, 2),
        )

    @classmethod
    def __format_file_check(cls, file_check: FileCheck) -> str:
        parameters = "\n".join(cls.__format_parameter(param[0]) for param in file_check.parameters.values())
        attributes: Iterable[tuple[str, str]] = filter(
            None,
            [
                ("name", file_check.name),
                ("type", file_check.type.name.lower()),
                ("ignore", "true") if file_check.ignore else None,
            ],
        )
        file_attrs = " ".join(f'{key}="{str(val)}"' for key, val in attributes)
        template = textwrap.dedent("""
            <file {file_attrs}>
                <parameters>
                    {parameters}
                </parameters>
            </file>
            """).strip()
        return template.format(file_attrs=file_attrs, parameters=cls.__indent(parameters, 2))

    @classmethod
    def __format_parameter(cls, param: Parameter) -> str:
        attributes: Iterable[tuple[str, str]] = filter(
            None,
            [
                ("name", param.name),
                ("toleranceAbsolute", str(param.tolerance_absolute)) if param.tolerance_absolute else None,
                ("toleranceRelative", str(param.tolerance_relative)) if param.tolerance_relative else None,
                ("ignore", "true") if param.ignore else None,
            ],
        )
        parameter_attrs = " ".join(f'{key}="{val}"' for key, val in attributes)
        return f"<parameter {parameter_attrs} />"

    @staticmethod
    def __indent(s: str, level: int, spaces_per_level: int = 4) -> str:
        spaces = level * spaces_per_level
        return s.replace("\n", "\n" + spaces * " ")


@dataclass
class TestCasePattern:
    """Pattern used to find a particular test case.

    The MinIO tool needs to look up test cases in testbench config files.
    Every test case is stored in a testbench config and should have a unique
    name within that config. The test case pattern is used to look up test
    cases in the test case index. The config "glob" pattern is used to limit
    the configs to look in. All test cases with a name that contain the
    `name_filter` as a substring should match.
    """

    __test__: ClassVar[bool] = False

    config_glob: str = "*"
    name_filter: str = ""

    @staticmethod
    def read_patterns_from_file(text_io: TextIO) -> "Iterable[TestCasePattern]":
        """Read test case patterns from a test case file.

        A test case file is a simple text file, where every line contains a
        `name_filter` and a `config_glob` separated by a comma. Lines starting
        with a `#` character are ignored.
        """
        for linenr, line in enumerate((line.strip() for line in text_io), start=1):
            if line.startswith("#"):
                continue

            name_filter, *glob_list = line.split(",", 1)
            if not name_filter:
                raise ValueError(f"Error on line {linenr}: Missing test case pattern.")

            config_glob = "" if not glob_list else glob_list[0].strip()
            yield TestCasePattern(
                config_glob=config_glob or "*",
                name_filter=name_filter.strip(),
            )


@dataclass
class TestCaseId:
    """Data class to represent a test case name."""

    __test__: ClassVar[bool] = False

    NAME_PATTERN: ClassVar[re.Pattern] = re.compile(
        r"^(?P<engine>e\d+)_(?P<feature>f\d+)_(?P<case>c\d+)(?P<trailer>[-_][-\w.]+)$"
    )

    engine_id: str
    feature_id: str
    case_id: str
    trailer: str

    def __str__(self) -> str:
        """Return the string representation of the test case name."""
        return f"{self.engine_id}_{self.feature_id}_{self.case_id}{self.trailer}"

    @property
    def identifier(self) -> str:
        """Return the identifier part of the test case name."""
        return f"{self.engine_id}_{self.feature_id}_{self.case_id}"

    @classmethod
    def from_name(cls, name: str) -> "TestCaseId":
        """Make a `TestCaseId` from a full test case name.

        Parameters
        ----------
        name : str
            The full test case name. It should match the `NAME_PATTERN`.

        Returns
        -------
        TestCaseId
        """
        match = cls.NAME_PATTERN.match(name)
        if not match:
            raise ValueError(f"Invalid test case name format: {name}")
        return TestCaseId(
            engine_id=match.group("engine"),
            feature_id=match.group("feature"),
            case_id=match.group("case"),
            trailer=match.group("trailer"),
        )


@dataclass
class ConfigData:
    """Data stored in a testbench config."""

    test_cases: Sequence[TestCaseData] = field(default_factory=list)
    default_test_cases: Sequence[DefaultTestCaseData] = field(default_factory=list)


class ConfigParser:
    """Parser for testbench configs."""

    DEFAULT_SERVER_BASE_URL: ClassVar[str] = "s3://dsc-testbench"

    def __init__(
        self,
        settings: CommandLineSettings,
        xml_parser: XmlConfigParser,
        logger: IMainLogger,
    ) -> None:
        self._settings = settings
        self._xml_parser = xml_parser
        self._logger = logger

    def parse_config(self, config: Path) -> ConfigData:
        """Read and parse a testbench config.

        Parameters
        ----------
        config : Path
            The path to the testbench config XML file.

        Returns
        -------
        ConfigData
        """
        # Quickly rule out that we're really not reading an 'include' XML file.
        if self.__first_xml_tag(config) != "deltaresTestbench_v3":
            raise ValueError("Not a testbench config")

        # Now that we're pretty sure we're reading a TestBench config file: Parse the XML.
        self._settings.config_file = str(config)
        xml_config = self._xml_parser.load(self._settings, self._logger)

        test_cases = sorted(
            (TestCaseData.from_config(config, xml_config.local_paths) for config in xml_config.testcase_configs),
            key=lambda test_case: test_case.name,
        )

        test_case_names = set(test_case.name for test_case in test_cases)
        default_test_cases = sorted(
            (
                DefaultTestCaseData.from_config(config)
                for config in self._xml_parser.default_cases
                if config.name not in test_case_names
            ),
            key=lambda test_case: test_case.name,
        )
        return ConfigData(test_cases=test_cases, default_test_cases=default_test_cases)

    @staticmethod
    def __first_xml_tag(config_path: Path) -> str | None:
        with open(config_path, "r") as file:
            matches = filter(None, (re.search(r"<\s*(?P<tag_name>[-\w]+)", line) for line in file))
            match = next(matches, None)
            return match.group("tag_name") if match else None

    @classmethod
    def with_default_settings(cls) -> "ConfigParser":
        """Make a ConfigParser with the default settings."""
        credentials = Credentials()
        credentials.name = "commandline"

        settings = CommandLineSettings()
        settings.server_base_url = cls.DEFAULT_SERVER_BASE_URL
        settings.credentials = credentials
        settings.override_paths = ""

        xml_parser = XmlConfigParser()
        logger = ConsoleLogger(LogLevel.DEBUG)

        return ConfigParser(settings=settings, xml_parser=xml_parser, logger=logger)


@dataclass
class FindTestCaseResult:
    """The result of the `find_test_case` method in the `TestCaseIndex`."""

    test_case_data: TestCaseData | None = None
    configs: Sequence[Path] = field(default_factory=list)


class TestCaseIndex:
    """An index of test cases found in a set of TestBench config files."""

    __test__: ClassVar[bool] = False

    DEFAULT_SERVER_BASE_URL: ClassVar[str] = "s3://dsc-testbench"

    def __init__(
        self,
        configs: Iterable[Path],
        config_parser: ConfigParser | None = None,
    ) -> None:
        self._index: dict[Path, ConfigData | None] = {config: None for config in configs}
        self._config_parser = config_parser or ConfigParser.with_default_settings()

    def get_config_data(self, config: Path) -> ConfigData | None:
        """Retrieve the `ConfigData` from a given `config`.

        This method will parse the config, and store the resulting `ConfigData` in
        the cache. Each config file is parsed at most once. If a config file fails
        to parse, an exception will be logged and this method returns `None`.

        Parameters
        ----------
        config : Path
            Path to the TestBench config XML file. This config file must be
            a member of the "index" set. Otherwise this method will return `None`.

        Returns
        -------
        ConfigData | None
            `None` is returned when the config can't be parsed, or is not in the index.
        """
        try:
            config_data = self._index[config]
            if config_data is not None:
                return config_data  # Return cached `ConfigData`.
        except KeyError:
            logging.warning(f"Config {config} is not in the index.")
            return None

        try:
            config_data = self._config_parser.parse_config(config)
            self._index[config] = config_data  # Store config data in the cache.
        except Exception as exc:
            logging.exception(f"Failed to parse config {config}. Message: {exc.args[0]}")
            self._index.pop(config)  # Remove config from index, to prevent parsing it again.

        return config_data

    def find_test_cases(self, test_case_patterns: Iterable[TestCasePattern]) -> Mapping[Path, Sequence[TestCaseData]]:
        """Find all of the test cases using several test case patterns.

        Parameters
        ----------
        test_case_patterns : Iterable[TestCasePattern]
            An iterable collection of test case patterns. Each pattern matching at most one test case.

        Returns
        -------
        Mapping[Path, Sequence[TestCaseData]]
            All of the found test cases, indexed by config file.
        """
        all_matching_configs: defaultdict[Path, list[TestCaseData]] = defaultdict(list)

        for pattern in test_case_patterns:
            find_result = self.find_test_case(pattern)
            found_test_case, found_configs = find_result.test_case_data, find_result.configs
            if found_test_case is None:
                continue

            for found_config in found_configs:
                other_test_cases = all_matching_configs[found_config]
                if not any(case.name == found_test_case.name for case in other_test_cases):
                    other_test_cases.append(found_test_case)

        return all_matching_configs

    def find_test_case(self, test_case_pattern: TestCasePattern) -> FindTestCaseResult:
        """Find a single test case matching the test case pattern.

        Parameters
        ----------
        test_case_pattern : TestCasePattern

        Returns
        -------
        FindTestCaseResult
            The test case data, along with all of the configs where this test case is found.
        """
        test_case_map: dict[Path, TestCaseData] = {}

        matching_configs = [config for config in self._index.keys() if config.match(test_case_pattern.config_glob)]
        for config_path in matching_configs:
            index_item = self.get_config_data(config_path)
            if index_item is None:
                continue

            cases = index_item.test_cases
            matching_cases = [case for case in cases if test_case_pattern.name_filter in case.name]
            if not matching_cases:
                continue

            matching_case, *other_matching_cases = matching_cases
            if other_matching_cases:
                suggestions = ", ".join(case.name for case in itertools.islice(matching_cases, 3))
                raise MinioToolError(
                    f"The pattern `{test_case_pattern.name_filter}` matches multiple test cases in "
                    f"config file `{config_path}`.\nSuggestions: `{suggestions}`"
                )

            test_case_map[config_path] = matching_case

        if not test_case_map:
            return FindTestCaseResult()

        (config_path, test_case), *other_items = test_case_map.items()
        for other_config, other_case in other_items:
            if self.__locations(other_case) != self.__locations(test_case):
                raise MinioToolError(
                    f"Test case `{test_case.name}` in config file `{config_path}` has different "
                    f"locations than test case `{other_case.name}` in config file `{other_config}`."
                )

        return FindTestCaseResult(test_case_data=test_case, configs=list(test_case_map.keys()))

    @staticmethod
    def __locations(case: TestCaseData) -> Tuple[Path, Path, S3Path, S3Path]:
        return (case.case_dir, case.reference_dir, case.case_prefix, case.reference_prefix)


class TestCaseWriter(abc.ABC):
    """Makes updates to test bench configs."""

    __test__: ClassVar[bool] = False

    @abc.abstractmethod
    def new_test_case(self, test_case: TestCaseData, config: Path) -> Mapping[Path, TextIO]:
        """Add a new test case to the config file.

        Parameters
        ----------
        test_case : TestCaseData
            All of the data needed to write the test case to the config.
        config : Path
            Path to the config file.

        Returns
        -------
        Mapping[Path, TextIO]
            A mapping of config files with their respective new contents.
            Since config files can include other config files, multiple
            config files may be updated in the process.
        """

    @abc.abstractmethod
    def update_versions(
        self, version_updates: Mapping[str, datetime], configs: Sequence[Path]
    ) -> Mapping[Path, TextIO]:
        """Generate new config files based on updates test cases.

        Parameters
        ----------
        version_updates : Mapping[str, datetime]
            Maps test case names to new versions.
            All of the test cases with a matching test case name should
            have the `version` attribute in their path elements updated
            in the config files. Several config files may be updated in
            the process because config files can 'include' other config files.
        configs : Sequence[Path]
            Sequence of configuration files that contain test cases that
            need to have their `version` updated..

        Returns
        -------
        Mapping[Path, TextIO]
            A map from config files to their new content, after the
            config updates have been applied. Since config files can 'include'
            other config files.
        """


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

    TEST_CASES_PATTERN: ClassVar[re.Pattern] = re.compile(
        r"""
        ^ (?P<leading_space> \s* )
        (?:
            (?P<test_cases> <testCases (?: \s+ xmlns="http://schemas\.deltares\.nl/deltaresTestbench_v3" )? > ) |
            (?P<include> <xi:include \s+ href="(?P<path>[-./\\:\s\w]+)" \s* />)
        )
        \s* $
        """,
        re.VERBOSE,
    )

    TEST_CASE_BEGIN_PATTERN: ClassVar[re.Pattern] = re.compile(
        r"^ \s* <testCase \s+ name=\"(?P<name>[-\w.]+)\" [^>]* > \s* $",
        re.VERBOSE,
    )

    TEST_CASE_END_PATTERN: ClassVar[re.Pattern] = re.compile(
        r"^ \s* </testCase> \s* $",
        re.VERBOSE,
    )

    @override
    def new_test_case(self, test_case: TestCaseData, config: Path) -> Mapping[Path, TextIO]:
        result: dict[Path, TextIO] = {}
        destination = io.StringIO()
        with config.open("r+") as source:
            while line := source.readline():
                destination.write(line)
                match_object = self.TEST_CASES_PATTERN.match(line)
                if not match_object:
                    continue

                if match_object.group("test_cases"):
                    leading_space = match_object.group("leading_space")
                    self.__search_test_case_insertion_point(test_case, source, destination)
                    self.__write_test_case(test_case, destination, leading_space)

                if match_object.group("include"):
                    # Found an include tag, we need to seek the included file.
                    path_attribute = match_object.group("path")
                    if not path_attribute:
                        raise ValueError("Include element without a path attribute found in config.")

                    # Seek the included file and copy its content.
                    include_path = Path(path_attribute)
                    if not include_path.is_absolute():  # include path is relative to config file
                        include_path = config.parent / include_path

                    updates = self.new_test_case(test_case, include_path)
                    result.update(updates)

        destination.seek(0)
        result[config] = destination
        return result

    def __search_test_case_insertion_point(self, test_case: TestCaseData, source: TextIO, destination: TextIO) -> None:
        start = source.tell()
        buffer = io.StringIO()
        while True:
            while line := source.readline():
                buffer.write(line)
                begin_match = self.TEST_CASE_BEGIN_PATTERN.match(line)
                if begin_match:
                    break
            if not begin_match:
                break  # End of file.

            test_case_name = begin_match.group("name")
            if test_case_name > test_case.name:
                source.seek(start)  # Found insertion point.
                break

            while line := source.readline():
                buffer.write(line)
                end_match = self.TEST_CASE_END_PATTERN.match(line)
                if end_match:
                    break
            if not end_match:
                break  # End of file.

            buffer.seek(0)
            destination.write(buffer.read())
            buffer = io.StringIO()
            start = source.tell()

    def __write_test_case(self, test_case: TestCaseData, destination: TextIO, leading_space: str) -> None:
        spaces = sum(4 if c == "\t" else 1 for c in leading_space)
        indents = (spaces + 3) // 4

        destination.write(self.__indent(test_case.to_xml(), indents + 1))
        destination.write("\n")

    @staticmethod
    def __indent(s: str, level: int, spaces_per_level: int = 4) -> str:
        spaces = (level * spaces_per_level) * " "
        return spaces + s.replace("\n", "\n" + spaces)

    @override
    def update_versions(
        self, version_updates: Mapping[str, datetime], configs: Sequence[Path]
    ) -> Mapping[Path, TextIO]:
        result: dict[Path, TextIO] = {}
        for file_path in configs:
            config_updates = self.__update_versions_in_config(file_path, version_updates)
            result.update(config_updates)
        return result

    def __update_versions_in_config(
        self, config_path: Path, version_updates: Mapping[str, datetime]
    ) -> Mapping[Path, TextIO]:
        result: dict[Path, TextIO] = {}
        out_handle = io.StringIO()
        with open(config_path, "r") as config_handle:
            for line in config_handle:
                out_handle.write(line)  # Copy line
                match_object = self.TEST_CASES_PATTERN.match(line)
                if not match_object:
                    continue

                if match_object.group("test_cases"):
                    self.__update_versions_in_test_cases(config_handle, out_handle, version_updates)

                if match_object.group("include"):
                    path_attribute = match_object.group("path")
                    if not path_attribute:
                        raise ValueError("Include element without a path attribute found in config.")

                    # Seek the included file and copy its content.
                    include_path = Path(path_attribute)
                    if not include_path.is_absolute():  # include path is relative to config file
                        include_path = config_path.parent / include_path

                    config_updates = self.__update_versions_in_config(include_path, version_updates)
                    result.update(config_updates)

        out_handle.seek(0)
        result[config_path] = out_handle
        return result

    def __update_versions_in_test_cases(
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
                    self.__update_version_in_test_case(config_handle, out_handle, new_version)

    def __update_version_in_test_case(
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
