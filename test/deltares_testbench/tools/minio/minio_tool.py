import difflib
import itertools
import shutil
from datetime import datetime, timedelta
from enum import Enum
from pathlib import Path
from typing import ClassVar, Dict, Iterator, List, Mapping, Optional, TextIO, Tuple

from minio.commonconfig import Tags
from s3_path_wrangler.paths import S3Path

from src.config.types.path_type import PathType
from src.utils.minio_rewinder import Operation, Plan, Rewinder, VersionPair
from tools.minio import utils
from tools.minio.config import TestBenchConfigWriter, TestCaseData
from tools.minio.prompt import Prompt


class ErrorCode(str, Enum):
    """Error code used in the `MinioToolError` base exception class."""

    AUTH = "auth"
    UNKNOWN = "unknown"


class MinioToolError(Exception):
    """Base class for errors in the `MinioTool`."""

    def __init__(self, message: str, code: ErrorCode = ErrorCode.UNKNOWN) -> None:
        super().__init__(message)
        self.code = code
        self.message = message


class MinioAuthError(MinioToolError):
    """Authentication or authorization error."""

    def __init__(self, message: str) -> None:
        super().__init__(message, ErrorCode.AUTH)


class MinioTool:
    """Upload and download objects in MinIO and update the testbench configs."""

    COLOR_SYMBOL_MAP: ClassVar[Mapping[Operation, str]] = {
        Operation.CREATE: utils.color("+", utils.Color.GREEN),
        Operation.UPDATE: utils.color("~", utils.Color.YELLOW),
        Operation.REMOVE: utils.color("-", utils.Color.RED),
    }

    SYMBOL_MAP: ClassVar[Mapping[Operation, str]] = {
        Operation.CREATE: "+",
        Operation.UPDATE: "~",
        Operation.REMOVE: "-",
    }

    def __init__(
        self,
        rewinder: Rewinder,
        test_case_writer: TestBenchConfigWriter,
        indexed_configs: Dict[Path, List[TestCaseData]],
        prompt: Prompt,
        tags: Optional[Tags] = None,
        color: bool = True,
    ) -> None:
        self._rewinder = rewinder
        self._test_case_writer = test_case_writer
        self._indexed_configs = indexed_configs
        self._prompt = prompt
        self._tags = tags
        self._color = color
        self._config_dir = Path("configs")

    def push(
        self,
        name_filter: str,
        path_type: PathType,
        config_glob: Optional[str] = None,
        local_dir: Optional[Path] = None,
        allow_create_and_delete: bool = False,
    ) -> None:
        """Upload local files to MinIO and update the timestamp in the testbench config.

        By default `push` uses case/reference data in the local
        directories configured in the test bench config file. If some other
        directory is required, it can be passed through the `local_dir` parameter.

        Parameters
        ----------
        name_filter : str
            Name of the test case to load from the config. Can be a substring
            of the actual test case name. It must match exactly one test case
            in the config.
        path_type : PathType
            If set to `PathType.INPUT`, case data is uploaded.
            If set to `PathType.REFERENCE`, reference data is uploaded.
        config_glob : Optional[str], optional
            This will limit the search and action of the indexed configurations.
            It will only include the configurations that match the filter
            to the indexed path of the configuration to run actions on.
        local_dir : Optional[Path], optional
            Path to the local directory containing files to upload to MinIO.
            If not set, use the local path from the test bench config.
        allow_create_and_delete : bool, optional
            This parameter can be used to not only update, but also allow the
            creation and removal of files in the MinIO object repository.
            Defaults to `False`.

        Raises
        ------
        MinioToolError
            Raised when the test case can not be found in the config, or unsupported
            parameters are passed.
        """
        test_case, configs = self.__get_test_case(name_filter, path_type, config_glob)
        default_dir, minio_prefix = test_case.get_default_dir_and_prefix(path_type)
        local_dir = local_dir or default_dir
        self.__print_locations(test_case, local_dir, minio_prefix)

        if test_case.version and not self.__ignore_conflicts(minio_prefix, test_case.version):
            return  # There's conflicts and the user decided to abort.

        if not self.__build_and_execute_plan(local_dir, minio_prefix, allow_create_and_delete):
            return  # No changes were made.

        # Add extra millisecond just to be safe.
        new_version = self.__max_last_modified(minio_prefix) + timedelta(milliseconds=1)

        self.__update_configs(test_case.name, new_version, configs)

    def update_references(
        self, name_filter: str, config_glob: Optional[str] = None, local_dir: Optional[Path] = None
    ) -> None:
        """Upload local files to MinIO and update the timestamp in the testbench config.

        Similar to `push`. Except that this method is always used to update the
        'references' data in MinIO, using the files in the local 'cases' directory.
        Only updated files will be uploaded to MinIO, so no files will be created
        or removed. This method supports a workflow that is often used to update
        the references:
        1. Download the cases data.
        2. Run a simulation with the latest binaries, this generates the new
           reference data
        3. Upload only new reference data.

        Parameters
        ----------
        name_filter : str
            Name of the test case to load from the config. Can be a substring
            of the actual test case name. It must match exactly one test case
            in the config.
        local_dir : Optional[Path], optional
            Path to the local directory containing files to upload to MinIO.
            If not set, use the local 'cases' path from the test bench config.
        config_glob : Optional[str], optional
            This will limit the search and action of the indexed configurations.
            It will only include the configurations that match the filter
            to the indexed path of the configuration to run actions on.

        Raises
        ------
        MinioToolError
            Raised when the test case can not be found in the config, or unsupported
            parameters are passed.
        """
        test_case, configs = self.__get_test_case(name_filter, PathType.REFERENCE, config_glob)
        local_dir = local_dir or test_case.case_dir
        minio_prefix = test_case.reference_prefix
        self.__print_locations(test_case, local_dir, minio_prefix)

        if test_case.version and not self.__ignore_conflicts(minio_prefix, test_case.version):
            return  # There's conflicts and the user decided to abort.

        if not self.__build_and_execute_plan(local_dir, minio_prefix, allow_create_and_delete=False):
            return  # No changes were made.

        # Add extra millisecond just to be safe.
        new_version = self.__max_last_modified(minio_prefix) + timedelta(milliseconds=1)

        self.__update_configs(test_case.name, new_version, configs)

    def pull(
        self,
        name_filter: str,
        path_type: PathType,
        config_glob: Optional[str] = None,
        local_dir: Optional[Path] = None,
        timestamp: Optional[datetime] = None,
        latest: bool = False,
    ) -> None:
        """Download objects from MinIO to a local directory.

        By default `pull` downloads case/reference data to the local
        directory configured in the test bench config file. The directory is
        created if it does not exist. If the test case in the config has a
        `path` element with a `version` attribute. Download the data at the
        timestamp indicated by the `version` attribute. This behavior can be
        overridden by the `timestamp` and `latest` parameters.

        Parameters
        ----------
        name_filter : str
            Name of the test case to load in the config. Can be a substring
            of the actual test case name. It must match exactly one test case
            in the config.
        path_type : PathType
            If set to `PathType.INPUT`, case data is downloaded.
            If set to `PathType.REFERENCE`, reference data is downloaded.
        config_glob : Optional[str], optional
            This will limit the search and action of the indexed configurations.
            It will only include the configurations that match the filter
            to the indexed path of the configuration to run actions on.
        local_dir : Optional[Path], optional
            Path to local directory to save the objects from MinIO in.
            This directory is created if it does not exist.
            If not set, use the local path stored in the testbench config.
        timestamp : Optional[datetime], optional
            If set, overrides `version` timestamp set in the config. Rewind
            time to this timestamp to get objects from this moment in the past.
        latest : bool, optional
            If set to `True`, ignore the `version` timestamp in the config.
            Get the latest objects from MinIO. Default value: `False`.
            This option conflicts with the `timestamp` argument. If both
            are set, `latest` takes precedence.

        Raises
        ------
        MinioToolError
            Raised when test case can't be found or if invalid arguments are passed.
        """
        test_case, _ = self.__get_test_case(name_filter, path_type, config_glob)
        default_dir, minio_prefix = test_case.get_default_dir_and_prefix(path_type)
        local_dir = local_dir or default_dir
        self.__print_locations(test_case, local_dir, minio_prefix)

        if latest:
            timestamp = None
        elif timestamp is None:  # Neither `latest` nor `timestamp` is set. Use config version.
            timestamp = test_case.version
            if timestamp and not self.__ignore_conflicts(minio_prefix, timestamp):
                return  # There's conflicts and the user decided to abort.

        self.__download(minio_prefix, local_dir, timestamp)

    def __print_locations(self, test_case: TestCaseData, local_dir: Path, minio_prefix: S3Path) -> None:
        """Print some details from the config and the used locations."""
        label_value_pairs = (
            ("Test case name:", test_case.name),
            ("Config data version:", test_case.version),
            ("Local directory:", local_dir),
            ("MinIO path:", minio_prefix),
        )
        print("".join(f"{label:24s}{value}\n" for label, value in label_value_pairs))

    def __build_and_execute_plan(
        self,
        local_dir: Path,
        minio_prefix: S3Path,
        allow_create_and_delete: bool,
    ) -> bool:
        """Compare local directory to objects in MinIO, build a plan and let user decide to execute it."""
        plan = self._rewinder.build_plan(
            src_dir=local_dir,
            dst_prefix=minio_prefix,
            tags=self._tags,
            allow_create_and_delete=allow_create_and_delete,
        )
        if not plan.items:
            print(f"Local directory `{plan.local_dir}` is already up to date with `{plan.minio_prefix}`.")
            return False

        self.__print_plan(plan)
        if not self._prompt.yes_no("Apply these changes?"):
            return False

        self._rewinder.execute_plan(plan)

        return True

    def __ignore_conflicts(self, minio_prefix: S3Path, timestamp: datetime) -> bool:
        """See if there has been any changes to the data in MinIO between `timestamp` and `now`."""
        conflicts = self._rewinder.detect_conflicts(minio_prefix, timestamp, add_tags_to_latest=True)
        if conflicts:
            self.__print_conflicts(conflicts, minio_prefix, timestamp)
            if not self._prompt.yes_no("Continue anyway?", default_yes=False):
                return False

        return True

    def __update_configs(self, test_case_name: str, new_version: datetime, configs: List[Path]) -> bool:
        """Update config with new timestamp for test case `test_case_name`."""
        new_configs = self._test_case_writer.config_updates({test_case_name: new_version}, configs)

        # Compare old configs with new configs.
        self.__print_config_diffs(new_configs)
        if not self._prompt.yes_no("Save these changes to your local config files?"):
            return False

        # Write new configs.
        self.__save_new_configs(new_configs)
        print("Applied changes to config files.")
        return True

    def __max_last_modified(self, minio_prefix: S3Path) -> datetime:
        """Compute the maximum `last_modified` of the latest objects in MinIO.

        Notes
        -----
        https://blog.min.io/strict-consistency-hard-requirement-for-primary-storage/
        This method is called right after we've uploaded new objects to MinIO. According to the
        MinIO blog, the `list_objects` operation has strong read-after-write consistency.
        This means that `list_objects` is guaranteed to return the latest versions of all
        of the objects we've just written.
        """
        latest_objects = self._rewinder.list_objects(minio_prefix, include_delete_markers=True)
        return max(obj.last_modified for obj in latest_objects if obj.last_modified is not None)

    def __download(
        self,
        minio_prefix: S3Path,
        local_dir: Path,
        rewind_timestamp: Optional[datetime] = None,
    ) -> None:
        """Download objects from MinIO to a local directory."""
        # Create directory if it doesn't exist. Ask user to continue if it is not empty.
        local_dir.mkdir(parents=True, exist_ok=True)
        if next(local_dir.iterdir(), None) is not None:  # Directory is not empty.
            if not self._prompt.yes_no(f"Directory {local_dir} is not empty. Continue anyway?"):
                return

        self._rewinder.download(minio_prefix.bucket, minio_prefix.key, local_dir, rewind_timestamp)

    def __get_test_case(
        self, test_case_name: str, path_type: PathType, config_glob: Optional[str] = None
    ) -> Tuple[TestCaseData, List[Path]]:
        """Get the TestCaseData from the first occurence of the testcase.

        The testcase needs to match the config_glob and the test_case_name filter.

        Parameters
        ----------
        test_case_name : str
            Name of the test case to load in the config. Can be a substring
            of the actual test case name. It must match exactly one test case
            in the config.
        path_type : PathType
            If set to `PathType.INPUT`, case data is downloaded.
            If set to `PathType.REFERENCE`, reference data is downloaded.
        config_glob : Optional[str], optional
            This will limit the search and action of the indexed configurations.
            It will only include the configurations that match the filter
            to the indexed path of the configuration to run actions on.

        Returns
        -------
        Tuple[TestCaseData, List[Path]]
            The first occurence of the TestCaseData matching the config and name filter
            will be returned. Together with a list of xml configur file Paths that contain
            the same testcase by name and have equal paths to the case/reference dir.
        """
        matches = {
            config: matching_cases
            for config, cases in self._indexed_configs.items()
            if (config_glob is None or config.match(config_glob))
            and (matching_cases := [c for c in cases if test_case_name in c.name])
        }

        if not matches:
            raise MinioToolError(f"The name `{test_case_name}` does not match any test cases")

        # Each config can have at most one 'matching' test case, or else an error is raised.
        if config := next(self.__configs_with_multiple_matches(matches), None):
            suggestions = ", ".join(case.name for case in itertools.islice(matches[config], 3))
            raise MinioToolError(
                f"The name `{test_case_name}` matches multiple test cases within config file `{config}`.\n"
                f"Suggestions: `{suggestions}`"
            )

        # Each config has exactly one matching test-case.
        configs: tuple[Path]
        test_cases: tuple[TestCaseData]
        configs, test_cases = zip(*((config, cases[0]) for config, cases in matches.items()))
        if len(test_cases) > 1:
            print(
                "\n".join(
                    [
                        f"Found multiple configurations containing {test_case_name}",
                        "-------------- Config List --------------",
                        "\n".join([str(config) for config in configs]),
                        "A single xml can be selected by specifying a filter that is strict enough to only match once.",
                    ]
                )
            )

            default_dir = test_cases[0].get_default_dir_and_prefix(path_type)[0]
            if not all(case.get_default_dir_and_prefix(path_type)[0] == default_dir for case in test_cases[1:]):
                raise MinioToolError("When using multiple configurations the case/reference path needs to be equal.")
            if not self._prompt.yes_no("Do you wish to continue?"):
                raise MinioToolError(
                    f"Manual abort, test case data empty because {test_case_name} is in multiple configurations."
                    + "Response to update all question was: [no]."
                )

        return test_cases[0], list(configs)

    @staticmethod
    def __configs_with_multiple_matches(matches: dict[Path, list[TestCaseData]]) -> Iterator[Path]:
        return (config for config, matching_cases in matches.items() if len(matching_cases) > 1)

    def __print_conflicts(self, conflicts: List[VersionPair], prefix: S3Path, timestamp: datetime) -> None:
        """Print information about the detected `conflicts` to the standard output."""
        symbols = self.COLOR_SYMBOL_MAP if self._color else self.SYMBOL_MAP

        print(f"Conflicts detected. The following changes have occurred since {timestamp}:")

        print(f"\n  {'File name':40s} {'Size    ':>15s} {'Last modified':30s} {'JIRA issue':20s}")
        for conflict in conflicts:
            latest = conflict.latest_version
            symbol = symbols[conflict.update_type]
            rel_key = latest.object_name[len(prefix.key) + 1 :]  # type: ignore
            size = utils.format_size(latest.size) if latest.size else ""
            modified = str((latest.last_modified + timedelta(microseconds=5e5)).replace(microsecond=0))  # type: ignore
            issue_id = latest.tags.get("jira-issue-id", "") if latest.tags else ""

            print(f"{symbol:{len(symbol)}s} {rel_key:40s} {size:>15s} {modified:30s} {issue_id:20s}")
        print()

    def __print_plan(self, plan: Plan) -> None:
        """Print the planned changes to MinIO to the standard output."""
        symbols = self.COLOR_SYMBOL_MAP if self._color else self.SYMBOL_MAP
        prefix = plan.minio_prefix.key

        print(f"The following files from `{plan.local_dir}` will be uploaded to `{plan.minio_prefix}`:")
        for item in plan.items:
            symbol = symbols[item.operation]
            rel_key = item.minio_path.key[len(prefix) + 1 :]
            size = utils.format_size(item.local_path.stat().st_size) if item.local_path else ""

            print(f"{symbol:{len(symbol)}s} {rel_key:40s} {size:>10s}")
        print()

        if plan.tags:
            print("The following tags will be attached to all changed objects:")
            print(", ".join(f"{key}={val}" for key, val in plan.tags.items()) + "\n")

    def __print_config_diffs(self, changed_configs: Mapping[Path, TextIO]) -> None:
        """Print unified diffs of all of the changed config files."""

        def color_line(line: str) -> str:
            c = {"+": utils.Color.GREEN, "-": utils.Color.RED}.get(line[0])
            return line if c is None else utils.color(line, c)

        print("Unified diff of config files:")
        for path, new_config_buffer in changed_configs.items():
            filename = str(path)
            new_config_buffer.seek(0)
            with open(path, "r") as cur_config_handle:
                diff_lines = difflib.unified_diff(
                    cur_config_handle.readlines(),
                    new_config_buffer.readlines(),
                    fromfile=filename,
                    tofile=filename,
                )

            if self._color:
                diff_lines = (color_line(line) if line else line for line in diff_lines)

            diff = "".join(diff_lines)
            if diff:
                print(diff)
        print()

    @staticmethod
    def __save_new_configs(new_configs: Mapping[Path, TextIO]) -> None:
        """Save the contents of the new config files on disk."""
        for path, buffer in new_configs.items():
            buffer.seek(0)
            with open(path, "w") as out_handle:
                shutil.copyfileobj(fsrc=buffer, fdst=out_handle)
