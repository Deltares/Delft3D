import difflib
import itertools
import shutil
from datetime import datetime, timedelta, timezone
from enum import Enum
from pathlib import Path
from typing import (
    ClassVar,
    List,
    Mapping,
    Optional,
    TextIO,
)

from minio.commonconfig import Tags
from s3_path_wrangler.paths import S3Path

from src.config.types.path_type import PathType
from src.utils.minio_rewinder import Operation, Plan, Rewinder, VersionPair
from tools.minio import utils
from tools.minio.config import (
    TestCaseData,
    TestCaseLoader,
    TestCaseWriter,
)
from tools.minio.prompt import Prompt


class ErrorCode(str, Enum):
    AUTH = "auth"
    UNKNOWN = "unknown"


class MinioToolError(Exception):
    def __init__(self, message: str, code: ErrorCode = ErrorCode.UNKNOWN) -> None:
        super().__init__(message)
        self.code = code
        self.message = message


class MinioAuthError(MinioToolError):
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
        test_case_loader: TestCaseLoader,
        test_case_writer: TestCaseWriter,
        prompt: Prompt,
        tags: Optional[Tags] = None,
        color: bool = True,
    ) -> None:
        self._rewinder = rewinder
        self._test_case_loader = test_case_loader
        self._test_case_writer = test_case_writer
        self._prompt = prompt
        self._tags = tags
        self._color = color

    def push(
        self,
        name_filter: str,
        path_type: PathType,
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
        local_dir : Optional[Path], optional
            Path to the local directory containing files to upload to MinIO.
            If not set, use the local path from the test bench config.
        allow_create_and_delete
            This parameter can be used to not only update, but also allow the
            creation and removal of files in the MinIO object repository.

        Raises
        ------
        MinioToolError
            Raised when the test case can not be found in the config, or unsupported
            parameters are passed.
        """
        test_case = self.__get_test_case(name_filter)
        default_dir, minio_prefix = test_case.get_default_dir_and_prefix(path_type)
        local_dir = local_dir or default_dir
        self.__print_locations(test_case, local_dir, minio_prefix)

        if test_case.version and not self.__ignore_conflicts(minio_prefix, test_case.version):
            return  # There's conflicts and the user decided to abort.

        if not self.__build_and_execute_plan(local_dir, minio_prefix, allow_create_and_delete):
            return  # No changes were made.

        self.__update_config(test_case.name)

    def update_references(self, name_filter: str, local_dir: Optional[Path] = None) -> None:
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

        Raises
        ------
        MinioToolError
            Raised when the test case can not be found in the config, or unsupported
            parameters are passed.
        """
        test_case = self.__get_test_case(name_filter)
        local_dir = local_dir or test_case.case_dir
        minio_prefix = test_case.reference_prefix
        self.__print_locations(test_case, local_dir, minio_prefix)

        if test_case.version and not self.__ignore_conflicts(minio_prefix, test_case.version):
            return  # There's conflicts and the user decided to abort.

        if not self.__build_and_execute_plan(local_dir, minio_prefix, allow_create_and_delete=True):
            return  # No changes were made.

        self.__update_config(test_case.name)

    def pull(
        self,
        name_filter: str,
        path_type: PathType,
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
        test_case = self.__get_test_case(name_filter)
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
        # Verify that version timestamp is in the past.
        now = datetime.now(timezone.utc)
        if now < timestamp:
            seconds = (timestamp - now).total_seconds()
            raise MinioToolError(f"Test case path has version timestamp {seconds:.2f}s in the future")

        conflicts = self._rewinder.detect_conflicts(minio_prefix, timestamp, add_tags_to_latest=True)
        if conflicts:
            self.__print_conflicts(conflicts, minio_prefix, timestamp)
            if not self._prompt.yes_no("Continue anyway?", default_yes=False):
                return False

        return True

    def __update_config(self, test_case_name: str) -> bool:
        """Update config with new timestamp for test case `test_case_name`."""
        # Set timestamp at least 1 minute in the future to avoid clock skew issues.
        new_version = utils.ceil_dt(datetime.now(timezone.utc) + timedelta(minutes=1), timedelta(minutes=1))
        new_configs = self._test_case_writer.config_updates({test_case_name: new_version})

        # Compare old configs with new configs.
        self.__print_config_diffs(new_configs)
        if not self._prompt.yes_no("Save these changes to your local config files?"):
            return False

        # Write new configs.
        self.__save_new_configs(new_configs)
        print("Applied changes to config files.")
        return True

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

    def __get_test_case(self, test_case_name: str) -> TestCaseData:
        """Get test case data from the config based on the `test_case_name`."""
        test_cases = self._test_case_loader.get_test_cases(test_case_name)
        if not test_cases:
            raise MinioToolError(f"The name `{test_case_name}` does not match any test cases")

        test_case, *other_test_cases = test_cases
        if other_test_cases:
            suggestions = ", ".join(case.name for case in itertools.islice(test_cases, 3))
            raise MinioToolError(
                f"The name `{test_case_name}` matches multiple test cases. Suggestions: `{suggestions}`"
            )

        return test_case

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
