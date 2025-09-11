import difflib
import shutil
import textwrap
from collections import defaultdict
from datetime import datetime, timedelta
from enum import StrEnum
from pathlib import Path
from typing import ClassVar, Iterable, Iterator, List, Mapping, TextIO

import netCDF4
from minio.commonconfig import Tags
from s3_path_wrangler.paths import S3Path
from typing_extensions import override

from src.config.file_check import FileCheck
from src.config.parameter import Parameter
from src.config.types.file_type import FileType
from src.config.types.path_type import PathType
from src.utils.minio_rewinder import Operation, Plan, Rewinder, VersionPair
from tools.minio import utils
from tools.minio.config import (
    DefaultTestCaseData,
    TestCaseData,
    TestCaseId,
    TestCaseIndex,
    TestCasePattern,
    TestCaseWriter,
)
from tools.minio.error import MinioToolError
from tools.minio.prompt import Answer, InputParser, Prompt


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

    DEFAULT_REFERENCE_GLOBS: ClassVar[tuple[str, ...]] = (
        "_tb3_char.run",
        "*_his.nc",
        "*_map.nc",
        "*.dia",
    )

    def __init__(
        self,
        bucket: S3Path,
        rewinder: Rewinder,
        test_case_index: TestCaseIndex,
        test_case_writer: TestCaseWriter,
        prompt: Prompt,
        tags: Tags | None = None,
        color: bool = True,
    ) -> None:
        self._bucket = bucket
        self._rewinder = rewinder
        self._test_case_index = test_case_index
        self._test_case_writer = test_case_writer
        self._prompt = prompt
        self._tags = tags
        self._color = color

    def new(
        self,
        local_dir: Path,
        config_path: Path,
        test_case_name: str | None = None,
        case_prefix: S3Path | None = None,
        reference_prefix: S3Path | None = None,
        default_test_case_name: str | None = None,
    ) -> None:
        """Create a new test case in the config.

        This method uploads both the "case" data and the "reference" data in
        `local_dir` to MinIO, and adds the test case to the config file.
        In most cases "new" requires the user's input on which files in
        `local_dir` are reference files and which files are case files.
        It is also possible to add "file checks" to the config.

        Parameters
        ----------
        local_dir : Path
            Path to the local directory containing files to upload to MinIO.
        config_path : Path
            Path to the config file to add the new test case to.
        test_case_name : str | None, optional
            The name of the new test case. This name must adhere to the strict
            naming convention of the testbench test cases. In essence, the name
            must match the pattern `e<engine_nr>_f<function_nr>_c<case_nr>_<description>`.
            If the test case name is not specified, then the directory name of `local_dir`
            is used instead, provided it adheres to the same test case naming convention.
        case_prefix : S3Path | None, optional
            The prefix in MinIO to upload the case files to. If not specified, `new` will
            try to find the correct prefix to use based on the test case name.
        reference_prefix : S3Path | None, optional
            The prefix in MinIO to upload the reference files to. If not specified `new`
            will tro to find the correct prefix to use based on the test case name.
        default_test_case_name : str | None, optional
            Test cases in the TestBench configs usually inherit properties from a
            "default test case", which are declared at the top level of the config.
            You can choose which default test case by passing the `default_test_case_name`.
            If not specified, `new` will let the user choose the default test case.

        Raises
        ------
        MinioToolError
            Raised when the test case can not be found in the config, or unsupported
            parameters are passed.
        """
        test_case_id = TestCaseId.from_name(test_case_name or local_dir.name)
        test_case, _ = self._test_case_index.find_test_case(TestCasePattern(name_filter=test_case_id.identifier))
        if test_case is not None:
            raise MinioToolError(
                f"A test case matching `{test_case_id}` already exists in the config file `{config_path}`. "
                "Please use a different name."
            )

        print(f"Creating a new test case `{test_case_id}` in the config file `{config_path}`.\n")

        if not case_prefix:
            case_prefix = self.__build_minio_prefix(self._bucket / "cases", test_case_id)
        if not reference_prefix:
            reference_prefix = self.__build_minio_prefix(self._bucket / "references/win64", test_case_id)

        print(f"Prefix for case files: {case_prefix}")
        print(f"Prefix for reference files: {reference_prefix}")

        if default_test_case_name is None:
            default_test_case = self.__choose_default_test_case(config_path)
        else:
            default_cases = self._test_case_index.index[config_path].default_test_cases
            match = next((case for case in default_cases if case.name == default_test_case_name), None)
            if match is None:
                raise MinioToolError(f"Default test case not found: {default_test_case_name}")
            default_test_case = match

        case_plan, reference_plan = self.__build_case_and_reference_plan(
            local_dir=local_dir, case_prefix=case_prefix, reference_prefix=reference_prefix
        )

        self._rewinder.execute_plan(case_plan)
        self._rewinder.execute_plan(reference_plan)
        new_timestamp = max(
            self.__max_last_modified(case_plan.minio_prefix),
            self.__max_last_modified(reference_plan.minio_prefix),
        ) + timedelta(milliseconds=1)

        file_checks = self.__choose_file_checks(reference_plan)

        test_case = TestCaseData(
            name=str(test_case_id),
            case_dir=local_dir,
            reference_dir=local_dir,
            case_prefix=case_prefix,
            reference_prefix=reference_prefix,
            version=new_timestamp,
            file_checks=file_checks,
            default_test_case=default_test_case.name,
            max_run_time=default_test_case.max_run_time,
        )

        config_updates = self._test_case_writer.new_test_case(test_case, config_path)
        self.__update_configs(config_updates)

    def push(
        self,
        test_case_pattern: TestCasePattern,
        path_type: PathType,
        local_dir: Path | None = None,
        allow_create_and_delete: bool = False,
    ) -> None:
        """Upload local files to MinIO and update the timestamp in the testbench config.

        By default `push` uses case/reference data in the local
        directories configured in the test bench config file. If some other
        directory is required, it can be passed through the `local_dir` parameter.

        Parameters
        ----------
        test_case_pattern : TestCasePattern
            Look for a test case in the index with this pattern. The pattern
            can contain a glob pattern for the config file and a substring of
            the name of the test case. The pattern must match exactly one
            test case in the index, but it may occur in multiple config files.
        path_type : PathType
            If set to `PathType.INPUT`, case data is uploaded.
            If set to `PathType.REFERENCE`, reference data is uploaded.
        local_dir : Path | None, optional
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
        test_case, configs = self._test_case_index.find_test_case(test_case_pattern)
        if not test_case or not configs:
            raise MinioToolError(f"No test case found matching `{test_case_pattern.name_filter}` in the config.")

        minio_prefix = test_case.get_remote_prefix(path_type)
        local_dir = local_dir or test_case.get_default_local_directory(path_type)
        self.__print_locations(test_case, local_dir, minio_prefix)

        if test_case.version and not self.__ignore_conflicts(minio_prefix, test_case.version):
            return  # There's conflicts and the user decided to abort.

        if not self.__build_and_execute_plan(local_dir, minio_prefix, allow_create_and_delete):
            return  # No changes were made.

        # Add extra millisecond just to be safe.
        new_version = self.__max_last_modified(minio_prefix) + timedelta(milliseconds=1)

        config_updates = self._test_case_writer.update_versions({test_case.name: new_version}, configs)
        self.__update_configs(config_updates)

    def update_references(self, test_case_pattern: TestCasePattern, local_dir: Path | None = None) -> None:
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
        test_case_pattern : TestCasePattern
            Look for a test case in the index with this pattern. The pattern
            can contain a glob pattern for the config file and a substring of
            the name of the test case. The pattern must match exactly one
            test case in the index, but it may occur in multiple config files.
        local_dir : Path | None, optional
            Path to the local directory containing files to upload to MinIO.
            If not set, use the local path from the test bench config.

        Raises
        ------
        MinioToolError
            Raised when the test case can not be found in the config, or unsupported
            parameters are passed.
        """
        test_case, configs = self._test_case_index.find_test_case(test_case_pattern)
        if not test_case or not configs:
            raise MinioToolError(f"No test case found matching `{test_case_pattern.name_filter}` in the config.")

        local_dir = local_dir or test_case.case_dir
        minio_prefix = test_case.reference_prefix
        self.__print_locations(test_case, local_dir, minio_prefix)

        if test_case.version and not self.__ignore_conflicts(minio_prefix, test_case.version):
            return  # There's conflicts and the user decided to abort.

        if not self.__build_and_execute_plan(local_dir, minio_prefix, allow_create_and_delete=False):
            return  # No changes were made.

        # Add extra millisecond just to be safe.
        new_version = self.__max_last_modified(minio_prefix) + timedelta(milliseconds=1)

        config_updates = self._test_case_writer.update_versions({test_case.name: new_version}, configs)
        self.__update_configs(config_updates)

    def pull(
        self,
        test_case_pattern: TestCasePattern,
        path_type: PathType,
        local_dir: Path | None = None,
        timestamp: datetime | None = None,
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
        test_case_pattern : TestCasePattern
            Look for a test case in the index with this pattern. The pattern
            can contain a glob pattern for the config file and a substring of
            the name of the test case. The pattern must match exactly one
            test case in the index, but it may occur in multiple config files.
        path_type : PathType
            If set to `PathType.INPUT`, case data is downloaded.
            If set to `PathType.REFERENCE`, reference data is downloaded.
        local_dir : Path | None, optional
            Path to local directory to save the objects from MinIO in.
            This directory is created if it does not exist.
            If not set, use the local path stored in the testbench config.
        timestamp : datetime | None, optional
            If set, overrides `version` timestamp set in the config. Rewind
            time to this timestamp to get objects from this moment in the past.

        Raises
        ------
        MinioToolError
            Raised when test case can't be found or if invalid arguments are passed.
        """
        test_case, _ = self._test_case_index.find_test_case(test_case_pattern)
        if not test_case:
            raise MinioToolError(f"No test case found matching `{test_case_pattern.name_filter}` in the config.")

        minio_prefix = test_case.get_remote_prefix(path_type)
        local_dir = local_dir or test_case.get_default_local_directory(path_type)

        self.__print_locations(test_case, local_dir, minio_prefix)

        if timestamp is None and test_case.version and not self.__ignore_conflicts(minio_prefix, test_case.version):
            return  # There's conflicts and the user decided to abort.

        self.__download(minio_prefix, local_dir, timestamp or test_case.version)

    def __choose_file_checks(self, reference_plan: Plan) -> list[FileCheck]:
        netcdfs = [
            p.local_path.relative_to(reference_plan.local_dir)
            for p in reference_plan.items
            if p.local_path and p.local_path.suffix.lower() == ".nc"
        ]

        file_checks: defaultdict[Path, list[str]] = defaultdict(list)

        for netcdf in netcdfs:
            dataset = netCDF4.Dataset(reference_plan.local_dir / netcdf, mode="r")
            variable_names = {var for var in dataset.variables.keys()}

            selected_variables = self._prompt.input(
                f"Enter variable names from {netcdf.as_posix()} to add to the checks (separated by spaces).\n"
                "Press <TAB> to auto-complete, press <TAB> twice to view options",
                parser=MultiInputParser(variable_names),
            )
            to_check = set(selected_variables) & variable_names
            to_skip = set(selected_variables) - variable_names
            if to_skip:
                print(f"Skipping non-existing variables {to_skip}")
            if to_check:
                print(f"Adding checks for variables: {to_check}")
                file_checks[netcdf].extend(to_check)

        return [self.__make_netcdf_file_check(path, variables) for path, variables in file_checks.items()]

    def __choose_default_test_case(self, config: Path) -> DefaultTestCaseData:
        choices = self._test_case_index.get_default_test_cases(config)
        if not choices:
            raise ValueError(f"Config {config} does not have any default test cases")

        if len(choices) == 1:
            return choices[0]

        default_idx = next((i for i, test_case in enumerate(choices) if "default" in test_case.name), 0)
        choice = self._prompt.choose(
            "Choose a default test case",
            choices,
            default_idx=default_idx,
            formatter=lambda case: f"{case.name} (max runtime: {case.max_run_time} seconds)",
        )
        if choice is None:
            raise ValueError("Failed to make a choice of default test case")

        return choice

    def __build_minio_prefix(self, prefix: S3Path, test_case_name: TestCaseId) -> S3Path:
        # Check if the test case already exists.
        for suggestion in (test_case_name.engine_id, test_case_name.feature_id):
            hint = prefix / suggestion
            options = self._rewinder.autocomplete_prefix(hint)
            match options:
                case []:
                    raise ValueError(f"No test case found with the prefix: {hint.key}")
                case [option]:
                    prefix = option
                case _:
                    message = f"Multiple test cases found with prefix `{hint.key}`. Please choose one."
                    choice = self._prompt.choose(message, options)
                    if choice is None:
                        raise ValueError("No choice made. Abort creation of test case.")
                    prefix = choice

        hint = prefix / test_case_name.case_id
        options = self._rewinder.autocomplete_prefix(hint)
        if options:
            raise ValueError(
                f"Please use a different test case name.\n"
                f"Test case prefix {hint} already exists in MinIO: {options[0]}."
            )

        return prefix / f"{test_case_name.case_id}{test_case_name.trailer}"

    def __build_case_and_reference_plan(
        self, local_dir: Path, case_prefix: S3Path, reference_prefix: S3Path
    ) -> tuple[Plan, Plan]:
        """Determine which files to upload to MinIO and where.

        Some files will by default be uploaded to the references. But the
        user's input is required to determine what files to upload to the
        cases and what files to upload to the references.
        """
        completions = [p.relative_to(local_dir).as_posix() for p in local_dir.rglob("*")]
        ignore_globs: list[str] = []
        reference_globs = list(self.DEFAULT_REFERENCE_GLOBS)

        rebuild_plan = True
        while True:
            if rebuild_plan:
                case_plan = self._rewinder.build_plan(
                    src_dir=local_dir,
                    dst_prefix=case_prefix,
                    allow_create_and_delete=True,
                    tags=self._tags,
                    exclude_patterns=reference_globs + ignore_globs,
                )

                reference_plan = self._rewinder.build_plan(
                    src_dir=local_dir,
                    dst_prefix=reference_prefix,
                    allow_create_and_delete=True,
                    tags=self._tags,
                    include_patterns=reference_globs,
                    exclude_patterns=ignore_globs,
                )

                print("Case plan:")
                self.__print_plan(case_plan)
                print("Reference plan:")
                self.__print_plan(reference_plan)

            rebuild_plan = True
            command, argument = self._prompt.input(
                message="Type command (ok, cancel, show, help, ignore [glob], reference [glob], reset)",
                parser=PlannerCommandParser(completions),
            )
            match command:
                case Command.IGNORE if argument:
                    ignore_globs.append(argument)
                case Command.REFERENCE if argument:
                    reference_globs.append(argument)
                case Command.RESET:
                    ignore_globs.clear()
                    reference_globs = list(self.DEFAULT_REFERENCE_GLOBS)
                case Command.CANCEL:
                    raise MinioToolError("Operation cancelled by the user.")
                case Command.HELP:
                    rebuild_plan = False
                    self.__print_testcase_planner_help()
                case Command.SHOW:
                    pass
                case Command.OK:
                    return case_plan, reference_plan

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
        if self._prompt.yes_no("Apply these changes?") != Answer.YES:
            return False

        self._rewinder.execute_plan(plan)

        return True

    def __ignore_conflicts(self, minio_prefix: S3Path, timestamp: datetime) -> bool:
        """See if there has been any changes to the data in MinIO between `timestamp` and `now`."""
        conflicts = self._rewinder.detect_conflicts(minio_prefix, timestamp, add_tags_to_latest=True)
        if conflicts:
            self.__print_conflicts(conflicts, minio_prefix, timestamp)
            if self._prompt.yes_no("Continue anyway?", default=Answer.NO) != Answer.YES:
                return False

        return True

    def __update_configs(self, config_updates: Mapping[Path, TextIO]) -> bool:
        """Update config with new timestamp for test case `test_case_name`."""
        # Compare old configs with new configs.
        self.__print_config_diffs(config_updates)
        if self._prompt.yes_no("Save these changes to your local config files?") != Answer.YES:
            return False

        # Write new configs.
        self.__save_new_configs(config_updates)
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
        rewind_timestamp: datetime | None = None,
    ) -> None:
        """Download objects from MinIO to a local directory."""
        # Create directory if it doesn't exist. Ask user to continue if it is not empty.
        local_dir.mkdir(parents=True, exist_ok=True)
        if next(local_dir.iterdir(), None) is not None:  # Directory is not empty.
            if self._prompt.yes_no(f"Directory {local_dir} is not empty. Continue anyway?") != Answer.YES:
                return

        self._rewinder.download(minio_prefix.bucket, minio_prefix.key, local_dir, rewind_timestamp)

    @staticmethod
    def __save_new_configs(new_configs: Mapping[Path, TextIO]) -> None:
        """Save the contents of the new config files on disk."""
        for path, buffer in new_configs.items():
            buffer.seek(0)
            with open(path, "w") as out_handle:
                shutil.copyfileobj(fsrc=buffer, fdst=out_handle)

    @staticmethod
    def __make_netcdf_file_check(
        path: Path, variable_names: Iterable[str], tolerance_absolute: float = 1e-4
    ) -> FileCheck:
        file_check = FileCheck()
        file_check.name = path.as_posix()
        file_check.type = FileType.NETCDF
        parameters: dict[str, list[Parameter]] = {}
        for variable_name in variable_names:
            parameter = Parameter()
            parameter.name = variable_name
            parameter.tolerance_absolute = tolerance_absolute
            parameters[variable_name] = [parameter]
        file_check.parameters = parameters
        return file_check

    def __print_locations(self, test_case: TestCaseData, local_dir: Path, minio_prefix: S3Path) -> None:
        """Print some details from the config and the used locations."""
        label_value_pairs = (
            ("Test case name:", test_case.name),
            ("Config data version:", test_case.version),
            ("Local directory:", local_dir),
            ("MinIO path:", minio_prefix),
        )
        print("".join(f"{label:24s}{value}\n" for label, value in label_value_pairs))

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

    def __print_testcase_planner_help(self) -> None:
        """Print help message for the user."""
        message = textwrap.dedent("""
            Welcome to the MinIO test case upload planner!
            Available commands:
                ignore [glob]       Exclude files from both cases and references.
                reference [glob]    Add files to the references.
                reset               Reset all changes and restore default plan.
                cancel              Abort the operation.
                help                Show this help message.
                ok                  Finish and upload the files.

            Glob patterns can be used to match files in the local directory.
            The following special characters can be used in glob patterns:
            - `*` to match any sequence of characters within a path segment.
            - `?` to match any single character.
            - `**` to match any number of path segments.
            - [seq] to match any single character in the sequence.
            - [!seq] to match any single character not in the sequence.
            """).lstrip()

        print(message)


class Command(StrEnum):
    """Commands supported by the New Test Case Planner."""

    IGNORE = "ignore"
    REFERENCE = "reference"
    RESET = "reset"
    CANCEL = "cancel"
    HELP = "help"
    SHOW = "show"
    OK = "ok"


class PlannerCommandParser(InputParser[tuple[Command, str | None]]):
    """Parse input lines with commands for the New Test Case Planner."""

    def __init__(self, completions: Iterable[str]) -> None:
        self._completions: list[str] = sorted(completions)

    @override
    def parse(self, input_: str) -> tuple[Command, str | None]:
        if not input_.endswith("\n"):
            return Command.CANCEL, None  # End of file.

        stripped_input = input_.strip()
        if not stripped_input:
            return Command.OK, None  # Default is OK.

        command_str, *others = stripped_input.split(maxsplit=1)
        command = Command(command_str.lower())  # Raises ValueError if command is unknown.
        argument = others[0] if others else None

        argument_required = command in (Command.IGNORE, Command.REFERENCE)
        if argument_required and argument is None:
            raise ValueError(f"Command `{command.value}` requires a file path or glob pattern as argument.")
        elif not argument_required and argument is not None:
            raise ValueError(f"Command `{command.value}` does not accept any arguments.")

        return command, argument

    @override
    def complete(self, hint: str) -> Iterator[str]:
        return (item for item in self._completions if item.startswith(hint))


class MultiInputParser(InputParser[list[str]]):
    """Parse input lines with multiple values delimited by whitespace."""

    def __init__(self, completions: Iterable[str] | None = None) -> None:
        self._completions = [] if completions is None else sorted(completions)

    @override
    def parse(self, input_: str) -> list[str]:
        if not input_.endswith("\n"):
            return []  # End of file.
        return input_.split()

    @override
    def complete(self, hint: str) -> Iterator[str]:
        return (item for item in self._completions if item.startswith(hint))
