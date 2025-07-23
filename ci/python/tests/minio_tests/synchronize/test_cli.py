import re
from datetime import datetime
from pathlib import Path
from typing import Any

import minio
import pytest
from pyfakefs.fake_filesystem import FakeFilesystem
from pytest_mock import MockerFixture
from s3_path_wrangler.paths import S3Path

from ci_tools.minio import DEFAULT_MINIO_HOSTNAME, DEFAULT_MULTIPART_UPLOAD_PART_SIZE
from ci_tools.minio.matcher import GlobMatcher, RegexMatcher
from ci_tools.minio.synchronize import DEFAULT_QUEUE_SIZE, DEFAULT_WORKER_COUNT
from ci_tools.minio.synchronize.cli import CommandLine, CommandLineError
from ci_tools.minio.synchronize.sync_plan import Mode
from tests.helpers import minio as helper


class TestCommandLineInterface:
    def test_make_cli_arg_parser(self, fs: FakeFilesystem) -> None:
        parser = CommandLine._make_cli_arg_parser()
        fs.create_dir("./local_dir")

        args = parser.parse_args(["--source=s3://bucket/prefix", "--destination=./local_dir"])

        assert parser.prog == CommandLine.PROGRAM_NAME
        assert args.source == S3Path("s3://bucket/prefix")
        assert args.destination == Path("./local_dir")
        assert args.mode == Mode.NO_DELETE
        assert args.regex == []
        assert args.iregex == []
        assert args.glob == []
        assert args.exclude_glob == []
        assert args.exclude_regex == []
        assert args.exclude_iregex == []
        assert isinstance(args.progress, bool)
        assert args.progress
        assert args.jobs == DEFAULT_WORKER_COUNT
        assert args.queue_size == DEFAULT_QUEUE_SIZE
        assert args.part_size == DEFAULT_MULTIPART_UPLOAD_PART_SIZE
        assert args.endpoint_url == f"https://{DEFAULT_MINIO_HOSTNAME}"
        assert args.profile is None
        assert args.log_level == CommandLine.DEFAULT_LOG_LEVEL

    @pytest.mark.parametrize(
        ("name", "arguments", "expected", "expected_type"),
        [
            pytest.param(
                "timestamp",
                ["--timestamp=2025-01-01T00:00:00"],
                datetime(2025, 1, 1),  # noqa: DTZ001
                datetime,
                id="timestamp",
            ),
            pytest.param("mode", ["--delete"], Mode.DELETE, Mode, id="delete"),
            pytest.param("mode", ["--no-delete"], Mode.NO_DELETE, Mode, id="no-delete"),
            pytest.param("mode", ["--create-only"], Mode.CREATE_ONLY, Mode, id="create-only"),
            pytest.param("mode", ["--update-only"], Mode.UPDATE_ONLY, Mode, id="update-only"),
            pytest.param("progress", ["--progress"], True, bool, id="progress"),
            pytest.param("progress", ["--no-progress"], False, bool, id="no-progress"),
            pytest.param("log_level", ["--log-level=DEBUG"], "DEBUG", str, id="log-level"),
            pytest.param("jobs", ["--jobs=4"], 4, int, id="jobs"),
            pytest.param("queue_size", ["--queue-size=100"], 100, int, id="queue-size"),
            pytest.param("part_size", ["--part-size=42"], 42, int, id="part-size"),
            pytest.param("profile", ["--profile=foo"], "foo", str, id="profile"),
            pytest.param("endpoint_url", ["--endpoint-url=https://min.io"], "https://min.io", str, id="endpoint_url"),
        ],
    )
    def test_make_cli_arg_parser__long_optional_argument(
        self,
        name: str,
        arguments: list[str],
        expected: Any,  # noqa: ANN401
        expected_type: type,
        fs: FakeFilesystem,
    ) -> None:
        fs.create_dir("./local_dir")
        required = ["--source=s3://bucket/prefix", "--destination=./local_dir"]
        parser = CommandLine._make_cli_arg_parser()
        args = parser.parse_args(required + arguments)
        result = getattr(args, name)
        assert isinstance(result, expected_type)
        assert result == expected

    @pytest.mark.parametrize(
        ("name", "arguments", "expected", "expected_type"),
        [
            pytest.param(
                "timestamp",
                ["-t=2025-01-01T00:00:00"],
                datetime(2025, 1, 1),  # noqa: DTZ001
                datetime,
                id="timestamp",
            ),
            pytest.param("log_level", ["-l=DEBUG"], "DEBUG", str, id="log-level"),
            pytest.param("jobs", ["-j=4"], 4, int, id="jobs"),
        ],
    )
    def test_make_cli_arg_parser__short_optional_argument(
        self,
        name: str,
        arguments: list[str],
        expected: Any,  # noqa: ANN401
        expected_type: type,
        fs: FakeFilesystem,
    ) -> None:
        fs.create_dir("./local_dir")
        required = ["-s=s3://bucket/prefix", "-d=./local_dir"]
        parser = CommandLine._make_cli_arg_parser()
        args = parser.parse_args(required + arguments)
        result = getattr(args, name)
        assert isinstance(result, expected_type)
        assert result == expected

    def test_make_cli_arg_parser__multiple_patterns_long(self, fs: FakeFilesystem) -> None:
        fs.create_dir("./local_dir")
        parser = CommandLine._make_cli_arg_parser()
        args = parser.parse_args(
            [
                "--source=s3://bucket/prefix",
                "--destination=./local_dir",
                "--glob=*.txt",
                "--regex=fo+o",
                "--iregex=ba+r",
                "--glob=*.tex",
                "--regex=ba+z",
                "--iregex=qu+x",
            ],
        )

        globs: list[GlobMatcher] = args.glob
        regexes: list[RegexMatcher] = args.regex
        iregexes: list[RegexMatcher] = args.iregex

        assert [glob.pattern for glob in globs] == ["*.txt", "*.tex"]
        assert [regex.pattern.pattern for regex in regexes] == ["fo+o", "ba+z"]
        assert all((regex.pattern.flags & int(re.IGNORECASE)) == 0 for regex in regexes), "IGNORECASE should not be set"
        assert [regex.pattern.pattern for regex in iregexes] == ["ba+r", "qu+x"]
        assert all((regex.pattern.flags & int(re.IGNORECASE)) != 0 for regex in iregexes), "IGNORECASE should be set"

    def test_make_cli_arg_parser__multiple_patterns_short(self, fs: FakeFilesystem) -> None:
        fs.create_dir("./local_dir")
        parser = CommandLine._make_cli_arg_parser()
        args = parser.parse_args(
            [
                "-s=s3://bucket/prefix",
                "-d=./local_dir",
                "-g=*.txt",
                "-r=fo+o",
                "-i=ba+r",
                "-g=*.tex",
                "-r=ba+z",
                "-i=qu+x",
            ],
        )

        globs: list[GlobMatcher] = args.glob
        regexes: list[RegexMatcher] = args.regex
        iregexes: list[RegexMatcher] = args.iregex

        assert [glob.pattern for glob in globs] == ["*.txt", "*.tex"]
        assert [regex.pattern.pattern for regex in regexes] == ["fo+o", "ba+z"]
        assert all((regex.pattern.flags & int(re.IGNORECASE)) == 0 for regex in regexes), "IGNORECASE should not be set"
        assert [regex.pattern.pattern for regex in iregexes] == ["ba+r", "qu+x"]
        assert all((regex.pattern.flags & int(re.IGNORECASE)) != 0 for regex in iregexes), "IGNORECASE should be set"

    def test_make_cli_arg_parser__multiple_exclude_patterns_long(self, fs: FakeFilesystem) -> None:
        fs.create_dir("./local_dir")
        parser = CommandLine._make_cli_arg_parser()
        args = parser.parse_args(
            [
                "--source=s3://bucket/prefix",
                "--destination=./local_dir",
                "--exclude-glob=*.txt",
                "--exclude-regex=fo+o",
                "--exclude-iregex=ba+r",
                "--exclude-glob=*.tex",
                "--exclude-regex=ba+z",
                "--exclude-iregex=qu+x",
            ],
        )

        globs: list[GlobMatcher] = args.exclude_glob
        regexes: list[RegexMatcher] = args.exclude_regex
        iregexes: list[RegexMatcher] = args.exclude_iregex

        assert [glob.pattern for glob in globs] == ["*.txt", "*.tex"]
        assert [regex.pattern.pattern for regex in regexes] == ["fo+o", "ba+z"]
        assert all((regex.pattern.flags & int(re.IGNORECASE)) == 0 for regex in regexes), "IGNORECASE should not be set"
        assert [regex.pattern.pattern for regex in iregexes] == ["ba+r", "qu+x"]
        assert all((regex.pattern.flags & int(re.IGNORECASE)) != 0 for regex in iregexes), "IGNORECASE should be set"

    def test_make_cli_arg_parser__multiple_exclude_patterns_short(self, fs: FakeFilesystem) -> None:
        fs.create_dir("./local_dir")
        parser = CommandLine._make_cli_arg_parser()
        args = parser.parse_args(
            [
                "-s=s3://bucket/prefix",
                "-d=./local_dir",
                "-G=*.txt",
                "-R=fo+o",
                "-I=ba+r",
                "-G=*.tex",
                "-R=ba+z",
                "-I=qu+x",
            ],
        )

        globs: list[GlobMatcher] = args.exclude_glob
        regexes: list[RegexMatcher] = args.exclude_regex
        iregexes: list[RegexMatcher] = args.exclude_iregex

        assert [glob.pattern for glob in globs] == ["*.txt", "*.tex"]
        assert [regex.pattern.pattern for regex in regexes] == ["fo+o", "ba+z"]
        assert all((regex.pattern.flags & int(re.IGNORECASE)) == 0 for regex in regexes), "IGNORECASE should not be set"
        assert [regex.pattern.pattern for regex in iregexes] == ["ba+r", "qu+x"]
        assert all((regex.pattern.flags & int(re.IGNORECASE)) != 0 for regex in iregexes), "IGNORECASE should be set"

    def test_make_cli_arg_parser__invalid_local_dir(self, fs: FakeFilesystem) -> None:
        fs.create_file("./local_file")
        parser = CommandLine._make_cli_arg_parser()

        with pytest.raises(CommandLineError, match="directory"):
            parser.parse_args(["--source=s3://bucket/prefix", "--destination=./local_dir/"])

        with pytest.raises(CommandLineError, match="directory"):
            parser.parse_args(["--source=s3://bucket/prefix", "--destination=./non_existent_dir/"])

    def test_make_cli_arg_parser__invalid_timestamp(self, fs: FakeFilesystem) -> None:
        fs.create_dir("./local_dir")
        parser = CommandLine._make_cli_arg_parser()

        with pytest.raises(CommandLineError, match="Invalid timestamp format"):
            parser.parse_args(["--source=s3://bucket/prefix", "--destination=./local_dir", "--timestamp=2025-1-1"])

    @pytest.mark.parametrize("argument", ["regex", "iregex", "exclude-regex", "exclude-iregex"])
    def test_make_cli_arg_parser__invalid_regex(self, argument: str, fs: FakeFilesystem) -> None:
        fs.create_dir("./local_dir")
        parser = CommandLine._make_cli_arg_parser()

        with pytest.raises(CommandLineError, match="regular expression"):
            parser.parse_args(["--source=s3://bucket/prefix", "--destination=./local_dir", f"--{argument}=foo***"])

    def test_make_cli_arg_parser__invalid_endpoint_url(self, fs: FakeFilesystem) -> None:
        fs.create_dir("./local_dir")
        parser = CommandLine._make_cli_arg_parser()

        with pytest.raises(CommandLineError, match="endpoint URL"):
            parser.parse_args(
                ["--source=s3://bucket/prefix", "--destination=./local_dir", "--endpoint-url=s3.deltares.nl"],
            )

    @pytest.mark.parametrize(
        ("exception", "match"),
        [
            pytest.param(
                minio.error.InvalidResponseError(400, "text/plain", "Invalid response"),
                "Invalid MinIO server response",
                id="InvalidResponseError",
            ),
            pytest.param(minio.error.ServerError("Server error", 500), "MinIO server error", id="ServerError"),
            pytest.param(helper._make_s3_error(), "MinIO S3 error", id="S3Error"),
            pytest.param(ValueError("no credentials"), "Could not locate MinIO credentials", id="ValueError"),
        ],
    )
    def test_check_minio_connection__exception_handler(
        self, exception: Exception, match: str, mocker: MockerFixture
    ) -> None:
        client = mocker.Mock(spec=minio.Minio)
        client.list_buckets.side_effect = exception
        with pytest.raises(CommandLineError, match=match):
            CommandLine._check_minio_connection(client)
