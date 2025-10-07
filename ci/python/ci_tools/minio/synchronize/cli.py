import argparse
import itertools
import logging
import os
import re
import sys
import textwrap
from datetime import datetime
from pathlib import Path
from typing import ClassVar
from urllib.parse import urlparse

import certifi
import minio
import urllib3
from minio.credentials.providers import AWSConfigProvider
from s3_path_wrangler.paths import S3Path

from ci_tools import minio as const
from ci_tools.minio.matcher import GlobMatcher, RegexMatcher
from ci_tools.minio.synchronize import DEFAULT_QUEUE_SIZE, DEFAULT_WORKER_COUNT
from ci_tools.minio.synchronize.sync_builder import SyncBuilder
from ci_tools.minio.synchronize.sync_plan import Mode


class CommandLine:
    """Defines the command line interface for the minio-sync tool."""

    PROGRAM_NAME: ClassVar[str] = "minio-sync"
    LOG_LEVELS: ClassVar[list[str]] = ["DEBUG", "INFO", "WARNING", "ERROR", "FATAL"]
    DEFAULT_LOG_LEVEL: ClassVar[str] = "WARNING"

    LOGGER_FORMAT: ClassVar[str] = (
        "%(asctime)s.%(msecs)03d %(levelname)s %(threadName)s %(filename)s:%(lineno)s %(message)s"
    )
    LOGGER_DATE_FORMAT: ClassVar[str] = "%Y-%m-%dT%H:%M:%S"

    HTTP_DEFAULT_POOL_SIZE: ClassVar[int] = 10
    HTTP_TIMEOUT_SECONDS: ClassVar[int] = 300
    HTTP_RETRY_COUNT: ClassVar[int] = 5
    HTTP_BACKOFF_FACTOR: ClassVar[float] = 0.2

    CLI_DESCRIPTION: ClassVar[str] = textwrap.dedent("""
        Synchronize the contents of a local directory and a set of objects in a MinIO bucket.

        Example usage:
        Download all objects in a MinIO bucket under the 'my_objects/' prefix to the local directory 'my_files':
        > minio-sync --source s3://my-bucket/my_objects --destination ./my_files

        Download objects from a MinIO bucket at a specific moment in time:
        > minio-sync --source s3://my-bucket/my_objects --destination ./my_files --timestamp 2024-12-05T20:00:00

        Download only objects from MinIO with the '.jpg' or '.png' extensions:
        > minio-sync --source s3://my-bucket/my_objects --destination ./my_files --glob '*.jpg', --glob '*.png'

        Download only objects from MinIO without the '.nc' extension:
        > minio-sync --source s3://my-bucket/my_objects --destination ./my_files --exclude-glob '*.nc'

        Download objects from a MinIO bucket. Don't delete or overwrite existing files:
        > minio-sync --create-only --source s3://my-bucket/my_objects --destination ./my_files

        Upload all files from the local directory 'my_files' to a MinIO bucket under the 'my_objects/' prefix:
        > minio-sync --source ./my_files --destination s3://my-bucket/my_objects

        Upload files to a MinIO bucket. Only update existing objects the bucket:
        > minio-sync --update-only --source ./my_files --destination s3://my-bucket/my_objects
    """)

    HELP: ClassVar[dict[str, str]] = {
        "source": "Either a path to a local directory or a MinIO prefix. (MinIO prefixes start with 's3://')",
        "destination": "Either a path to a local directory or a MinIO prefix. (MinIO prefixes start with 's3://')",
        "timestamp": "Recover objects in MinIO at a specific point in time. Only supported for downloads.",
        "create-only": "Only create objects at the destination. No objects are overwritten or deleted.",
        "update-only": "Only update objects at the destination. No objects are created or deleted.",
        "no-delete": "Only create or update objects at the destination. No objects are deleted. (The default mode)",
        "delete": "Create, update, and also delete objects at the destination.",
        "glob": "Include objects that match the given glob pattern. Can be specified multiple times.",
        "regex": "Include objects that match the given regex pattern. Can be specified multiple times.",
        "iregex": "The same as 'regex', but case-insensitive. Can be specified multiple times.",
        "exclude-glob": "Exclude objects that match the given glob pattern. Can be spcified multiple times.",
        "exclude-regex": "Exclude objects that match the given regex pattern. Can be specified multiple times.",
        "exclude-iregex": "The same as 'exclude-regex', but case-insensitive. Can be specified multiple times.",
        "progress": "Show the progress bar during synchronization. (default)",
        "no-progress": "Don't show the progress bar during synchronization.",
        "log-level": "The verbosity level of the logging. Can be one of: DEBUG, INFO, WARNING, ERROR, FATAL.",
        "jobs": "The number of threads to use for synchronization.",
        "queue-size": "The maximum size of the queue for synchronization. This will limit memory usage.",
        "part-size": "The size of the parts to use for multipart uploads.",
        "profile": "The AWS profile to use to authenticate with MinIO.",
        "endpoint-url": "The endpoint URL to use for MinIO.",
    }

    @classmethod
    def run(cls) -> int:
        """Run 'minio-sync' command-line program.

        Returns
        -------
        int
            The exit code of the program. 0 if successful, non-zero if an error occurred.
        """
        try:
            parser = cls._make_cli_arg_parser()
            args = parser.parse_args()

            logger = cls._make_logger("minio-sync", args.log_level)
            minio_client = cls._make_minio_client(
                endpoint_url=args.endpoint_url,
                max_pool_size=args.jobs,
                profile=args.profile,
            )

            cls._check_minio_connection(minio_client)
        except CommandLineError as exc:
            print(exc, file=sys.stderr)
            return 1

        builder = (
            SyncBuilder()
            .set_mode(args.mode)
            .set_minio_client(minio_client)
            .set_logger(logger)
            .add_includes(itertools.chain(args.glob, args.regex, args.iregex))
            .add_excludes(itertools.chain(args.exclude_glob, args.exclude_regex, args.exclude_iregex))
            .set_remote_timestamp(args.timestamp)
            .set_show_progress(args.progress)
            .set_thread_count(args.jobs)
            .set_max_queue_size(args.queue_size)
            .set_part_size(args.part_size)
        )

        source: Path | S3Path = args.source
        destination: Path | S3Path = args.destination
        try:
            if isinstance(source, S3Path) and isinstance(destination, Path):
                builder.set_local_directory(destination)
                builder.set_remote_prefix(source)
                remote_to_local = builder.build_remote_to_local()
                remote_to_local.synchronize()
            elif isinstance(source, Path) and isinstance(destination, S3Path):
                builder.set_local_directory(source)
                builder.set_remote_prefix(destination)
                local_to_remote = builder.build_local_to_remote()
                local_to_remote.synchronize()
            elif isinstance(source, S3Path) and isinstance(destination, S3Path):
                print("Synchronizing two S3 prefixes is currently not supported.", file=sys.stderr)
                return 1
            else:
                print("Synchronizing two local directories is not supported.", file=sys.stderr)
                return 1
        except ValueError as exc:
            print(f"Synchronization failed: {exc.args[0]}", file=sys.stderr)
            return 1

        return 0

    @classmethod
    def _make_cli_arg_parser(cls) -> argparse.ArgumentParser:
        parser = argparse.ArgumentParser(
            prog=cls.PROGRAM_NAME,
            description=cls.CLI_DESCRIPTION,
            formatter_class=argparse.RawDescriptionHelpFormatter,
        )

        parser.add_argument("-s", "--source", required=True, type=cls._location, help=cls.HELP["source"])
        parser.add_argument("-d", "--destination", required=True, type=cls._location, help=cls.HELP["destination"])
        parser.add_argument("-t", "--timestamp", type=cls._timestamp, help=cls.HELP["timestamp"])

        mode_group = parser.add_mutually_exclusive_group()
        for mode in Mode:
            mode_group.add_argument(
                f"--{mode.value}", dest="mode", action="store_const", const=mode, help=cls.HELP[mode.value]
            )

        parser.add_argument("-g", "--glob", action="append", type=GlobMatcher, help=cls.HELP["glob"])
        parser.add_argument("-G", "--exclude-glob", action="append", type=GlobMatcher, help=cls.HELP["exclude-glob"])
        parser.add_argument("-r", "--regex", action="append", type=cls._regex, help=cls.HELP["regex"])
        parser.add_argument("-R", "--exclude-regex", action="append", type=cls._regex, help=cls.HELP["exclude-regex"])
        parser.add_argument("-i", "--iregex", action="append", type=cls._iregex, help=cls.HELP["iregex"])
        parser.add_argument(
            "-I", "--exclude-iregex", action="append", type=cls._iregex, help=cls.HELP["exclude-iregex"]
        )

        progress_group = parser.add_mutually_exclusive_group()
        progress_group.add_argument("--progress", dest="progress", action="store_true", help=cls.HELP["progress"])
        progress_group.add_argument(
            "--no-progress", dest="progress", action="store_false", help=cls.HELP["no-progress"]
        )

        parser.add_argument("-l", "--log-level", choices=cls.LOG_LEVELS, help=cls.HELP["log-level"])
        parser.add_argument("-j", "--jobs", type=int, help=cls.HELP["jobs"])
        parser.add_argument("--queue-size", type=int, help=cls.HELP["queue-size"])
        parser.add_argument("--part-size", type=int, help=cls.HELP["part-size"])
        parser.add_argument("--profile", help=cls.HELP["profile"])
        parser.add_argument("--endpoint-url", type=cls._endpoint_url, help=cls.HELP["endpoint-url"])

        parser.set_defaults(
            mode=Mode.NO_DELETE,
            glob=[],
            regex=[],
            iregex=[],
            exclude_glob=[],
            exclude_regex=[],
            exclude_iregex=[],
            progress=True,
            part_size=const.DEFAULT_MULTIPART_UPLOAD_PART_SIZE,
            jobs=DEFAULT_WORKER_COUNT,
            queue_size=DEFAULT_QUEUE_SIZE,
            log_level=cls.DEFAULT_LOG_LEVEL,
            endpoint_url=os.environ.get("ENDPOINT_URL", f"https://{const.DEFAULT_MINIO_HOSTNAME}"),
            profile=os.environ.get("AWS_PROFILE"),
        )

        return parser

    @classmethod
    def _make_logger(cls, name: str, log_level: str) -> logging.Logger:
        handler = logging.StreamHandler(sys.stderr)
        handler.setLevel(log_level)
        handler.setFormatter(logging.Formatter(fmt=cls.LOGGER_FORMAT, datefmt=cls.LOGGER_DATE_FORMAT))
        logger = logging.getLogger(name)
        logger.addHandler(handler)
        return logger

    @classmethod
    def _make_minio_client(
        cls,
        endpoint_url: str,
        profile: str | None = None,
        max_pool_size: int | None = None,
    ) -> minio.Minio:
        parsed_url = urlparse(endpoint_url)
        secure = False if parsed_url.scheme == "http" else True

        max_pool_size = max_pool_size or cls.HTTP_DEFAULT_POOL_SIZE
        timeout = cls.HTTP_TIMEOUT_SECONDS
        pool_manager = urllib3.PoolManager(
            timeout=urllib3.Timeout(connect=timeout, read=timeout),
            maxsize=max(cls.HTTP_DEFAULT_POOL_SIZE, max_pool_size),
            cert_reqs="CERT_REQUIRED" if secure else "CERT_NONE",
            ca_certs=os.environ.get("SSL_CERT_FILE", certifi.where()),
            retries=urllib3.Retry(
                total=cls.HTTP_RETRY_COUNT,
                backoff_factor=cls.HTTP_BACKOFF_FACTOR,
                status_forcelist=[500, 502, 503, 504],
            ),
        )
        return minio.Minio(
            parsed_url.netloc,
            credentials=AWSConfigProvider(profile=profile),
            http_client=pool_manager,
            secure=secure,
        )

    @classmethod
    def _check_minio_connection(cls, minio_client: minio.Minio) -> None:
        """Check if the credentials are valid."""
        try:
            minio_client.list_buckets()
        except minio.error.InvalidResponseError as exc:
            message = f"Invalid MinIO server response.\nMessage: {exc.args[0].capitalize()}"
            raise CommandLineError(message) from exc
        except minio.error.ServerError as exc:
            message = f"MinIO server error.\nHTTP status code: {exc.status_code}\nMessage: {exc.args[0].capitalize()}"
            raise CommandLineError(message) from exc
        except minio.error.S3Error as exc:
            message = f"MinIO S3 error.\nCode: {exc.code}\nMessage: {exc.args[0].capitalize()}"
            raise CommandLineError(message) from exc
        except ValueError as exc:
            # Raised by the AWSConfigProvider if there's problems with the credentials.
            message = textwrap.dedent(
                f"""
                Could not locate MinIO credentials:
                Message: {exc.args[0].capitalize()}"

                Please save your MinIO credentials in the AWS credentials file: {Path.home() / ".aws/credentials"}
                Example contents of the credentials file:

                [default]
                aws_access_key_id = YOUR_ACCESS_KEY
                aws_secret_access_key = YOUR_SECRET_KEY

                Where YOUR_ACCESS_KEY and YOUR_SECRET_KEY should be replaced by your MinIO credentials.
                You can generate new credentials using the MinIO web interface:
                https://{const.DEFAULT_MINIO_WEB_HOSTNAME}/access-keys
                """
            ).strip()
            raise CommandLineError(message) from exc

    @staticmethod
    def _timestamp(timestamp: str) -> datetime:
        """Parse a timestamp string into a datetime object."""
        try:
            return datetime.fromisoformat(timestamp)
        except ValueError as exc:
            message = textwrap.dedent(
                f"""
                Invalid timestamp format: {re.escape(timestamp)}
                Message: {exc.args[0]}

                The `datetime.fromisoformat` function is used to parse timestamps. It expects the
                format to be in the ISO 8601 format. The Python documentation has many examples of
                valid timestamps:
                https://docs.python.org/3/library/datetime.html#datetime.datetime.fromisoformat
                """
            ).strip()
            raise CommandLineError(message) from exc

    @staticmethod
    def _regex(pattern: str, ignore_case: bool = False) -> RegexMatcher:
        """Create a RegexMatcher from a pattern."""
        try:
            return RegexMatcher(pattern, ignore_case=ignore_case)
        except re.error as exc:
            message = textwrap.dedent(
                f"""
                Invalid regular expression: {exc.msg.capitalize()}.
                {str(exc.pattern)}
                {"^".rjust(exc.colno)}
                Please consult the Python documentation for help on writing regular expressions:
                https://docs.python.org/3/library/re.html#regular-expression-syntax
                """
            ).strip()
            raise CommandLineError(message) from exc

    @classmethod
    def _iregex(cls, pattern: str) -> RegexMatcher:
        """Create a case-insensitive RegexMatcher from a pattern."""
        return cls._regex(pattern, ignore_case=True)

    @classmethod
    def _location(cls, path: str) -> Path | S3Path:
        """Check if the path is a local directory or a MinIO prefix."""
        if path.startswith("s3://"):
            return S3Path(path)

        result = Path(path)
        if not result.is_dir():
            raise CommandLineError(f"Not a directory: {path}")
        return result

    @classmethod
    def _endpoint_url(cls, url: str) -> str:
        """Check if the URL is valid."""
        parsed_url = urlparse(url)
        if not parsed_url.scheme:
            message = textwrap.dedent(f"""
                Missing scheme in endpoint URL: {url}
                Please prefix the URL with either 'https://' or 'http://'.
                """).strip()
            raise CommandLineError(message)
        return url


class CommandLineError(Exception):
    """Custom exception for command line errors."""

    def __init__(self, message: str) -> None:
        super().__init__(message)
        self._message = message

    def __str__(self) -> str:
        """Return the error message."""
        return self._message


if __name__ == "__main__":
    CommandLine.run()
