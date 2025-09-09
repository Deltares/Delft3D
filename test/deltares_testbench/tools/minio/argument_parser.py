import argparse
import logging
import os
import re
import sys
from contextlib import contextmanager
from datetime import datetime, timezone
from pathlib import Path
from typing import Iterable, Iterator, Sequence
from urllib.parse import urlparse

from minio import Minio
from minio.commonconfig import Tags
from minio.error import MinioException, S3Error
from s3_path_wrangler.paths import S3Path

from src.config.types.path_type import PathType
from src.utils.handlers.credential_handler import CredentialHandler
from src.utils.minio_rewinder import Rewinder
from tools.minio import config, error, prompt
from tools.minio.config import TestBenchConfigWriter
from tools.minio.minio_tool import MinioTool

HELP_CONFIG = "Path to test bench config file."
HELP_TEST_CASE_FILE = "Relative path from the testbench root folder, contains a list of testcases to update."
HELP_TEST_CASE_NAME = (
    "Name of a test case in the config file. This can be a substring, but it should match exactly one test case."
)
HELP_NEW_TEST_CASE_NAME = (
    "Name of the new test case. This name must adhere to the test case name pattern: "
    "e<engine_id>_f<function_id>_c<case_id>[-_]<description"
)
HELP_COLOR = "Use color in the output."
HELP_NO_COLOR = "Don't use color in the output."
HELP_INTERACTIVE = "Use the interactive prompt to allow users to make decisions."
HELP_BATCH = "Turn on (non-interactive) batch mode. Makes the default decision."
HELP_FORCE = "Only in combination with batch mode: Ignore conflicts and always proceed."
HELP_LOCAL_PATH = "Path to local directory."
HELP_ALLOW_CREATE_AND_DELETE = "Create new files or remove files from MinIO."
HELP_ISSUE_ID = "Identifier for the JIRA issue related to this change. Format: '[A-Z]+-[0-9]+'"
HELP_TIMESTAMP = "Get past version of the objects in MinIO."
HELP_LATEST = "Get the latest version of the objects in MinIO."
HELP_BUCKET = "The name of the MinIO bucket to upload or download test case data."
HELP_PROFILE = "The name of the profile to load the credentials from in the credentials file."
HELP_ENDPOINT_URL = "The endpoint url to an S3-compatible service."
HELP_CASE_PREFIX = "Location in MinIO where the case data should be stored."
HELP_REFERENCE_PREFIX = "Location in MinIO where the reference data should be stored."


def issue_id_type(issue_id: str) -> str:
    """Verify that `issue_id` matches the JIRA issue regex."""
    if re.match(r"^[_A-Z0-9]+-\d+$", issue_id) is None:
        raise argparse.ArgumentTypeError("Issue id must match pattern [_A-Z0-9]+-[0-9]+")
    return issue_id


def utc_timestamp(value: str | None) -> datetime | None:
    """Parse a timestamp and replace the timezone with a UTC timezone."""
    if value is None:
        return None
    return datetime.fromisoformat(value).astimezone(tz=timezone.utc)


def make_argument_parser() -> argparse.ArgumentParser:
    """Build `ArgumentParser` used to parse the command line arguments."""
    # Create top level parser.
    parser = argparse.ArgumentParser(prog="tools.minio")

    # Add common arguments.
    common_parser = argparse.ArgumentParser(add_help=False)
    common_parser.add_argument("-c", "--config", type=Path, help=HELP_CONFIG)
    common_parser.add_argument("--color", dest="color_output", action="store_true", help=HELP_COLOR)
    common_parser.add_argument("--no-color", dest="color_output", action="store_false", help=HELP_NO_COLOR)
    common_parser.add_argument("-i", "--interactive", dest="interactive", action="store_true", help=HELP_INTERACTIVE)
    common_parser.add_argument("-b", "--batch", dest="interactive", action="store_false", help=HELP_BATCH)
    common_parser.add_argument("-f", "--force", action="store_true", help=HELP_FORCE)
    common_parser.add_argument("-p", "--local-path", type=Path, required=False, help=HELP_LOCAL_PATH)
    common_parser.add_argument("--log-level", required=False, choices=["CRITICAL", "ERROR", "WARNING", "INFO", "DEBUG"])
    common_parser.add_argument("--bucket", type=S3Path.from_bucket, required=False, help=HELP_BUCKET)
    common_parser.add_argument("--profile", required=False, help=HELP_PROFILE)
    common_parser.add_argument("--endpoint-url", required=False, help=HELP_ENDPOINT_URL)
    common_parser.set_defaults(
        endpoint_url=os.environ.get("AWS_ENDPOINT_URL", "https://s3.deltares.nl"),
        profile=os.environ.get("AWS_PROFILE"),
        bucket=S3Path.from_bucket("dsc-testbench"),
        log_level="INFO",
        color_output=True,
        interactive=True,
        force=False,
    )

    # Add subparsers for each 'subcommand'.
    subparsers = parser.add_subparsers()

    # Create the parser for the `new` subcommand.
    new_parser = subparsers.add_parser("new", parents=[common_parser])
    new_parser.add_argument("-n", "--test-case-name", type=str, help=HELP_NEW_TEST_CASE_NAME)
    new_parser.add_argument("--case-prefix", type=S3Path, help=HELP_CASE_PREFIX)
    new_parser.add_argument("--reference-prefix", type=S3Path, help=HELP_REFERENCE_PREFIX)
    new_parser.add_argument("--issue-id", type=issue_id_type, required=True, help=HELP_ISSUE_ID)
    new_parser.set_defaults(tool=new_tool)

    # Create the parser for the `push` subcommand.
    push_parser = subparsers.add_parser("push", parents=[common_parser])
    push_test_case_input_group = push_parser.add_mutually_exclusive_group(required=True)
    push_test_case_input_group.add_argument("-n", "--test-case-name", help=HELP_TEST_CASE_NAME)
    push_test_case_input_group.add_argument("-r", "--test-case-file", type=Path, help=HELP_TEST_CASE_FILE)
    push_path_type_group = push_parser.add_mutually_exclusive_group(required=True)
    push_path_type_group.add_argument("--case", dest="path_type", action="store_const", const=PathType.INPUT)
    push_path_type_group.add_argument("--reference", dest="path_type", action="store_const", const=PathType.REFERENCE)
    push_parser.add_argument(
        "--allow-create-and-delete", action="store_true", default=False, help=HELP_ALLOW_CREATE_AND_DELETE
    )
    push_parser.add_argument("--issue-id", type=issue_id_type, required=True, help=HELP_ISSUE_ID)
    push_parser.set_defaults(tool=push_tool)

    # Create the parser for the `pull` subcommand.
    pull_parser = subparsers.add_parser("pull", parents=[common_parser])
    pull_test_case_input_group = pull_parser.add_mutually_exclusive_group(required=True)
    pull_test_case_input_group.add_argument("-n", "--test-case-name", help=HELP_TEST_CASE_NAME)
    pull_test_case_input_group.add_argument("-r", "--test-case-file", type=Path, help=HELP_TEST_CASE_FILE)
    pull_path_type_group = pull_parser.add_mutually_exclusive_group(required=True)
    pull_path_type_group.add_argument("--case", dest="path_type", action="store_const", const=PathType.INPUT)
    pull_path_type_group.add_argument("--reference", dest="path_type", action="store_const", const=PathType.REFERENCE)
    pull_parser.add_argument("-t", "--timestamp", type=utc_timestamp, required=False, help=HELP_TIMESTAMP)
    pull_parser.add_argument("--latest", action="store_true", default=False, help=HELP_LATEST)
    pull_parser.set_defaults(tool=pull_tool)

    # Create the parser for the `update-references` subcommand.
    update_refs_parser = subparsers.add_parser("update-references", parents=[common_parser])
    update_refs_test_case_input_group = update_refs_parser.add_mutually_exclusive_group(required=True)
    update_refs_test_case_input_group.add_argument("-n", "--test-case-name", help=HELP_TEST_CASE_NAME)
    update_refs_test_case_input_group.add_argument("-r", "--test-case-file", type=Path, help=HELP_TEST_CASE_FILE)
    update_refs_parser.add_argument("--issue-id", type=issue_id_type, required=True, help=HELP_ISSUE_ID)
    update_refs_parser.set_defaults(tool=update_references_tool)

    return parser


def make_minio_tool(namespace: argparse.Namespace) -> MinioTool:
    """Make an instance of `MinioTool` based on the parsed command line arguments."""
    # Configure logging
    logger = logging.getLogger("minio")
    logger.addHandler(logging.StreamHandler(sys.stdout))
    logger.setLevel(namespace.log_level or os.environ.get("LOG_LEVEL", "INFO"))

    prompter: prompt.Prompt
    if namespace.interactive:
        prompter = prompt.InteractivePrompt()
    else:
        prompter = prompt.DefaultPrompt(force_yes=namespace.force)

    index_builder = config.ConfigIndexBuilder.from_default_settings()
    configs: Iterable[Path]
    if namespace.config is not None:
        configs = [namespace.config]
    else:
        configs_dir = Path("configs")
        configs = set(configs_dir.rglob("*.xml")) - set(configs_dir.rglob("include/**/*.xml"))

    tags: Tags | None = None
    issue_id: str | None = getattr(namespace, "issue_id", None)
    if issue_id is not None:
        tags = Tags.new_object_tags()
        tags["jira-issue-id"] = issue_id
    color_output: bool = namespace.color_output

    url = urlparse(namespace.endpoint_url)
    if not url.scheme:
        raise error.MinioCliArgsError(
            "Invalid endpoint url. Must start with scheme (e.g. 'https://<hostname>' or 'http://<hostname>')",
            {"--endpoint-url": namespace.endpoint_url},
        )

    minio_client = Minio(
        endpoint=str(url.netloc),
        credentials=CredentialHandler(profile=namespace.profile).get_credentials(),
        secure=(url.scheme != "http"),
    )
    return MinioTool(
        bucket=namespace.bucket,
        rewinder=Rewinder(minio_client, logger),  # type: ignore
        test_case_index=index_builder.build_index(configs),
        test_case_writer=TestBenchConfigWriter(),
        prompt=prompter,
        tags=tags,
        color=color_output,
    )


def new_tool(args: argparse.Namespace) -> None:
    """Parse command line arguments and call the new command on the MinioTool."""
    tool = make_minio_tool(args)
    config_path: Path | None = args.config
    if not config_path:
        raise error.MinioCliArgsError("You must specify a config file.", {"--config": args.config})

    local_path: Path | None = args.local_path
    if local_path is None or not local_path.is_dir():
        raise error.MinioCliArgsError(
            "You must specify a path to a local directory with the test case data.",
            {"--local-path": local_path},
        )

    case_prefix: S3Path | None = args.case_prefix
    reference_prefix: S3Path | None = args.reference_prefix
    if (case_prefix is None) ^ (reference_prefix is None):
        raise error.MinioCliArgsError(
            "You must specify a remote prefix for both the case and reference files or neither.",
            {"--case-prefix": case_prefix, "--reference-prefix": reference_prefix},
        )
    elif case_prefix and reference_prefix:
        bucket: S3Path = args.bucket
        case_prefix = case_prefix.with_bucket(bucket.name)
        reference_prefix = reference_prefix.with_bucket(bucket.name)

    with catch_minio_errors():
        tool.new(local_path, config_path, args.test_case_name, case_prefix, reference_prefix)


def push_tool(args: argparse.Namespace) -> None:
    """Parse command line arguments and call the push command on the MinioTool."""
    tool = make_minio_tool(args)
    with catch_minio_errors():
        for pattern in get_patterns(args.test_case_name, args.test_case_file):
            tool.push(pattern, args.path_type, args.local_path, args.allow_create_and_delete)


def update_references_tool(args: argparse.Namespace) -> None:
    """Parse command line arguments and call the update_references command on the MinioTool."""
    tool = make_minio_tool(args)
    with catch_minio_errors():
        for pattern in get_patterns(args.test_case_name, args.test_case_file):
            tool.update_references(pattern, args.local_path)


def pull_tool(args: argparse.Namespace) -> None:
    """Parse command line arguments and call the pull command on the MinioTool."""
    tool = make_minio_tool(args)

    timestamp: datetime | None = args.timestamp
    if args.latest and timestamp is None:
        timestamp = datetime.now(timezone.utc)

    with catch_minio_errors():
        for pattern in get_patterns(args.test_case_name, args.test_case_file):
            tool.pull(pattern, args.path_type, args.local_path, timestamp)


def get_patterns(test_case_name: str | None, test_case_file: Path | None) -> Sequence[config.TestCasePattern]:
    """Get the test case patterns based on the provided name or file."""
    if test_case_name:
        return [config.TestCasePattern(name_filter=test_case_name)]
    elif test_case_file:
        with test_case_file.open("r") as text_io:
            return list(config.TestCasePattern.read_patterns_from_file(text_io))
    else:
        raise error.MinioCliArgsError(
            "You must specify either a test case name or a test case file.",
            {"--test-case-name": test_case_name, "--test-case-file": str(test_case_file)},
        )


@contextmanager
def catch_minio_errors() -> Iterator[None]:
    """Context manager to catch MinIO errors and raise MinioToolError."""
    try:
        yield
    except S3Error as exc:
        raise error.MinioAuthError(f"MinIO client error ({exc.code}): {exc.message}") from exc
    except MinioException as exc:
        raise error.MinioToolError(f"MinIO client error: {exc.args}") from exc
