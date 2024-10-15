import argparse
import logging
import os
import re
import sys
from datetime import datetime, timezone
from pathlib import Path
from typing import Optional
from urllib.parse import urlparse

from minio import Minio
from minio.commonconfig import Tags
from minio.error import MinioException, S3Error

from src.config.types.path_type import PathType
from src.utils.handlers.credential_handler import CredentialHandler
from src.utils.minio_rewinder import Rewinder
from tools.minio import config, prompt
from tools.minio.config import TestBenchConfigWriter
from tools.minio.minio_tool import ErrorCode, MinioTool, MinioToolError

HELP_CONFIG = "Path to test bench config file."
HELP_TEST_CASE_FILE = "Relative path from the testbench root folder, contains a list of testcases to update."
HELP_TEST_CASE_NAME = (
    "Name of a test case in the config file. This can be a substring, but it should match exactly one test case."
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


def make_argument_parser() -> argparse.ArgumentParser:
    """Build `ArgumentParser` used to parse the command line arguments."""
    # Create top level parser.
    parser = argparse.ArgumentParser(prog="tools.minio")

    # Add common arguments.
    common_parser = argparse.ArgumentParser(add_help=False)
    common_parser.add_argument("-c", "--config", help=HELP_CONFIG)
    test_case_input_group = common_parser.add_mutually_exclusive_group(required=True)
    test_case_input_group.add_argument("-n", "--test-case-name", help=HELP_TEST_CASE_NAME)
    test_case_input_group.add_argument("-r", "--test-case-file", help=HELP_TEST_CASE_FILE)
    common_parser.add_argument("--color", dest="color_output", action="store_true", help=HELP_COLOR)
    common_parser.add_argument("--no-color", dest="color_output", action="store_false", help=HELP_NO_COLOR)
    common_parser.add_argument("-i", "--interactive", dest="interactive", action="store_true", help=HELP_INTERACTIVE)
    common_parser.add_argument("-b", "--batch", dest="interactive", action="store_false", help=HELP_BATCH)
    common_parser.add_argument("-f", "--force", action="store_true", help=HELP_FORCE)
    common_parser.add_argument("-p", "--local-path", required=False, help=HELP_LOCAL_PATH)
    common_parser.add_argument("--log-level", required=False, choices=["CRITICAL", "ERROR", "WARNING", "INFO", "DEBUG"])
    common_parser.add_argument("--bucket", required=False, help=HELP_BUCKET)
    common_parser.add_argument("--profile", required=False, help=HELP_PROFILE)
    common_parser.add_argument("--endpoint-url", required=False, help=HELP_ENDPOINT_URL)
    common_parser.set_defaults(
        endpoint_url=os.environ.get("AWS_ENDPOINT_URL", "https://s3.deltares.nl"),
        profile=os.environ.get("AWS_PROFILE"),
        bucket="dsc-testbench",
        log_level="INFO",
        color_output=True,
        interactive=True,
        force=False,
    )

    # Add subparsers for each 'command'.
    subparsers = parser.add_subparsers()

    # Create the parser for the `push` command.
    push_parser = subparsers.add_parser("push", parents=[common_parser])
    push_path_type_group = push_parser.add_mutually_exclusive_group(required=True)
    push_path_type_group.add_argument("--case", dest="path_type", action="store_const", const=PathType.INPUT)
    push_path_type_group.add_argument("--reference", dest="path_type", action="store_const", const=PathType.REFERENCE)
    push_parser.add_argument(
        "--allow-create-and-delete", action="store_true", default=False, help=HELP_ALLOW_CREATE_AND_DELETE
    )
    push_parser.add_argument("--issue-id", required=True, help=HELP_ISSUE_ID)
    push_parser.set_defaults(tool=push_tool)

    # Create the parser for the `pull` command.
    pull_parser = subparsers.add_parser("pull", parents=[common_parser])
    pull_path_type_group = pull_parser.add_mutually_exclusive_group(required=True)
    pull_path_type_group.add_argument("--case", dest="path_type", action="store_const", const=PathType.INPUT)
    pull_path_type_group.add_argument("--reference", dest="path_type", action="store_const", const=PathType.REFERENCE)
    pull_parser.add_argument("-t", "--timestamp", required=False, help=HELP_TIMESTAMP)
    pull_parser.add_argument("--latest", action="store_true", default=False, help=HELP_LATEST)
    pull_parser.set_defaults(tool=pull_tool)

    # Create the parser for the `update-references` command.
    update_refs_parser = subparsers.add_parser("update-references", parents=[common_parser])
    update_refs_parser.add_argument("--issue-id", required=True, help=HELP_ISSUE_ID)
    update_refs_parser.set_defaults(tool=update_references_tool)

    return parser


def make_minio_tool(namespace: argparse.Namespace) -> MinioTool:
    """Make an instance of `MinioTool` based on the parsed command line arguments."""
    if namespace.config and namespace.test_case_file:
        raise MinioToolError(
            "Argument [--config] provided with [--test-case-file]. These arguments are not compatible."
            + "Write your config filter with the testcase name(s) in the test case file, using a `,` seperator."
        )

    # Configure logging
    logger = logging.getLogger("minio")
    logger.addHandler(logging.StreamHandler(sys.stdout))
    logger.setLevel(namespace.log_level or os.environ.get("LOG_LEVEL", "INFO"))

    if namespace.config is None:
        logger.info("Indexing configs in directory `configs`. Parsing testcases and creating a Dict for processing.")
        indexer = config.ConfigIndexer(logger=logger, bucket=namespace.bucket)
    else:
        logger.info(f"Reading test cases from file: {namespace.config}")
        indexer = config.ConfigIndexer(configs=[Path(namespace.config)], bucket=namespace.bucket, logger=logger)

    prompter: prompt.Prompt = (
        prompt.InteractivePrompt() if namespace.interactive else prompt.DefaultPrompt(force_yes=namespace.force)
    )
    tags: Optional[Tags] = None
    issue_id: Optional[str] = getattr(namespace, "issue_id", None)
    if issue_id is not None:
        if not re.match(r"^[_A-Z]+-\d+$", issue_id):
            raise ValueError("Invalid JIRA issue id: Must match the pattern [_A-Z]+-[0-9]+")
        tags = Tags.new_object_tags()
        tags["jira-issue-id"] = issue_id
    color_output: bool = namespace.color_output

    url = urlparse(namespace.endpoint_url)
    if not url.scheme:
        raise ValueError(
            f"Invalid endpoint url '{namespace.endpoint_url}': Url must start with a scheme "
            "(e.g. 'https://<hostname>' or 'http://<hostname>')"
        )

    minio_client = Minio(
        endpoint=str(url.netloc),
        credentials=CredentialHandler(profile=namespace.profile).get_credentials(),
    )
    return MinioTool(
        rewinder=Rewinder(minio_client, logger),  # type: ignore
        test_case_writer=TestBenchConfigWriter(),
        indexed_configs=indexer.index_configs(),
        prompt=prompter,
        tags=tags,
        color=color_output,
    )


def push_tool(args: argparse.Namespace) -> None:
    """Parse command line arguments and call the push command on the MinioTool."""
    tool = make_minio_tool(args)
    path_type: PathType = args.path_type
    local_path = Path(args.local_path) if args.local_path else None

    cases: dict[str, list[str]] = {}
    if args.test_case_name:
        cases[args.test_case_name] = [args.config]
    elif args.test_case_file:
        cases = config.CaseListReader().read_cases_from_file(Path(args.test_case_file))

    try:
        for case, config_globs in cases.items():
            for config_glob in config_globs:
                tool.push(case, path_type, config_glob, local_path, args.allow_create_and_delete)
    except S3Error as exc:
        raise MinioToolError(f"{exc.code}: {exc.message}", ErrorCode.AUTH) from exc
    except MinioException as exc:
        raise MinioToolError(f"MinioException: {exc.args}") from exc


def pull_tool(args: argparse.Namespace) -> None:
    """Parse command line arguments and call the pull command on the MinioTool."""
    tool = make_minio_tool(args)
    path_type: PathType = args.path_type
    local_path = Path(args.local_path) if args.local_path else None
    latest: bool = args.latest
    timestamp: Optional[datetime] = None
    if args.timestamp and not latest:
        timestamp = datetime.fromisoformat(args.timestamp).replace(tzinfo=timezone.utc)

    cases: dict[str, list[str]] = {}
    if args.test_case_name:
        cases[args.test_case_name] = [args.config]
    elif args.test_case_file:
        cases = config.CaseListReader().read_cases_from_file(Path(args.test_case_file))

    try:
        for case, config_globs in cases.items():
            for config_glob in config_globs:
                tool.pull(case, path_type, config_glob, local_path, timestamp, latest)
    except S3Error as exc:
        raise MinioToolError(f"{exc.code}: {exc.message}", ErrorCode.AUTH) from exc
    except MinioException as exc:
        raise MinioToolError(f"MinIO error: {exc.args}") from exc


def update_references_tool(args: argparse.Namespace) -> None:
    """Parse command line arguments and call the update_references command on the MinioTool."""
    tool = make_minio_tool(args)
    local_path = Path(args.local_path) if args.local_path else None

    cases: dict[str, list[str]] = {}
    if args.test_case_name:
        cases[args.test_case_name] = [args.config]
    elif args.test_case_file:
        cases = config.CaseListReader().read_cases_from_file(Path(args.test_case_file))

    try:
        for case, config_globs in cases.items():
            for config_glob in config_globs:
                tool.update_references(case, config_glob, local_path)
    except S3Error as exc:
        raise MinioToolError(f"{exc.code}: {exc.message}", ErrorCode.AUTH) from exc
    except MinioException as exc:
        raise MinioToolError(f"MinioException: {exc.args}") from exc
