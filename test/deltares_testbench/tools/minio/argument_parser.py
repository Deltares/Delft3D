import argparse
import logging
import os
import re
import sys
from datetime import datetime, timezone
from pathlib import Path
from typing import Optional

from minio import Minio
from minio.commonconfig import Tags
from minio.credentials.providers import AWSConfigProvider
from minio.error import MinioException, S3Error
from tools.minio import config, prompt
from tools.minio.minio_tool import ErrorCode, MinioTool, MinioToolError

from src.config.types.path_type import PathType
from src.utils.minio_rewinder import Rewinder

HELP_CONFIG = "Path to test bench config file"
HELP_TEST_CASE_NAME = (
    "Name of a test case in the config file. This can be a substring, but it should match exactly one test case."
)
HELP_COLOR = "Use color in the output"
HELP_NO_COLOR = "Don't use color in the output"
HELP_INTERACTIVE = "Use the interactive prompt to allow users to make decisions"
HELP_BATCH = "Turn on (non-interactive) batch mode. Makes the default decision"
HELP_FORCE = "Only in combination with batch mode: Ignore conflicts and always proceed"
HELP_LOCAL_PATH = "Path to local directory"
HELP_UPDATE_ONLY = "Don't create new files or remove files from MinIO. Only upload new versions of existing files."
HELP_ISSUE_ID = "Identifier for the JIRA issue related to this change. Format: '[A-Z]+-[0-9]+'"
HELP_TIMESTAMP = "Get past version of the objects in MinIO"
HELP_LATEST = "Get the latest version of the objects in MinIO"


def make_argument_parser() -> argparse.ArgumentParser:
    """Build `ArgumentParser` used to parse the command line arguments."""
    # Create top level parser.
    parser = argparse.ArgumentParser(prog="tools.minio")

    # Add common arguments.
    common_parser = argparse.ArgumentParser(add_help=False)
    common_parser.add_argument("-c", "--config", required=True, help=HELP_CONFIG)
    common_parser.add_argument("-n", "--test-case-name", required=True, help=HELP_TEST_CASE_NAME)
    common_parser.add_argument("--color", dest="color_output", action="store_true", help=HELP_COLOR)
    common_parser.add_argument("--no-color", dest="color_output", action="store_false", help=HELP_NO_COLOR)
    common_parser.add_argument("-i", "--interactive", dest="interactive", action="store_true", help=HELP_INTERACTIVE)
    common_parser.add_argument("-b", "--batch", dest="interactive", action="store_false", help=HELP_BATCH)
    common_parser.add_argument("-f", "--force", action="store_true", help=HELP_FORCE)
    common_parser.add_argument("-p", "--local-path", required=False, help=HELP_LOCAL_PATH)
    common_parser.add_argument("--log-level", required=False, choices=["CRITICAL", "ERROR", "WARNING", "INFO", "DEBUG"])
    common_parser.set_defaults(color_output=True, interactive=True, force=False)

    # Add subparsers for each 'command'.
    subparsers = parser.add_subparsers()

    # Create the parser for the `push` command.
    push_parser = subparsers.add_parser("push", parents=[common_parser])
    push_path_type_group = push_parser.add_mutually_exclusive_group(required=True)
    push_path_type_group.add_argument("--case", dest="path_type", action="store_const", const=PathType.INPUT)
    push_path_type_group.add_argument("--reference", dest="path_type", action="store_const", const=PathType.REFERENCE)
    push_parser.add_argument("-u", "--update-only", action="store_true", default=False, help=HELP_UPDATE_ONLY)
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
    config_path: Path = Path(namespace.config)
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

    # Configure logging
    logger = logging.getLogger("minio")
    logger.addHandler(logging.StreamHandler(sys.stdout))
    logger.setLevel(namespace.log_level or os.environ.get("LOG_LEVEL", "INFO"))

    minio_client = Minio(
        endpoint="s3.deltares.nl",
        credentials=AWSConfigProvider(
            filename=str(Path("~/.aws/credentials").expanduser()),
        ),
    )
    return MinioTool(
        rewinder=Rewinder(minio_client, logger),  # type: ignore
        test_case_loader=config.TestBenchConfigLoader(config_path),
        test_case_writer=config.TestBenchConfigWriter(config_path),
        prompt=prompter,
        tags=tags,
        color=color_output,
    )


def push_tool(args: argparse.Namespace) -> None:
    """Parse command line arguments and call the push command on the MinioTool."""
    tool = make_minio_tool(args)
    path_type: PathType = args.path_type
    local_path = Path(args.local_path) if args.local_path else None

    try:
        tool.push(args.test_case_name, path_type, local_path, args.update_only)
    except S3Error as exc:
        raise MinioToolError(f"{exc.code}: {exc.message}", ErrorCode.AUTH) from exc
    except MinioException as exc:
        raise MinioToolError(f"MinioException: {exc.args}") from exc


def pull_tool(args: argparse.Namespace):
    """Parse command line arguments and call the pull command on the MinioTool."""
    path_type: PathType = args.path_type
    local_path = Path(args.local_path) if args.local_path else None
    latest: bool = args.latest
    timestamp: Optional[datetime] = None
    if args.timestamp and not latest:
        timestamp = datetime.fromisoformat(args.timestamp).replace(tzinfo=timezone.utc)

    tool = make_minio_tool(args)
    try:
        tool.pull(args.test_case_name, path_type, local_path, timestamp, latest)
    except S3Error as exc:
        raise MinioToolError(f"{exc.code}: {exc.message}", ErrorCode.AUTH) from exc
    except MinioException as exc:
        raise MinioToolError(f"MinIO error: {exc.args}") from exc


def update_references_tool(args: argparse.Namespace):
    """Parse command line arguments and call the update_references command on the MinioTool."""
    tool = make_minio_tool(args)
    test_case_name: str = args.test_case_name
    local_path = Path(args.local_path) if args.local_path else None

    try:
        tool.update_references(test_case_name, local_path)
    except S3Error as exc:
        raise MinioToolError(f"{exc.code}: {exc.message}", ErrorCode.AUTH) from exc
    except MinioException as exc:
        raise MinioToolError(f"MinioException: {exc.args}") from exc
