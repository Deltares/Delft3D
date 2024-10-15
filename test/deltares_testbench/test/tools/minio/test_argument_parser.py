import argparse
import os
from datetime import datetime, timezone
from typing import List, Union

import pytest

from src.config.types.path_type import PathType
from tools.minio.argument_parser import make_argument_parser


@pytest.fixture(scope="session")
def arg_parser() -> argparse.ArgumentParser:
    return make_argument_parser()


def assert_defaults(args: argparse.Namespace) -> None:
    assert args.color_output
    assert args.interactive
    assert args.bucket == "dsc-testbench"
    assert args.endpoint_url == os.environ.get("AWS_ENDPOINT_URL", "https://s3.deltares.nl")
    assert args.profile == os.environ.get("AWS_PROFILE")
    assert args.local_path is None
    assert not args.force


def assert_push_defaults(args: argparse.Namespace) -> None:
    assert not args.allow_create_and_delete
    assert_defaults(args)


def assert_pull_defaults(args: argparse.Namespace) -> None:
    assert args.timestamp is None
    assert not args.latest
    assert_defaults(args)


def assert_update_refs_defaults(args: argparse.Namespace) -> None:
    assert_defaults(args)


def test_non_existent_command__raise_system_exit(arg_parser: argparse.ArgumentParser) -> None:
    # Arrange, Act, Assert
    with pytest.raises(SystemExit):
        arg_parser.parse_args(["foo"])


@pytest.mark.parametrize(
    "argv",
    [
        ["push", "--reference", "--config=path/config.xml", "--issue-id=FOO-123"],
        ["push", "-c=path/config.xml", "-n=foo", "--issue-id=FOO-123"],
        ["push", "--case", "-c=path/config.xml", "-n=foo"],
        ["update-references", "--config=path/config.xml", "--issue-id=FOO-123"],
        ["update-references", "-c=path/config.xml", "-n=foo"],
        ["pull", "--reference", "--config=path/config.xml"],
        ["pull", "-c=path/config.xml", "-n=foo"],
    ],
    ids=[
        "push-missing-name",
        "push-missing-path-type",
        "push-missing-issue-id",
        "update-refs-missing-name",
        "update-refs-missing-issue-id",
        "pull-missing-name",
        "pull-missing-path-type",
    ],
)
def test_missing_required__raise_system_exit(argv: List[str], arg_parser: argparse.ArgumentParser) -> None:
    with pytest.raises(SystemExit):
        arg_parser.parse_args(argv)


@pytest.mark.parametrize(
    ("path_type_flag", "path_type"),
    [("--case", PathType.INPUT), ("--reference", PathType.REFERENCE)],
)
def test_push__only_required(
    path_type_flag: str,
    path_type: PathType,
    arg_parser: argparse.ArgumentParser,
) -> None:
    # Arrange, Act
    argv = ["push", "-c=path/to/config", "-n=foo", "--issue-id=FOO-123", path_type_flag]
    args = arg_parser.parse_args(argv)

    # Assert
    assert args.config == "path/to/config"
    assert args.test_case_name == "foo"
    assert args.path_type == path_type
    assert args.issue_id == "FOO-123"
    assert_push_defaults(args)


@pytest.mark.parametrize(
    ("path_type_flag", "path_type"),
    [("--case", PathType.INPUT), ("--reference", PathType.REFERENCE)],
)
def test_push__only_required_long_opts(
    path_type_flag: str,
    path_type: PathType,
    arg_parser: argparse.ArgumentParser,
) -> None:
    # Arrange, Act
    argv = ["push", "--config=path/to/config", "--test-case-name=foo", "--issue-id=FOO-123", path_type_flag]
    args = arg_parser.parse_args(argv)

    # Assert
    assert args.config == "path/to/config"
    assert args.test_case_name == "foo"
    assert args.path_type == path_type
    assert args.issue_id == "FOO-123"
    assert_push_defaults(args)


@pytest.mark.parametrize(
    ("flag", "attr_name", "attr_value"),
    [
        pytest.param("--allow-create-and-delete", "allow_create_and_delete", True, id="allow-create-and-delete"),
    ],
)
def test_push__optional_arguments(
    flag: str, attr_name: str, attr_value: Union[bool, str], arg_parser: argparse.ArgumentParser
) -> None:
    # Arrange, Act
    argv = ["push", "-c=path/to/config", "-n=foo", "--case", "--issue-id=FOO-123", flag]
    args = arg_parser.parse_args(argv)

    # Assert
    assert getattr(args, attr_name) == attr_value


def test_update_refs__required_only(arg_parser: argparse.ArgumentParser) -> None:
    # Arrange, Act
    argv = ["update-references", "-c=path/to/config", "-n=foo", "--issue-id=FOO-123"]
    args = arg_parser.parse_args(argv)

    # Assert
    assert args.config == "path/to/config"
    assert args.test_case_name == "foo"
    assert args.issue_id == "FOO-123"
    assert_update_refs_defaults(args)


def test_update_refs__required_only__long_opts(arg_parser: argparse.ArgumentParser) -> None:
    # Arrange, Act
    argv = ["update-references", "--config=path/to/config", "--test-case-name=foo", "--issue-id=FOO-123"]
    args = arg_parser.parse_args(argv)

    # Assert
    assert args.config == "path/to/config"
    assert args.test_case_name == "foo"
    assert args.issue_id == "FOO-123"
    assert_update_refs_defaults(args)


@pytest.mark.parametrize(
    ("path_type_flag", "path_type"),
    [("--case", PathType.INPUT), ("--reference", PathType.REFERENCE)],
)
def test_pull__only_required(
    path_type_flag: str,
    path_type: PathType,
    arg_parser: argparse.ArgumentParser,
) -> None:
    # Arrange, Act
    argv = ["pull", "-c=path/to/config", "-n=foo", path_type_flag]
    args = arg_parser.parse_args(argv)

    # Assert
    assert args.config == "path/to/config"
    assert args.test_case_name == "foo"
    assert args.path_type == path_type
    assert_pull_defaults(args)


@pytest.mark.parametrize(
    ("path_type_flag", "path_type"),
    [("--case", PathType.INPUT), ("--reference", PathType.REFERENCE)],
)
def test_pull__only_required_long_opts(
    path_type_flag: str,
    path_type: PathType,
    arg_parser: argparse.ArgumentParser,
) -> None:
    # Arrange, Act
    argv = ["pull", "--config=path/to/config", "--test-case-name=foo", path_type_flag]
    args = arg_parser.parse_args(argv)

    # Assert
    assert args.config == "path/to/config"
    assert args.test_case_name == "foo"
    assert args.path_type == path_type
    assert_pull_defaults(args)


def test_pull__timestamp(arg_parser: argparse.ArgumentParser) -> None:
    # Arrange
    now = datetime.now(timezone.utc).replace(microsecond=0).isoformat().split("+")[0]
    argv = ["pull", "-c=path/to/config", "-n=foo", "--case", f"--timestamp={now}"]

    # Act
    args = arg_parser.parse_args(argv)

    # Assert
    assert args.timestamp == now


def test_pull__latest(arg_parser: argparse.ArgumentParser) -> None:
    # Arrange, Act, Assert
    argv = ["pull", "-c=path/to/config", "-n=foo", "--case", "--latest"]
    args = arg_parser.parse_args(argv)
    assert args.latest


@pytest.mark.parametrize(
    ("flag", "attr_name", "attr_value"),
    [
        pytest.param("--timestamp=2024-04-17T12:00", "timestamp", "2024-04-17T12:00", id="timestamp"),
        pytest.param("-t=2024-04-17T12:00", "timestamp", "2024-04-17T12:00", id="short-timestamp"),
        pytest.param("--latest", "latest", True, id="latest"),
    ],
)
def test_pull__optional_arguments(
    flag: str, attr_name: str, attr_value: Union[bool, str], arg_parser: argparse.ArgumentParser
) -> None:
    # Arrange, Act, Assert
    argv = ["pull", "--reference", "-c=path/to/config", "-n=foo", flag]
    args = arg_parser.parse_args(argv)
    assert getattr(args, attr_name) == attr_value


@pytest.mark.parametrize(
    ("flag", "attr_name", "attr_value"),
    [
        pytest.param("--no-color", "color_output", False, id="no-color"),
        pytest.param("--batch", "interactive", False, id="batch"),
        pytest.param("--force", "force", True, id="force"),
        pytest.param("--local-path=foo/bar", "local_path", "foo/bar", id="local-path"),
        pytest.param(
            "-p=foo/bar",
            "local_path",
            "foo/bar",
            id="short-local-path",
        ),
        pytest.param("--bucket=my-bucket", "bucket", "my-bucket", id="bucket"),
        pytest.param("--endpoint-url=https://my.s3:4242", "endpoint_url", "https://my.s3:4242", id="endpoint-url"),
        pytest.param("--profile=minio-super-admin", "profile", "minio-super-admin", id="profile"),
    ],
)
def test_optional_arguments(
    flag: str, attr_name: str, attr_value: Union[bool, str], arg_parser: argparse.ArgumentParser
) -> None:
    # Arrange, Act, Assert
    argv = ["pull", "--reference", "-c=path/to/config", "-n=foo", flag]
    args = arg_parser.parse_args(argv)
    assert getattr(args, attr_name) == attr_value
