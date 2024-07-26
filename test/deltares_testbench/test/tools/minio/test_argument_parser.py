import argparse
from datetime import datetime, timezone
from typing import List, Union

import pytest

from src.config.types.path_type import PathType
from tools.minio.argument_parser import make_argument_parser


@pytest.fixture(scope="session")
def arg_parser() -> argparse.ArgumentParser:
    return make_argument_parser()


def assert_push_defaults(args: argparse.Namespace) -> None:
    assert args.color_output
    assert args.interactive
    assert not args.force
    assert args.local_path is None
    assert not args.allow_create_and_delete


def assert_pull_defaults(args: argparse.Namespace) -> None:
    assert args.color_output
    assert args.interactive
    assert not args.force
    assert args.local_path is None
    assert args.timestamp is None
    assert not args.latest


def assert_update_refs_defaults(args: argparse.Namespace) -> None:
    assert args.color_output
    assert args.interactive
    assert not args.force
    assert args.local_path is None


def test_non_existent_command__raise_system_exit(arg_parser: argparse.ArgumentParser) -> None:
    # Arrange, Act, Assert
    with pytest.raises(SystemExit):
        arg_parser.parse_args(["foo"])


@pytest.mark.parametrize(
    "argv",
    [
        ["push", "--case", "--test-case-name=foo", "--issue-id=FOO-123"],
        ["push", "--reference", "--config=path/config.xml", "--issue-id=FOO-123"],
        ["push", "-c=path/config.xml", "-n=foo", "--issue-id=FOO-123"],
        ["push", "--case", "-c=path/config.xml", "-n=foo"],
        ["update-references", "--test-case-name=foo", "--issue-id=FOO-123"],
        ["update-references", "--config=path/config.xml", "--issue-id=FOO-123"],
        ["update-references", "-c=path/config.xml", "-n=foo"],
        ["pull", "--case", "--test-case-name=foo"],
        ["pull", "--reference", "--config=path/config.xml"],
        ["pull", "-c=path/config.xml", "-n=foo"],
    ],
    ids=[
        "push-missing-config",
        "push-missing-name",
        "push-missing-path-type",
        "push-missing-issue-id",
        "update-refs-missing-config",
        "update-refs-missing-name",
        "update-refs-missing-issue-id",
        "pull-missing-config",
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
    ids=["no-color", "batch", "force", "local-path", "short-local-path", "allow-create-and-delete"],
    argvalues=[
        ("--no-color", "color_output", False),
        ("--batch", "interactive", False),
        ("--force", "force", True),
        ("--local-path=foo/bar", "local_path", "foo/bar"),
        ("-p=foo/bar", "local_path", "foo/bar"),
        ("--allow-create-and-delete", "allow_create_and_delete", True),
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
    ("flag", "attr_name", "attr_value"),
    ids=["no-color", "batch", "force", "local-path", "short-local-path"],
    argvalues=[
        ("--no-color", "color_output", False),
        ("--batch", "interactive", False),
        ("--force", "force", True),
        ("--local-path=foo/bar", "local_path", "foo/bar"),
        ("-p=foo/bar", "local_path", "foo/bar"),
    ],
)
def test_update_references__optional_arguments(
    flag: str, attr_name: str, attr_value: Union[bool, str], arg_parser: argparse.ArgumentParser
) -> None:
    # Arrange, Act, Assert
    argv = ["update-references", "-c=path/to/config", "-n=foo", "--issue-id=FOO-123", flag]
    args = arg_parser.parse_args(argv)
    assert getattr(args, attr_name) == attr_value


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
    ids=["no-color", "batch", "force", "local-path", "short-local-path", "timestamp", "timestamp-short", "latest"],
    argvalues=[
        ("--no-color", "color_output", False),
        ("--batch", "interactive", False),
        ("--force", "force", True),
        ("--local-path=foo/bar", "local_path", "foo/bar"),
        ("-p=foo/bar", "local_path", "foo/bar"),
        ("--timestamp=2024-04-17T12:00", "timestamp", "2024-04-17T12:00"),
        ("-t=2024-04-17T12:00", "timestamp", "2024-04-17T12:00"),
        ("--latest", "latest", True),
    ],
)
def test_pull__optional_arguments(
    flag: str, attr_name: str, attr_value: Union[bool, str], arg_parser: argparse.ArgumentParser
) -> None:
    # Arrange, Act, Assert
    argv = ["pull", "--reference", "-c=path/to/config", "-n=foo", flag]
    args = arg_parser.parse_args(argv)
    assert getattr(args, attr_name) == attr_value
