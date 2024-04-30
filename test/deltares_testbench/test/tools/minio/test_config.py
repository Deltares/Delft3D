import difflib
import textwrap
from datetime import datetime, timedelta, timezone
from pathlib import Path
from typing import List, Optional, Sequence, Tuple

import pytest
from pyfakefs.fake_filesystem import FakeFilesystem
from pytest_mock import MockerFixture
from s3_path_wrangler.paths import S3Path
from tools.minio.config import (
    TestBenchConfigLoader,
    TestBenchConfigWriter,
    TestCaseData,
)

from src.config.local_paths import LocalPaths
from src.config.location import Location
from src.config.test_case_config import TestCaseConfig
from src.config.test_case_path import TestCasePath
from src.config.types.path_type import PathType
from src.utils.xml_config_parser import XmlConfigParser


def make_location(
    type_: PathType,
    root: str = "s3://my-bucket",
    name: str = "",
    from_path: Optional[str] = None,
) -> Location:
    location = Location()
    location.type = type_
    location.root = root.rstrip("/") + ("/cases" if type_ == PathType.INPUT else "/references")
    location.name = name if name else type_.name
    location.from_path = from_path or "."
    return location


def make_config(
    name: str,
    prefix: str,
    root: str = "s3://my-bucket",
    version: Optional[datetime] = None,
    cases_from_path: Optional[str] = ".",
    reference_from_path: Optional[str] = "lnx64",
) -> TestCaseConfig:
    config = TestCaseConfig()
    config.name = name
    version_str = None if version is None else version.replace(microsecond=0).isoformat()
    config.path = TestCasePath(prefix=prefix, version=version_str)
    config.locations = [
        make_location(PathType.INPUT, root=root, from_path=cases_from_path),
        make_location(PathType.REFERENCE, root=root, from_path=reference_from_path),
    ]
    return config


def make_local_paths(
    cases_path: str = "./data/cases",
    reference_path: str = "./data/references",
    engines_path: str = "./data/engines",
) -> LocalPaths:
    paths = LocalPaths()
    paths.cases_path = cases_path
    paths.reference_path = reference_path
    paths.engines_path = engines_path
    return paths


class TestTestCaseData:
    @pytest.mark.parametrize("ref_from_path", ["lnx64", "win64"])
    def test_from_config(self, ref_from_path: str) -> None:
        # Arrange
        local_paths = make_local_paths()
        name = "e999_c42_foo"
        prefix = "e999_minio/c42_foo"
        config = make_config(name, prefix, root="s3://my-bucket", reference_from_path=ref_from_path)

        # Act
        result = TestCaseData.from_config(config, local_paths)

        # Assert
        assert result == TestCaseData(
            name="e999_c42_foo",
            case_dir=Path(local_paths.cases_path) / name,
            reference_dir=Path(local_paths.reference_path) / ref_from_path / name,
            case_prefix=S3Path.from_bucket("my-bucket") / "cases" / prefix,
            reference_prefix=S3Path.from_bucket("my-bucket") / "references" / ref_from_path / prefix,
        )

    def test_from_config__local_paths_have_backslashes_and_relative_notation(self) -> None:
        # Arrange
        local_paths = make_local_paths()
        name = "e999_c42_foo"
        prefix = "e999_minio/c42_foo"
        config = make_config(
            name,
            prefix,
            root="s3://my-bucket",
            cases_from_path=".\\",
            reference_from_path=".\\win64\\",
        )

        # Act
        result = TestCaseData.from_config(config, local_paths)

        # Assert
        assert result == TestCaseData(
            name="e999_c42_foo",
            case_dir=Path(local_paths.cases_path) / name,
            reference_dir=Path(local_paths.reference_path) / "win64" / name,
            case_prefix=S3Path.from_bucket("my-bucket") / "cases" / prefix,
            reference_prefix=S3Path.from_bucket("my-bucket") / "references" / "win64" / prefix,
        )

    @pytest.mark.parametrize("path_type", [PathType.INPUT, PathType.REFERENCE])
    def test_get_default_dir_and_prefix(self, path_type: PathType) -> None:
        # Arrange
        name = "e999_c42_foo"
        prefix = "e999_minio/c42_foo"
        test_case = TestCaseData(
            name=name,
            case_dir=Path("data/cases") / name,
            reference_dir=Path("data/references") / "win64" / name,
            case_prefix=S3Path.from_bucket("my-bucket") / "cases" / prefix,
            reference_prefix=S3Path.from_bucket("my-bucket") / "references" / "win64" / prefix,
        )

        # Act
        default_dir, prefix = test_case.get_default_dir_and_prefix(path_type)

        # Assert
        assert default_dir == test_case.case_dir if path_type == PathType.INPUT else test_case.reference_dir
        assert prefix == test_case.case_prefix if path_type == PathType.INPUT else test_case.reference_prefix


class TestTestBenchConfigLoader:
    def test_get_test_cases__with_filter__one_match(self, mocker: MockerFixture) -> None:
        # Arrange
        config_parser = mocker.Mock(spec=XmlConfigParser)
        path = Path("path/to/config.xml")
        loader = TestBenchConfigLoader(
            path,
            config_parser=config_parser,
            server_base_url="s3://my-bucket",
        )
        test_case_configs = [
            make_config(name, f"c999_minio_tool/c{i:02d}_{name}") for i, name in enumerate(("foo", "bar", "baz"), 1)
        ]
        local_paths = make_local_paths()
        s3_bucket = S3Path.from_bucket("my-bucket")
        cases_prefix = s3_bucket / "cases/c999_minio_tool"
        refs_prefix = s3_bucket / "references/lnx64/c999_minio_tool"
        expected = TestCaseData(
            name="foo",
            case_dir=Path(local_paths.cases_path) / "foo",
            reference_dir=Path(local_paths.reference_path) / "lnx64/foo",
            case_prefix=cases_prefix / "c01_foo",
            reference_prefix=refs_prefix / "c01_foo",
        )
        config_parser.load.return_value = [local_paths, None, test_case_configs]

        # Act
        first_result, *other_results = loader.get_test_cases("foo")

        # Assert
        assert first_result == expected
        assert not other_results

    def test_get_test_cases__with_filter__multiple_matches(self, mocker: MockerFixture) -> None:
        # Arrange
        config_parser = mocker.Mock(spec=XmlConfigParser)
        path = Path("path/to/config.xml")
        loader = TestBenchConfigLoader(
            path,
            config_parser=config_parser,
            server_base_url="s3://my-bucket",
        )
        test_case_configs = [
            make_config(name, f"c999_minio_tool/c{i:02d}_{name}") for i, name in enumerate(("foo", "bar", "baz"), 1)
        ]
        local_paths = make_local_paths()
        case_dir = Path(local_paths.cases_path)
        reference_dir = Path(local_paths.reference_path) / "lnx64"
        s3_bucket = S3Path.from_bucket("my-bucket")
        cases_prefix = s3_bucket / "cases/c999_minio_tool"
        refs_prefix = s3_bucket / "references/lnx64/c999_minio_tool"
        expected = [
            TestCaseData(
                name="bar",
                case_dir=case_dir / "bar",
                reference_dir=reference_dir / "bar",
                case_prefix=cases_prefix / "c02_bar",
                reference_prefix=refs_prefix / "c02_bar",
            ),
            TestCaseData(
                name="baz",
                case_dir=case_dir / "baz",
                reference_dir=reference_dir / "baz",
                case_prefix=cases_prefix / "c03_baz",
                reference_prefix=refs_prefix / "c03_baz",
            ),
        ]
        config_parser.load.return_value = [local_paths, None, test_case_configs]

        # Act
        result = list(loader.get_test_cases("ba"))

        # Assert
        assert result == expected

    def test_get_test_cases__with_filter__no_matches(self, mocker: MockerFixture) -> None:
        # Arrange
        config_parser = mocker.Mock(spec=XmlConfigParser)
        path = Path("path/to/config.xml")
        loader = TestBenchConfigLoader(
            path,
            config_parser=config_parser,
            server_base_url="s3://my-bucket",
        )
        test_case_configs = [
            make_config(name, f"c999_minio_tool/c{i:02d}_{name}") for i, name in enumerate(("foo", "bar", "baz"), 1)
        ]
        local_paths = make_local_paths()
        config_parser.load.return_value = [local_paths, None, test_case_configs]

        # Act
        result = list(loader.get_test_cases("qux"))

        # Assert
        assert not result

    def test_get_test_cases__no_filter__return_all(self, mocker: MockerFixture) -> None:
        # Arrange
        config_parser = mocker.Mock(spec=XmlConfigParser)
        path = Path("path/to/config.xml")
        loader = TestBenchConfigLoader(
            path,
            config_parser=config_parser,
            server_base_url="s3://my-bucket",
        )
        test_case_configs = [
            make_config(name, f"c999_minio_tool/c{i:02d}_{name}") for i, name in enumerate(("foo", "bar", "baz"), 1)
        ]
        local_paths = make_local_paths()
        case_dir = Path(local_paths.cases_path)
        reference_dir = Path(local_paths.reference_path) / "lnx64"
        s3_bucket = S3Path.from_bucket("my-bucket")
        cases_prefix = s3_bucket / "cases/c999_minio_tool"
        refs_prefix = s3_bucket / "references/lnx64/c999_minio_tool"
        expected = [
            TestCaseData(
                name="foo",
                case_dir=case_dir / "foo",
                reference_dir=reference_dir / "foo",
                case_prefix=cases_prefix / "c01_foo",
                reference_prefix=refs_prefix / "c01_foo",
            ),
            TestCaseData(
                name="bar",
                case_dir=case_dir / "bar",
                reference_dir=reference_dir / "bar",
                case_prefix=cases_prefix / "c02_bar",
                reference_prefix=refs_prefix / "c02_bar",
            ),
            TestCaseData(
                name="baz",
                case_dir=case_dir / "baz",
                reference_dir=reference_dir / "baz",
                case_prefix=cases_prefix / "c03_baz",
                reference_prefix=refs_prefix / "c03_baz",
            ),
        ]
        config_parser.load.return_value = [local_paths, None, test_case_configs]

        # Act
        result = list(loader.get_test_cases())

        # Assert
        assert result == expected


def make_config_content(name_version_pairs: List[Tuple[str, Optional[datetime]]]) -> str:
    """Generate simple XML config with some test cases with versioned paths."""

    content_list = []
    for name, version in name_version_pairs:
        if version is None:
            path = f"<path>{name}</path>"
        else:
            version_str = version.replace(microsecond=0).isoformat().split("+")[0]
            path = f'<path version="{version_str}">{name}</path>'

        content_list.append(f"""\
<testCase name="{name}" ref="default">
    {path}
</testCase>""")

    header = """\
<?xml version="1.0"?>
<deltaresTestbench_v3>
    <testCases>
"""
    trailer = """
    </testCases>
</deltaresTestbench_v3>"""

    return header + textwrap.indent("\n".join(content_list), 8 * " ") + trailer


def get_added_and_removed_lines(
    src: Sequence[str], dst: Sequence[str]
) -> Tuple[List[Tuple[int, str]], List[Tuple[int, str]]]:
    added_lines: List[Tuple[int, str]] = []
    removed_lines: List[Tuple[int, str]] = []
    differ = difflib.Differ()

    old_line_nr = 1
    new_line_nr = 1
    for diff_line in differ.compare(src, dst):
        prefix, line = diff_line[:2], diff_line[2:]

        if prefix == "- ":  # Line in old file, but not in new file.
            removed_lines.append((old_line_nr, line))
            old_line_nr += 1
        elif prefix == "+ ":  # Line not in old file, but in new file.
            added_lines.append((new_line_nr, line))
            new_line_nr += 1
        elif prefix == "? ":  # Info line, not present in old or new file, ignore.
            pass
        else:  # Line both in old file and in new file.
            old_line_nr += 1
            new_line_nr += 1

    return added_lines, removed_lines


class TestTestBenchConfigWriter:
    def test_config_updates__single_test_case_without_version__single_line_change(self, fs: FakeFilesystem) -> None:
        # Arrange
        config_path = Path("configs/fake_config.xml")
        writer = TestBenchConfigWriter(config_path)
        now = datetime.now(timezone.utc).replace(microsecond=0)
        content = make_config_content([("foo", None), ("bar", None), ("baz", now - timedelta(days=3))])
        fs.create_file(config_path, contents=content)

        # Act
        updates = writer.config_updates({"foo": now})

        # Assert
        assert len(updates) == 1
        new_content = updates.get(config_path, None)
        assert new_content is not None

        added_lines, removed_lines = get_added_and_removed_lines(
            content.splitlines(keepends=True), new_content.readlines()
        )
        (added_line_nr, added_line), *other_added = added_lines
        (removed_line_nr, removed_line), *other_removed = removed_lines
        assert not other_added and not other_removed
        assert f'<path version="{now.isoformat().split("+")[0]}">foo</path>' in added_line
        assert "<path>foo</path>" in removed_line
        assert added_line_nr == removed_line_nr

    def test_config_updates__single_test_case_with_version__single_line_change(self, fs: FakeFilesystem) -> None:
        # Arrange
        config_path = Path("configs/fake_config.xml")
        writer = TestBenchConfigWriter(config_path)
        now = datetime.now(timezone.utc).replace(microsecond=0)
        content = make_config_content([("foo", None), ("bar", None), ("baz", now - timedelta(days=3))])
        fs.create_file(config_path, contents=content)

        # Act
        updates = writer.config_updates({"baz": now})

        # Assert
        assert len(updates) == 1
        new_content = updates.get(config_path, None)
        assert new_content is not None

        added_lines, removed_lines = get_added_and_removed_lines(
            content.splitlines(keepends=True),
            new_content.readlines(),
        )
        (added_line_nr, added_line), *other_added = added_lines
        (removed_line_nr, removed_line), *other_removed = removed_lines
        assert not other_added and not other_removed
        assert f'<path version="{now.isoformat().split("+")[0]}">baz</path>' in added_line
        assert f'<path version="{(now - timedelta(days=3)).isoformat().split("+")[0]}">baz</path>' in removed_line
        assert added_line_nr == removed_line_nr

    def test_config_updates__multiple_test_cases__multiple_line_changes(self, fs: FakeFilesystem) -> None:
        # Arrange
        config_path = Path("configs/fake_config.xml")
        writer = TestBenchConfigWriter(config_path)
        now = datetime.now(timezone.utc).replace(microsecond=0)
        content = make_config_content([("foo", None), ("bar", None), ("baz", now - timedelta(days=3))])
        fs.create_file(config_path, contents=content)

        # Act
        result = writer.config_updates({"bar": now, "baz": now})

        # Assert
        assert len(result) == 1
        new_content = result.get(config_path, None)
        assert new_content is not None

        added_lines, removed_lines = get_added_and_removed_lines(
            content.splitlines(keepends=True),
            new_content.readlines(),
        )
        assert len(added_lines) == 2
        assert len(removed_lines) == 2

        for i, (name, old_timestamp) in enumerate([("bar", None), ("baz", now - timedelta(days=3))]):
            added_line_nr, added_line = added_lines[i]
            removed_line_nr, removed_line = removed_lines[i]

            old_version = "" if old_timestamp is None else f' version="{old_timestamp.isoformat().split("+")[0]}"'

            assert f"<path{old_version}>{name}</path>" in removed_line
            assert f'<path version="{now.isoformat().split("+")[0]}">{name}</path>' in added_line
            assert added_line_nr == removed_line_nr

    def test_config_updates__config_with_includes__update_included_config(self, fs: FakeFilesystem) -> None:
        # Arrange
        config_path = Path("configs/fake_config.xml")
        included_config_path = Path("configs/include/included_config.xml")
        writer = TestBenchConfigWriter(config_path)

        now = datetime.now(timezone.utc).replace(microsecond=0)
        config_content = """
<?xml version="1.0"?>
<deltaresTestbench_v3 xmlns:xi="http://www.w3.org/2001/XInclude">
    <xi:include href="include/included_config.xml"/>
</deltaresTestbench_v3>
        """
        fs.create_file(config_path, contents=config_content)

        included_content = f"""
<testCases>
    <testCase name="foo" ref="default">
        <path version="{(now - timedelta(days=3)).isoformat().split("+")[0]}">foo</path>
    </testCase>
</testCases>
        """
        fs.create_file(included_config_path, contents=included_content)

        # Act
        updates = writer.config_updates({"foo": now})

        # Assert
        assert included_config_path in updates
        new_content = updates.get(included_config_path)
        assert new_content is not None

        # Changes in included file.
        added_lines, removed_lines = get_added_and_removed_lines(
            included_content.splitlines(keepends=True),
            new_content.readlines(),
        )
        (added_line_nr, added_line), *other_added = added_lines
        (removed_line_nr, removed_line), *other_removed = removed_lines
        assert not other_added and not other_removed
        assert f'<path version="{now.isoformat().split("+")[0]}">foo</path>' in added_line
        assert f'<path version="{(now - timedelta(days=3)).isoformat().split("+")[0]}">foo</path>' in removed_line
        assert added_line_nr == removed_line_nr

        # No changes in config file.
        added_lines, removed_lines = get_added_and_removed_lines(
            config_content.splitlines(keepends=True),
            updates[config_path].readlines(),
        )
        assert not added_lines and not removed_lines
