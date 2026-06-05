import io
import textwrap
from datetime import datetime, timedelta, timezone
from pathlib import Path

import pytest
from pyfakefs.fake_filesystem import FakeFilesystem
from pytest_mock import MockerFixture
from s3_path_wrangler.paths import S3Path

from src.config.local_paths import LocalPaths
from src.config.types.path_type import PathType
from src.utils.xml_config_parser import XmlConfig, XmlConfigParser
from test.helpers import minio_tool as helper
from tools.minio.config import (
    ConfigData,
    ConfigParser,
    TestBenchConfigWriter,
    TestCaseData,
    TestCaseId,
    TestCaseIndex,
    TestCasePattern,
)
from tools.minio.error import MinioToolError


class TestTestCaseData:
    @pytest.mark.parametrize("ref_from_path", ["lnx64", "win64"])
    def test_from_config(self, ref_from_path: str) -> None:
        # Arrange
        local_paths = helper.make_local_paths()
        name = "e999_c42_foo"
        prefix = "e999_minio/c42_foo"
        config = helper.make_test_case_config(
            name,
            prefix,
            root="s3://my-bucket",
            reference_from_path=ref_from_path,
            max_run_time=42.0,
        )

        # Act
        result = TestCaseData.from_config(config, local_paths)

        # Assert
        assert result == TestCaseData(
            name="e999_c42_foo",
            case_dir=Path(local_paths.cases_path) / name,
            reference_dir=Path(local_paths.reference_path) / ref_from_path / name,
            case_prefix=S3Path.from_bucket("my-bucket") / "cases" / prefix,
            reference_prefix=S3Path.from_bucket("my-bucket") / "references" / ref_from_path / prefix,
            max_run_time=42.0,
        )

    def test_from_config__local_paths_have_backslashes_and_relative_notation(self) -> None:
        # Arrange
        local_paths = helper.make_local_paths()
        name = "e999_c42_foo"
        prefix = "e999_minio/c42_foo"
        config = helper.make_test_case_config(
            name,
            prefix,
            root="s3://my-bucket",
            cases_from_path=".\\",
            reference_from_path=".\\win64\\",
            max_run_time=42.0,
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
            max_run_time=42.0,
        )

    @pytest.mark.parametrize("path_type", [PathType.INPUT, PathType.REFERENCE])
    def test_get_default_local_dir(self, path_type: PathType) -> None:
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
        default_dir = test_case.get_default_local_directory(path_type)

        # Assert
        assert default_dir == test_case.case_dir if path_type == PathType.INPUT else test_case.reference_dir

    @pytest.mark.parametrize("path_type", [PathType.INPUT, PathType.REFERENCE])
    def test_get_remote_prefix(self, path_type: PathType) -> None:
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
        prefix = test_case.get_remote_prefix(path_type)

        # Assert
        # assert default_dir == test_case.case_dir if path_type == PathType.INPUT else test_case.reference_dir
        assert prefix == test_case.case_prefix if path_type == PathType.INPUT else test_case.reference_prefix


class TestTestCaseId:
    @pytest.mark.parametrize(
        ("name", "engine", "feature", "case", "trailer"),
        [
            pytest.param("e02_f102_c042_description", "e02", "f102", "c042", "_description", id="underscore"),
            pytest.param("e99_f012_c043-my.fav.testcase", "e99", "f012", "c043", "-my.fav.testcase", id="hyphen"),
        ],
    )
    def test_from_name(self, name: str, engine: str, feature: str, case: str, trailer: str) -> None:
        test_case_id = TestCaseId.from_name(name)
        assert test_case_id.engine_id == engine
        assert test_case_id.feature_id == feature
        assert test_case_id.case_id == case
        assert test_case_id.trailer == trailer
        assert str(test_case_id) == name
        assert test_case_id.identifier == f"{engine}_{feature}_{case}"

    @pytest.mark.parametrize(
        "name",
        [
            pytest.param("e02_c045_no_feature", id="no-feature"),
            pytest.param("e999_f42_c234.no_hyphen_or_underscore", id="no-hyphen-or-underscore"),
        ],
    )
    def test_from_name__failures(self, name: str) -> None:
        with pytest.raises(ValueError, match="Invalid"):
            TestCaseId.from_name(name)


class TestTestCasePattern:
    def test_read_patterns_from_file(self) -> None:
        # Arrange
        content = textwrap.dedent(
            """
            # Comments are ignored
            e02_f001_c01_foo,
                # Leading spaces and then a comment are ignored
            e02_f001_c02_bar
                e02_f001_c03_baz,*lnx64*
            # Look, you can also use spaces between the name and the comma:
            e02_f001_c04_qux    ,    *win64*
            """
        ).strip()

        patterns = list(TestCasePattern.read_patterns_from_file(io.StringIO(content)))

        assert len(patterns) == 4
        foo, bar, baz, qux = patterns

        assert foo.name_filter == "e02_f001_c01_foo"
        assert foo.config_glob == "*"

        assert bar.name_filter == "e02_f001_c02_bar"
        assert bar.config_glob == "*"

        assert baz.name_filter == "e02_f001_c03_baz"
        assert baz.config_glob == "*lnx64*"

        assert qux.name_filter == "e02_f001_c04_qux"
        assert qux.config_glob == "*win64*"

    def test_read_patterns_from_file__missing_name_filter__raise_value_error(self) -> None:
        # Arrange
        content = textwrap.dedent(
            """
            # Missing name filter
            bar,
            ,foo
            baz, *lnx64*
            """
        ).strip()

        with pytest.raises(ValueError, match="line 3"):
            list(TestCasePattern.read_patterns_from_file(io.StringIO(content)))


class TestTestCaseIndex:
    def test_find_test_case__no_glob__find_case_in_all_configs(self, mocker: MockerFixture) -> None:
        # Arrange
        test_cases = [helper.make_test_case(name) for name in ("bar", "baz", "foo")]
        paths = [
            Path("configs/foo/my_config.xml"),
            Path("configs/foo/my_second_config.xml"),
            Path("configs/hidden_config.xml"),
        ]
        config_parser = mocker.Mock(spec=ConfigParser)
        config_parser.parse_config.return_value = ConfigData(test_cases=test_cases)
        index = TestCaseIndex(paths, config_parser)

        # Act
        result = index.find_test_case(TestCasePattern(name_filter="foo"))

        # Assert
        assert all(path in result.configs for path in paths)
        assert result.test_case_data is not None
        assert result.test_case_data.name == "foo"

    def test_find_test_case__with_glob__find_case_in_matching_configs(self, mocker: MockerFixture) -> None:
        # Arrange
        test_cases = [helper.make_test_case(name) for name in ("bar", "baz", "foo")]
        paths = [
            Path("configs/foo/my_config.xml"),
            Path("configs/foo/my_second_config.xml"),
            Path("configs/hidden_config.xml"),
        ]
        config_parser = mocker.Mock(spec=ConfigParser)
        config_parser.parse_config.return_value = ConfigData(test_cases=test_cases)
        index = TestCaseIndex(paths, config_parser)

        # Act
        result = index.find_test_case(TestCasePattern(name_filter="foo", config_glob="foo/*.xml"))

        # Assert
        assert result.test_case_data is not None
        assert result.test_case_data.name == "foo"
        assert len(result.configs) == 2
        assert paths[-1] not in result.configs
        assert all(path in result.configs for path in paths[:2])

    def test_find_test_case__multiple_matches_in_config__raise_error(self, mocker: MockerFixture) -> None:
        # Arrange
        test_cases = [helper.make_test_case(name) for name in ("bar", "baz", "foo")]
        config_parser = mocker.Mock(spec=ConfigParser)
        config_parser.parse_config.return_value = ConfigData(test_cases=test_cases)
        index = TestCaseIndex([Path("configs/just_one_config.xml")], config_parser)

        # Act, Assert
        with pytest.raises(MinioToolError, match="matches multiple test cases"):
            index.find_test_case(TestCasePattern(name_filter="ba"))

    def test_find_test_cases(self, mocker: MockerFixture) -> None:
        # Arrange
        config_parser = mocker.Mock(spec=ConfigParser)
        configs = [Path("configs/foo.xml"), Path("configs/bar.xml"), Path("configs/baz.xml")]
        config_parser.parse_config.side_effect = [
            ConfigData([helper.make_test_case(name) for name in ("bar", "baz")]),
            ConfigData([helper.make_test_case(name) for name in ("baz", "foo")]),
            ConfigData([helper.make_test_case(name) for name in ("foo", "bar")]),
        ]
        index = TestCaseIndex(configs, config_parser)

        # Act
        result = index.find_test_cases(
            [
                TestCasePattern(config_glob="foo.xml", name_filter="bar"),  # Find 'bar' in 'foo.xml'
                TestCasePattern(config_glob="ba*.xml", name_filter="foo"),  # Find 'foo' in both 'bar.xml' and 'baz.xml'
                TestCasePattern(config_glob="baz.xml", name_filter="ba"),  # Find 'bar' in 'baz.xml'
            ]
        )

        # Assert
        assert len(result) == 3
        foo_cases = result[Path("configs/foo.xml")]
        bar_cases = result[Path("configs/bar.xml")]
        baz_cases = result[Path("configs/baz.xml")]

        assert len(foo_cases) == 1
        assert foo_cases[0].name == "bar"

        assert len(bar_cases) == 1
        assert bar_cases[0].name == "foo"

        assert len(baz_cases) == 2
        assert sorted(case.name for case in baz_cases) == ["bar", "foo"]

    def test_get_config_data(self, mocker: MockerFixture) -> None:
        # Arrange
        config = Path("configs/my-config.xml")
        config_parser = mocker.Mock(spec=ConfigParser)
        config_parser.parse_config.return_value = ConfigData(
            test_cases=[helper.make_test_case("foo")],
            default_test_cases=[helper.make_default_test_case(name, max_run_time=42.0) for name in ("bar", "baz")],
        )
        index = TestCaseIndex([config], config_parser)

        # Act
        config_data = index.get_config_data(config)

        # Assert
        assert config_data
        default_cases = config_data.default_test_cases
        assert len(default_cases) == 2
        assert default_cases[0].name == "bar"
        assert default_cases[0].max_run_time == 42.0
        assert default_cases[1].name == "baz"
        assert default_cases[1].max_run_time == 42.0


class TestConfigParser:
    def test_parse_config__wrong_first_tag__log_warning(self, fs: FakeFilesystem, mocker: MockerFixture) -> None:
        # Arrange
        fs.create_file(Path("configs/bad_config.xml"), contents="<wrongTag />")
        config_parser = helper.make_config_parser()

        with pytest.raises(ValueError, match="Not a testbench config"):
            config_parser.parse_config(Path("configs/bad_config.xml"))

    def test_parse_config(self, fs: FakeFilesystem, mocker: MockerFixture) -> None:
        # Arrange
        config_path = Path("configs/good_config.xml")
        fs.create_file(
            config_path,
            contents=textwrap.dedent("""
                <?xml version="1.0" encoding="iso-8859-1"?>
                <deltaresTestbench_v3 xmlns="http://schemas.deltares.nl/deltaresTestbench_v3" />
            """).strip(),
        )
        version = datetime.now(timezone.utc)
        test_case_config = helper.make_test_case_config("foo", "c999_minio_tool/c01_foo", version=version)
        xml_parser = mocker.Mock(spec=XmlConfigParser)
        xml_parser.default_cases = []
        xml_config = XmlConfig()
        xml_config.local_paths = LocalPaths()
        xml_config.program_configs = []
        xml_config.testcase_configs = [test_case_config]
        xml_parser.load.return_value = xml_config
        config_parser = helper.make_config_parser(xml_parser=xml_parser)

        # Act
        config_data = config_parser.parse_config(config_path)
        case, *other_cases = config_data.test_cases

        # Assert
        assert not other_cases
        assert case.name == "foo"
        assert case.case_dir == Path("cases/foo")
        assert case.reference_dir == Path("references/lnx64/foo")
        assert case.case_prefix == S3Path.from_bucket("my-bucket") / "cases/c999_minio_tool/c01_foo"
        assert case.reference_prefix == S3Path.from_bucket("my-bucket") / "references/lnx64/c999_minio_tool/c01_foo"
        assert case.version is not None
        assert abs((case.version - version).total_seconds()) < 1

    def test_parse_config__multiple_cases(self, fs: FakeFilesystem, mocker: MockerFixture) -> None:
        # Arrange
        config_path = Path("configs/my_config.xml")
        fs.create_file(config_path, contents="<deltaresTestbench_v3 />")

        test_cases = [helper.make_test_case_config(name, name) for name in ("foo", "bar", "baz")]
        default_cases = [helper.make_default_test_case("default")]

        xml_parser = mocker.Mock(spec=XmlConfigParser)
        xml_config = XmlConfig()
        xml_config.local_paths = LocalPaths()
        xml_config.program_configs = []
        xml_config.testcase_configs = test_cases
        xml_parser.load.return_value = xml_config
        xml_parser.default_cases = default_cases
        config_parser = helper.make_config_parser(xml_parser=xml_parser)

        # Act
        config_data = config_parser.parse_config(config_path)

        # Assert
        assert sorted(case.name for case in config_data.test_cases) == ["bar", "baz", "foo"]
        assert config_data.default_test_cases == default_cases


class TestTestBenchConfigWriter:
    def test_update_versions__single_test_case_without_version__single_line_change(self, fs: FakeFilesystem) -> None:
        # Arrange
        config_path = Path("configs/fake_config.xml")
        writer = TestBenchConfigWriter()
        now = datetime.now(timezone.utc).replace(microsecond=0)
        content = helper.make_config_content([("foo", None), ("bar", None), ("baz", now - timedelta(days=3))])
        fs.create_file(config_path, contents=content)

        # Act
        updates = writer.update_versions({"foo": now}, [config_path])

        # Assert
        assert len(updates) == 1
        new_content = updates.get(config_path, None)
        assert new_content is not None

        added_lines, removed_lines = helper.get_added_and_removed_lines(
            content.splitlines(keepends=True), new_content.readlines()
        )
        (added_line_nr, added_line), *other_added = added_lines
        (removed_line_nr, removed_line), *other_removed = removed_lines
        assert not other_added
        assert not other_removed
        assert f'<path version="{now.isoformat().split("+")[0]}">foo</path>' in added_line
        assert "<path>foo</path>" in removed_line
        assert added_line_nr == removed_line_nr

    def test_update_versions__single_test_case_with_version__single_line_change(self, fs: FakeFilesystem) -> None:
        # Arrange
        config_path = Path("configs/fake_config.xml")
        writer = TestBenchConfigWriter()
        now = datetime.now(timezone.utc).replace(microsecond=0)
        content = helper.make_config_content([("foo", None), ("bar", None), ("baz", now - timedelta(days=3))])
        fs.create_file(config_path, contents=content)

        # Act
        updates = writer.update_versions({"baz": now}, [config_path])

        # Assert
        assert len(updates) == 1
        new_content = updates.get(config_path, None)
        assert new_content is not None

        added_lines, removed_lines = helper.get_added_and_removed_lines(
            content.splitlines(keepends=True),
            new_content.readlines(),
        )
        (added_line_nr, added_line), *other_added = added_lines
        (removed_line_nr, removed_line), *other_removed = removed_lines
        assert not other_added
        assert not other_removed
        assert f'<path version="{now.isoformat().split("+")[0]}">baz</path>' in added_line
        assert f'<path version="{(now - timedelta(days=3)).isoformat().split("+")[0]}">baz</path>' in removed_line
        assert added_line_nr == removed_line_nr

    def test_update_versions__multiple_test_cases__multiple_line_changes(self, fs: FakeFilesystem) -> None:
        # Arrange
        config_path = Path("configs/fake_config.xml")
        writer = TestBenchConfigWriter()
        now = datetime.now(timezone.utc).replace(microsecond=0)
        content = helper.make_config_content([("foo", None), ("bar", None), ("baz", now - timedelta(days=3))])
        fs.create_file(config_path, contents=content)

        # Act
        result = writer.update_versions({"bar": now, "baz": now}, [config_path])

        # Assert
        assert len(result) == 1
        new_content = result.get(config_path, None)
        assert new_content is not None

        added_lines, removed_lines = helper.get_added_and_removed_lines(
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

    def test_update_versions__config_with_includes__update_included_config(self, fs: FakeFilesystem) -> None:
        # Arrange
        config_path = Path("configs/fake_config.xml")
        included_config_path = Path("configs/include/included_config.xml")
        writer = TestBenchConfigWriter()

        now = datetime.now(timezone.utc).replace(microsecond=0)
        config_content = textwrap.dedent("""
            <?xml version="1.0"?>
            <deltaresTestbench_v3 xmlns:xi="http://www.w3.org/2001/XInclude">
                <xi:include href="include/included_config.xml"/>
            </deltaresTestbench_v3>
            """).strip()
        fs.create_file(config_path, contents=config_content)

        included_content = textwrap.dedent(f"""
            <testCases>
                <testCase name="foo" ref="default">
                    <path version="{(now - timedelta(days=3)).isoformat().split("+")[0]}">foo</path>
                </testCase>
            </testCases>
            """).strip()
        fs.create_file(included_config_path, contents=included_content)

        # Act
        updates = writer.update_versions({"foo": now}, [config_path])

        # Assert
        assert included_config_path in updates
        new_content = updates.get(included_config_path)
        assert new_content is not None

        # Changes in included file.
        added_lines, removed_lines = helper.get_added_and_removed_lines(
            included_content.splitlines(keepends=True),
            new_content.readlines(),
        )
        (added_line_nr, added_line), *other_added = added_lines
        (removed_line_nr, removed_line), *other_removed = removed_lines
        assert not other_added
        assert not other_removed
        assert f'<path version="{now.isoformat().split("+")[0]}">foo</path>' in added_line
        assert f'<path version="{(now - timedelta(days=3)).isoformat().split("+")[0]}">foo</path>' in removed_line
        assert added_line_nr == removed_line_nr

        # No changes in config file.
        added_lines, removed_lines = helper.get_added_and_removed_lines(
            config_content.splitlines(keepends=True),
            updates[config_path].readlines(),
        )
        assert not added_lines
        assert not removed_lines

    def test_new_test_case__with_file_checks(self, fs: FakeFilesystem) -> None:
        # Arrange
        config_path = Path("configs/fake_config.xml")
        writer = TestBenchConfigWriter()
        now = datetime.now(timezone.utc).replace(microsecond=0)
        content = helper.make_config_content([("bar", None), ("foo", now - timedelta(days=3))])
        fs.create_file(config_path, contents=content)
        file_checks = [helper.make_file_check(f"{name}.nc", {name: 0.001}) for name in ("foo", "bar", "baz")]

        test_case = helper.make_test_case("baz", file_checks=file_checks, version=now)

        # Act
        result = writer.new_test_case(test_case, config_path)
        buffer = result.get(config_path)

        # Assert
        assert buffer is not None
        added_lines, removed_lines = helper.get_added_and_removed_lines(
            content.splitlines(keepends=True),
            buffer.readlines(),
        )

        assert not removed_lines
        assert any('<parameter name="foo" toleranceAbsolute="0.001" />' in line for _, line in added_lines)
        assert any('<parameter name="bar" toleranceAbsolute="0.001" />' in line for _, line in added_lines)
        assert any('<parameter name="baz" toleranceAbsolute="0.001" />' in line for _, line in added_lines)

    def test_new_test_case__with_include_file(self, fs: FakeFilesystem) -> None:
        # Arrange
        config_path = Path("configs/config.xml")
        include_path = Path("configs/include/included_config.xml")
        writer = TestBenchConfigWriter()
        now = datetime.now(timezone.utc).replace(microsecond=0)

        config_content = textwrap.dedent("""
            <?xml version="1.0"?>
            <deltaresTestbench_v3 xmlns:xi="http://www.w3.org/2001/XInclude">
                <xi:include href="include/included_config.xml"/>
            </deltaresTestbench_v3>
            """).strip()
        fs.create_file(config_path, contents=config_content)

        include_content = textwrap.dedent("""
            <testCases>
            </testCases>
            """).strip()
        fs.create_file(include_path, contents=include_content)

        file_checks = [helper.make_file_check("foo.nc", {"foo": 0.001})]

        test_case = helper.make_test_case(
            "foo", file_checks=file_checks, version=now, max_run_time=42.0, default_testcase="my-default"
        )

        # Act
        result = writer.new_test_case(test_case, config_path)
        include_buffer = result.get(include_path)
        config_buffer = result.get(config_path)

        # Assert
        assert include_buffer is not None
        include_added_lines, _ = helper.get_added_and_removed_lines(
            include_content.splitlines(keepends=True),
            include_buffer.readlines(),
        )

        assert any('<testCase name="foo" ref="my-default">' in line for _, line in include_added_lines)
        assert any("<maxRunTime>42.0</maxRunTime>" in line for _, line in include_added_lines)
        assert any('<parameter name="foo" toleranceAbsolute="0.001" />' in line for _, line in include_added_lines)

        assert config_buffer is not None
        config_added_lines, config_removed_lines = helper.get_added_and_removed_lines(
            config_content.splitlines(keepends=True),
            config_buffer.readlines(),
        )
        assert not config_added_lines
        assert not config_removed_lines
