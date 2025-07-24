import io
from datetime import datetime, timedelta, timezone
from pathlib import Path
from typing import List, Optional

import pytest
from minio.commonconfig import Tags
from pyfakefs.fake_filesystem import FakeFilesystem
from pytest_mock import MockerFixture
from s3_path_wrangler.paths import S3Path

from src.config.types.path_type import PathType
from src.utils.minio_rewinder import Plan, PlanItem, Rewinder, VersionPair
from test.helpers import minio_tool as helper
from tools.minio.config import IndexItem, TestCaseData, TestCaseIndex, TestCasePattern, TestCaseWriter
from tools.minio.error import MinioToolError
from tools.minio.minio_tool import Command, MultiInputParser, PlannerCommandParser
from tools.minio.prompt import Answer, Prompt


class TestMinioTool:
    def test_push__non_existent_test_case__raise_error(self) -> None:
        # Arrange
        test_case_index = TestCaseIndex({})
        minio_tool = helper.make_minio_tool(test_case_index=test_case_index)

        # Act
        with pytest.raises(MinioToolError, match="No test case found"):
            minio_tool.push(TestCasePattern(name_filter="foo"), PathType.INPUT)

    def test_push__multiple_matching_test_cases__raise_error(self) -> None:
        # Arrange
        test_case_index = TestCaseIndex(
            {Path("configs/foo.xml"): IndexItem([helper.make_test_case("foo"), helper.make_test_case("foobar")], [])}
        )
        minio_tool = helper.make_minio_tool(test_case_index=test_case_index)

        # Act
        with pytest.raises(MinioToolError, match="matches multiple"):
            minio_tool.push(
                TestCasePattern(name_filter="foo"),
                PathType.INPUT,
                local_dir=Path("local"),
            )

    def test_push__unsupported_path_type__raise_error(self) -> None:
        # Arrange
        test_case_index = TestCaseIndex({Path("configs/foo.xml"): IndexItem([helper.make_test_case("foo")], [])})
        minio_tool = helper.make_minio_tool(test_case_index=test_case_index)

        # Act
        with pytest.raises(ValueError, match="Unsupported path type"):
            minio_tool.push(TestCasePattern(name_filter="foo"), PathType.NONE)

    def test_push__no_changes__print_up_to_date_message(
        self, capsys: pytest.CaptureFixture, mocker: MockerFixture
    ) -> None:
        # Arrange
        local_dir = Path("local")
        test_case_index = TestCaseIndex({Path("configs/foo.xml"): IndexItem([helper.make_test_case("foo")], [])})
        rewinder = mocker.Mock(spec=Rewinder)
        rewinder.build_plan.return_value = Plan(
            local_dir=Path("local"),
            minio_prefix=S3Path.from_bucket("my-bucket") / "references",
            items=[],  # No changes.
        )
        minio_tool = helper.make_minio_tool(rewinder=rewinder, test_case_index=test_case_index)

        # Act
        minio_tool.push(TestCasePattern(name_filter="foo"), PathType.REFERENCE, local_dir=local_dir)

        # Assert
        cap = capsys.readouterr()
        assert "`local` is already up to date with `s3://my-bucket/references`" in cap.out

    def test_push__dont_apply_changes(
        self,
        capsys: pytest.CaptureFixture,
        mocker: MockerFixture,
        fs: FakeFilesystem,
    ) -> None:
        # Arrange
        bucket = S3Path.from_bucket("my-bucket")
        local_dir = Path("local")
        fs.create_file(local_dir / "foo.txt", contents="foo")
        rewinder = mocker.Mock(spec=Rewinder)
        rewinder.build_plan.return_value = Plan(
            local_dir=Path("local"),
            minio_prefix=bucket / "references",
            items=[PlanItem.create(local_dir / "foo.txt", bucket / "references/foo.txt")],  # No changes.
        )
        prompt = mocker.Mock(spec=Prompt)
        prompt.yes_no.return_value = False  # No, don't apply these changes.
        test_case_index = TestCaseIndex({Path("configs/foo.xml"): IndexItem([helper.make_test_case("foo")], [])})
        minio_tool = helper.make_minio_tool(
            bucket=bucket, rewinder=rewinder, prompt=prompt, test_case_index=test_case_index
        )

        # Act
        minio_tool.push(TestCasePattern(name_filter="foo"), PathType.REFERENCE, local_dir=local_dir)

        # Assert
        rewinder.build_plan.assert_called_once_with(
            src_dir=local_dir, dst_prefix=bucket / "references/win64/foo", tags=None, allow_create_and_delete=False
        )
        rewinder.execute_plan.assert_not_called()
        out_lines: List[str] = capsys.readouterr().out.splitlines()
        message = "The following files from `local` will be uploaded to `s3://my-bucket/references`"
        linenr = next((idx for idx, line in enumerate(out_lines) if message in line), None)
        assert linenr is not None
        assert linenr + 1 != len(out_lines)
        assert "foo.txt" in out_lines[linenr + 1]  # File name
        assert "3 B" in out_lines[linenr + 1]  # File size

    def test_push__add_tags_but_dont_apply_changes__print_tags(
        self, mocker: MockerFixture, capsys: pytest.CaptureFixture, fs: FakeFilesystem
    ) -> None:
        # Arrange
        local_dir = Path("local")
        fs.create_file(local_dir / "foo.txt", contents="foo")
        bucket = S3Path.from_bucket("my-bucket")
        index = TestCaseIndex({Path("configs/foo.xml"): IndexItem([helper.make_test_case("foo")], [])})
        rewinder = mocker.Mock(spec=Rewinder)
        tags = Tags()
        tags.update({"foo": "bar"})
        rewinder.build_plan.return_value = Plan(
            local_dir=Path("local"),
            minio_prefix=bucket / "references",
            items=[PlanItem.update(local_dir / "foo.txt", bucket / "references/foo.txt")],  # No changes.
            tags=tags,
        )
        prompt = mocker.Mock(spec=Prompt)
        prompt.yes_no.return_value = Answer.NO  # No, don't apply these changes.
        minio_tool = helper.make_minio_tool(
            test_case_index=index, rewinder=rewinder, prompt=prompt, tags={"foo": "bar"}
        )

        # Act
        minio_tool.push(
            TestCasePattern(name_filter="foo"), PathType.INPUT, local_dir=local_dir, allow_create_and_delete=True
        )

        # Assert
        rewinder.build_plan.assert_called_once_with(
            src_dir=local_dir,
            dst_prefix=bucket / "cases/foo",
            tags=tags,
            allow_create_and_delete=True,
        )
        rewinder.execute_plan.assert_not_called()
        cap = capsys.readouterr()
        assert "The following tags" in cap.out
        assert "foo=bar" in cap.out

    def test_push__apply_changes_dont_save_configs(
        self, mocker: MockerFixture, capsys: pytest.CaptureFixture, fs: FakeFilesystem
    ) -> None:
        # Arrange
        now = datetime.now(timezone.utc)
        local_dir = Path("local")
        config_path = Path("configs/config.xml")
        fs.create_file(local_dir / "foo.txt", contents="foo")
        fs.create_file(config_path, contents="old")
        bucket = S3Path.from_bucket("my-bucket")
        index = TestCaseIndex({config_path: IndexItem([helper.make_test_case("foo")], [])})
        plan = Plan(
            local_dir=Path("local"),
            minio_prefix=bucket / "references",
            items=[PlanItem.create(local_dir / "foo.txt", bucket / "references/foo.txt")],  # No changes.
        )
        rewinder = mocker.Mock(spec=Rewinder)
        rewinder.build_plan.return_value = plan
        rewinder.list_objects.return_value = [helper.make_object("references/foo.txt", last_modified=now)]
        prompt = mocker.Mock(spec=Prompt)
        prompt.yes_no.side_effect = [Answer.YES, Answer.NO]  # Yes, apply changes. No, don't save the configs.
        writer = mocker.Mock(spec=TestCaseWriter)
        writer.update_versions.return_value = {config_path: io.StringIO("new")}
        minio_tool = helper.make_minio_tool(
            bucket=bucket, test_case_index=index, rewinder=rewinder, prompt=prompt, test_case_writer=writer
        )

        # Act
        minio_tool.push(
            TestCasePattern(name_filter="foo"), PathType.REFERENCE, local_dir=local_dir, allow_create_and_delete=False
        )

        # Assert
        rewinder.build_plan.assert_called_once_with(
            src_dir=local_dir,
            dst_prefix=bucket / "references/win64/foo",
            tags=None,
            allow_create_and_delete=False,
        )
        rewinder.execute_plan.assert_called_once_with(plan)
        writer.update_versions.assert_called_once_with({"foo": now + timedelta(milliseconds=1)}, [config_path])

        cap = capsys.readouterr()
        assert "-old+new" in cap.out  # The diff is printed to the output.
        assert config_path.read_text() == "old"  # The config file's content is still old.

    def test_push__apply_changes_save_configs(
        self, mocker: MockerFixture, capsys: pytest.CaptureFixture, fs: FakeFilesystem
    ) -> None:
        # Arrange
        now = datetime.now(timezone.utc)
        local_dir = Path("local")
        config_path = Path("configs/config.xml")
        fs.create_file(local_dir / "foo.txt", contents="foo")
        fs.create_file(config_path, contents="old")
        bucket = S3Path.from_bucket("my-bucket")
        index = TestCaseIndex({config_path: IndexItem([helper.make_test_case("foo")], [])})
        rewinder = mocker.Mock(spec=Rewinder)
        rewinder.build_plan.return_value = Plan(
            local_dir=local_dir,
            minio_prefix=bucket / "references",
            items=[PlanItem.create(local_dir / "foo.txt", bucket / "references/foo.txt")],  # No changes.
        )
        rewinder.list_objects.return_value = [helper.make_object("references/foo.txt", last_modified=now)]
        prompt = mocker.Mock(spec=Prompt)
        prompt.yes_no.return_value = Answer.YES  # Yes to all prompts.
        writer = mocker.Mock(spec=TestCaseWriter)
        writer.update_versions.return_value = {config_path: io.StringIO("new")}
        minio_tool = helper.make_minio_tool(
            bucket=bucket, test_case_index=index, rewinder=rewinder, prompt=prompt, test_case_writer=writer
        )

        # Act
        minio_tool.push(TestCasePattern(name_filter="foo"), PathType.REFERENCE)

        # Assert
        cap = capsys.readouterr()
        assert "Unified diff of config files" in cap.out
        assert "-old+new" in cap.out  # The diff is printed to the output.
        assert config_path.read_text() == "new"  # The config file's content is still old.

    def test_push__conflicts_detected_dont_continue__return_before_build_plan(
        self, mocker: MockerFixture, capsys: pytest.CaptureFixture
    ) -> None:
        # Arrange
        now = datetime.now(timezone.utc)
        index = TestCaseIndex(
            {Path("configs/foo.xml"): IndexItem([helper.make_test_case("foo", version=now - timedelta(days=3))], [])}
        )
        rewinder = mocker.Mock(spec=Rewinder)
        rewinder.detect_conflicts.return_value = [
            VersionPair(
                rewinded_version=None,
                latest_version=helper.make_object(
                    "cases/foo/bar.txt", size=42 * 1024 * 1024, tags={"jira-issue-id": "FOO-123"}
                ),  # 42 MiB
            ),
        ]
        prompt = mocker.Mock(spec=Prompt)
        prompt.yes_no.return_value = Answer.NO  # Don't continue after detecting conflicts
        minio_tool = helper.make_minio_tool(rewinder=rewinder, test_case_index=index, prompt=prompt)

        # Act
        minio_tool.push(TestCasePattern(name_filter="foo"), PathType.INPUT)

        # Assert
        rewinder.build_plan.assert_not_called()  # Aborted before making the plan.
        cap = capsys.readouterr()
        out_lines: List[str] = cap.out.splitlines()
        message = "Conflicts detected."
        linenr = next((idx for idx, line in enumerate(out_lines) if message in line), None)
        assert linenr is not None
        remaining_lines = out_lines[linenr + 1 :]
        assert any(line.startswith("+ bar.txt") for line in remaining_lines)
        assert any("42 MiB" in line for line in remaining_lines)
        assert any("FOO-123" in line for line in remaining_lines)

    def test_push__conflicts_detected_and_apply_changes_and_save_configs(
        self, mocker: MockerFixture, capsys: pytest.CaptureFixture, fs: FakeFilesystem
    ) -> None:
        # Arrange
        local_dir = Path("local")
        config_path = Path("configs/config.xml")
        fs.create_file(local_dir / "foo.txt", contents="foo")
        fs.create_file(config_path, contents="old")
        bucket = S3Path.from_bucket("my-bucket")
        now = datetime.now(timezone.utc)

        test_case_index = TestCaseIndex(
            {config_path: IndexItem([helper.make_test_case("foo", version=now - timedelta(days=3))], [])}
        )

        rewinder = mocker.Mock(spec=Rewinder)
        rewinder.list_objects.return_value = [helper.make_object("references/foo.txt", last_modified=now)]
        rewinder.detect_conflicts.return_value = [
            VersionPair(
                rewinded_version=None,
                latest_version=helper.make_object(
                    "cases/bar.txt", size=42 * 1024 * 1024, tags={"jira-issue-id": "FOO-123"}
                ),  # 42 MiB
            ),
        ]
        rewinder.build_plan.return_value = Plan(
            local_dir=Path("local"),
            minio_prefix=bucket / "references",
            items=[PlanItem.create(local_dir / "foo.txt", bucket / "references/foo.txt")],  # No changes.
        )

        prompt = mocker.Mock(spec=Prompt)
        prompt.yes_no.return_value = Answer.YES  # Yes to all prompts.

        test_case_writer = mocker.Mock(spec=TestCaseWriter)
        test_case_writer.update_versions.return_value = {config_path: io.StringIO("new")}

        minio_tool = helper.make_minio_tool(
            bucket=bucket,
            rewinder=rewinder,
            test_case_index=test_case_index,
            test_case_writer=test_case_writer,
            prompt=prompt,
        )

        # Act
        minio_tool.push(TestCasePattern(name_filter="foo"), PathType.REFERENCE, local_dir=local_dir)

        # Assert
        cap = capsys.readouterr()
        assert "FOO-123" in cap.out
        assert "-old+new" in cap.out  # The diff is printed to the output.
        assert config_path.read_text() == "new"  # The config file's content is still old.

    def test_pull__non_existent_test_case__raise_error(self) -> None:
        # Arrange
        minio_tool = helper.make_minio_tool(test_case_index=TestCaseIndex({}))

        # Act
        with pytest.raises(MinioToolError, match="No test case found"):
            minio_tool.pull(TestCasePattern(name_filter="foo"), PathType.INPUT)

    def test_pull__multiple_matching_test_cases__raise_error(self) -> None:
        # Arrange
        test_case_index = TestCaseIndex(
            {Path("configs/foo.xml"): IndexItem([helper.make_test_case("foo"), helper.make_test_case("foobar")], [])}
        )
        minio_tool = helper.make_minio_tool(test_case_index=test_case_index)

        # Act
        with pytest.raises(MinioToolError, match="matches multiple"):
            minio_tool.pull(TestCasePattern(name_filter="foo"), PathType.INPUT)

    @pytest.mark.parametrize(
        ("path_type", "prefix"),
        [
            (PathType.INPUT, "cases"),
            (PathType.REFERENCE, "references/win64"),
        ],
    )
    def test_pull__directory_empty__download_files(
        self,
        path_type: PathType,
        prefix: str,
        mocker: MockerFixture,
        fs: FakeFilesystem,
    ) -> None:
        # Arrange
        local_dir = Path("local")
        fs.create_dir(local_dir)
        prompt = mocker.Mock(spec=Prompt)
        rewinder = mocker.Mock(spec=Rewinder)
        index = TestCaseIndex({Path("configs/foo.xml"): IndexItem([helper.make_test_case("foo")], [])})
        minio_tool = helper.make_minio_tool(rewinder=rewinder, test_case_index=index, prompt=prompt)

        # Act
        minio_tool.pull(TestCasePattern(name_filter="foo"), path_type, local_dir=local_dir)

        # Assert
        prompt.yes_no.assert_not_called()
        rewinder.download.assert_called_once_with("my-bucket", f"{prefix}/foo", local_dir, None)

    @pytest.mark.parametrize(
        ("path_type", "prefix", "rewind"),
        [
            (PathType.INPUT, "cases", None),
            (PathType.REFERENCE, "references/win64", datetime.now(timezone.utc)),
        ],
    )
    def test_pull__directory_not_empty__prompt_before_downloading_files(
        self,
        path_type: PathType,
        prefix: str,
        rewind: Optional[datetime],
        mocker: MockerFixture,
        fs: FakeFilesystem,
    ) -> None:
        # Arrange
        local_dir = Path("local")
        fs.create_file(local_dir / "bar.txt")
        index = TestCaseIndex({Path("configs/foo.xml"): IndexItem([helper.make_test_case("foo")], [])})
        rewinder = mocker.Mock(spec=Rewinder)
        rewinder.detect_conflicts.return_value = []
        prompt = mocker.Mock(spec=Prompt)
        prompt.yes_no.return_value = Answer.YES  # Yes, download files.
        minio_tool = helper.make_minio_tool(test_case_index=index, rewinder=rewinder, prompt=prompt)

        # Act
        minio_tool.pull(TestCasePattern(name_filter="foo"), path_type, local_dir=local_dir, timestamp=rewind)

        # Assert
        prompt.yes_no.assert_called_once()
        rewinder.download.assert_called_once_with("my-bucket", f"{prefix}/foo", local_dir, rewind)

    def test_pull__directory_not_empty_dont_download(self, fs: FakeFilesystem, mocker: MockerFixture) -> None:
        # Arrange
        test_case = helper.make_test_case("foo")
        fs.create_file(test_case.reference_dir / "foo.txt")
        index = TestCaseIndex({Path("configs/foo.xml"): IndexItem([test_case], [])})
        prompt = mocker.Mock(spec=Prompt)
        prompt.yes_no.return_value = Answer.NO  # No, don't download files.
        rewinder = mocker.Mock(spec=Rewinder)
        minio_tool = helper.make_minio_tool(test_case_index=index, rewinder=rewinder, prompt=prompt)

        # Act
        minio_tool.pull(TestCasePattern(name_filter="foo"), PathType.REFERENCE)

        # Assert
        prompt.yes_no.assert_called_once()
        rewinder.download.assert_not_called()

    def test_pull__unsupported_path_type__raise_error(self) -> None:
        # Arrange
        index = TestCaseIndex({Path("configs/foo.xml"): IndexItem([helper.make_test_case("foo")], [])})
        minio_tool = helper.make_minio_tool(test_case_index=index)

        # Act
        with pytest.raises(ValueError, match="Unsupported path type"):
            minio_tool.pull(TestCasePattern(name_filter="foo"), PathType.NONE)

    def test_pull__no_timestamp_no_version__skip_detect_conflicts(self, mocker: MockerFixture) -> None:
        # Arrange
        test_case = helper.make_test_case("foo")
        index = TestCaseIndex({Path("configs/foo.xml"): IndexItem([test_case], [])})
        rewinder = mocker.Mock(spec=Rewinder)
        minio_tool = helper.make_minio_tool(test_case_index=index, rewinder=rewinder)

        # Act
        minio_tool.pull(TestCasePattern(name_filter="foo"), PathType.REFERENCE)

        # Assert
        bucket = test_case.reference_prefix.bucket
        key = test_case.reference_prefix.key
        rewinder.detect_conflicts.assert_not_called()
        rewinder.download.assert_called_once_with(bucket, key, test_case.reference_dir, None)

    def test_pull__no_timestamp_with_version__detect_conflicts__no_conflict(
        self, fs: FakeFilesystem, mocker: MockerFixture
    ) -> None:
        # Arrange
        three_days_ago = datetime.now(timezone.utc) - timedelta(days=3)
        test_case = helper.make_test_case("foo", version=three_days_ago)
        index = TestCaseIndex({Path("configs/foo.xml"): IndexItem([test_case], [])})
        rewinder = mocker.Mock(spec=Rewinder)
        rewinder.detect_conflicts.return_value = []
        minio_tool = helper.make_minio_tool(test_case_index=index, rewinder=rewinder)

        # Act
        minio_tool.pull(TestCasePattern(name_filter="foo"), PathType.REFERENCE)

        # Assert
        bucket = test_case.reference_prefix.bucket
        key = test_case.reference_prefix.key
        rewinder.detect_conflicts.assert_called_once_with(
            test_case.reference_prefix, three_days_ago, add_tags_to_latest=True
        )
        rewinder.download.assert_called_once_with(bucket, key, test_case.reference_dir, three_days_ago)
        assert fs.exists(Path(test_case.reference_dir))

    def test_pull__no_timestamp_with_version__detect_conflicts__accept(
        self, mocker: MockerFixture, fs: FakeFilesystem, capsys: pytest.CaptureFixture
    ) -> None:
        # Arrange
        three_days_ago = datetime.now(timezone.utc) - timedelta(days=3)
        test_case = helper.make_test_case("foo", version=three_days_ago)
        index = TestCaseIndex({Path("configs/foo.xml"): IndexItem([test_case], [])})
        conflict = VersionPair(
            rewinded_version=None,
            latest_version=helper.make_object(
                "references/bar.txt", size=42 * 1024 * 1024, tags={"jira-issue-id": "FOO-123"}
            ),  # 42 MiB
        )
        rewinder = mocker.Mock(spec=Rewinder)
        rewinder.detect_conflicts.return_value = [conflict]
        prompt = mocker.Mock(spec=Prompt)
        prompt.yes_no.return_value = Answer.YES
        minio_tool = helper.make_minio_tool(test_case_index=index, rewinder=rewinder, prompt=prompt)

        # Act
        minio_tool.pull(TestCasePattern(name_filter="foo"), PathType.REFERENCE)

        # Assert
        captured = capsys.readouterr()
        assert "FOO-123" in captured.out
        bucket = test_case.reference_prefix.bucket
        key = test_case.reference_prefix.key
        rewinder.detect_conflicts.assert_called_once_with(
            "s3://my-bucket/references/win64/foo", three_days_ago, add_tags_to_latest=True
        )
        rewinder.download.assert_called_once_with(bucket, key, test_case.reference_dir, three_days_ago)
        assert fs.exists(Path(test_case.reference_dir))

    def test_pull__no_timestamp_with_version__detect_conflicts__decline(
        self, mocker: MockerFixture, fs: FakeFilesystem, capsys: pytest.CaptureFixture
    ) -> None:
        # Arrange
        three_days_ago = datetime.now(timezone.utc) - timedelta(days=3)
        test_case = helper.make_test_case("foo", version=three_days_ago)
        index = TestCaseIndex({Path("configs/foo.xml"): IndexItem([test_case], [])})
        conflict = VersionPair(
            rewinded_version=None,
            latest_version=helper.make_object(
                "cases/bar.txt", size=42 * 1024 * 1024, tags={"jira-issue-id": "FOO-123"}
            ),  # 42 MiB
        )
        rewinder = mocker.Mock(spec=Rewinder)
        rewinder.detect_conflicts.return_value = [conflict]
        prompt = mocker.Mock(spec=Prompt)
        prompt.yes_no.return_value = Answer.NO
        minio_tool = helper.make_minio_tool(test_case_index=index, rewinder=rewinder, prompt=prompt)

        # Act
        minio_tool.pull(TestCasePattern(name_filter="foo"), PathType.INPUT)

        # Assert
        captured = capsys.readouterr()
        assert "FOO-123" in captured.out
        rewinder.detect_conflicts.assert_called_once_with(
            "s3://my-bucket/cases/foo", three_days_ago, add_tags_to_latest=True
        )
        rewinder.download.assert_not_called()
        assert not fs.exists(Path(test_case.case_dir))

    def test_pull__with_timestamp_and_version__timestamp_takes_precedence(
        self, mocker: MockerFixture, fs: FakeFilesystem
    ) -> None:
        # Arrange
        now = datetime.now(timezone.utc)
        test_case = helper.make_test_case("foo", version=now - timedelta(days=3))
        index = TestCaseIndex({Path("configs/foo.xml"): IndexItem([test_case], [])})
        rewinder = mocker.Mock(spec=Rewinder)
        rewinder.detect_conflicts.return_value = []
        minio_tool = helper.make_minio_tool(test_case_index=index, rewinder=rewinder)

        # Act
        minio_tool.pull(TestCasePattern(name_filter="foo"), PathType.INPUT, timestamp=now - timedelta(days=42))

        # Assert
        prefix = test_case.case_prefix
        rewinder.detect_conflicts.assert_not_called()
        rewinder.download.assert_called_once_with(
            prefix.bucket,
            prefix.key,
            test_case.case_dir,
            now - timedelta(days=42),
        )
        assert fs.exists(Path(test_case.case_dir))

    def test_update_references__non_existent_test_case__raise_error(self) -> None:
        # Arrange
        index = TestCaseIndex({Path("configs/foo.xml"): IndexItem([], [])})
        minio_tool = helper.make_minio_tool(test_case_index=index)

        # Act
        with pytest.raises(MinioToolError, match="No test case found"):
            minio_tool.update_references(TestCasePattern(name_filter="foo"))

    def test_update_references__multiple_matching_test_cases__raise_error(self) -> None:
        # Arrange
        index = TestCaseIndex(
            {Path("configs/foo.xml"): IndexItem([helper.make_test_case("foo"), helper.make_test_case("foobar")], [])}
        )
        minio_tool = helper.make_minio_tool(test_case_index=index)

        # Act
        with pytest.raises(MinioToolError, match="matches multiple"):
            minio_tool.update_references(TestCasePattern(name_filter="foo"), local_dir=Path("local"))

    def test_update_references__no_changes__print_up_to_date_message(
        self, mocker: MockerFixture, capsys: pytest.CaptureFixture
    ) -> None:
        # Arrange
        local_dir = Path("local")
        index = TestCaseIndex({Path("configs/foo.xml"): IndexItem([helper.make_test_case("foo")], [])})
        rewinder = mocker.Mock(spec=Rewinder)
        rewinder.build_plan.return_value = Plan(
            local_dir=Path("local"),
            minio_prefix=S3Path.from_bucket("my-bucket") / "references",
            items=[],  # No changes.
        )
        minio_tool = helper.make_minio_tool(test_case_index=index, rewinder=rewinder)

        # Act
        minio_tool.update_references(TestCasePattern(name_filter="foo"), local_dir=local_dir)

        # Assert
        cap = capsys.readouterr()
        assert "`local` is already up to date with `s3://my-bucket/references`" in cap.out

    def test_update_references__dont_apply_changes(
        self, mocker: MockerFixture, capsys: pytest.CaptureFixture, fs: FakeFilesystem
    ) -> None:
        # Arrange
        local_dir = Path("local")
        fs.create_file(local_dir / "foo.txt", contents="foo")
        bucket = S3Path.from_bucket("my-bucket")
        index = TestCaseIndex({Path("configs/foo.xml"): IndexItem([helper.make_test_case("foo")], [])})
        rewinder = mocker.Mock(spec=Rewinder)
        rewinder.build_plan.return_value = Plan(
            local_dir=Path("local"),
            minio_prefix=bucket / "references",
            items=[PlanItem.update(local_dir / "foo.txt", bucket / "references/foo.txt")],  # No changes.
        )
        prompt = mocker.Mock(spec=Prompt)
        prompt.yes_no.return_value = False  # No, don't apply these changes.
        minio_tool = helper.make_minio_tool(bucket=bucket, test_case_index=index, rewinder=rewinder, prompt=prompt)

        # Act
        minio_tool.update_references(TestCasePattern(name_filter="foo"), local_dir=local_dir)

        # Assert
        rewinder.build_plan.assert_called_once_with(
            src_dir=local_dir, dst_prefix=bucket / "references/win64/foo", tags=None, allow_create_and_delete=False
        )
        rewinder.execute_plan.assert_not_called()
        out_lines: List[str] = capsys.readouterr().out.splitlines()
        message = "The following files from `local` will be uploaded to `s3://my-bucket/references`"
        linenr = next((idx for idx, line in enumerate(out_lines) if message in line), None)
        assert linenr is not None
        remaining_lines = out_lines[linenr + 1 :]
        assert "foo.txt" in remaining_lines[0]  # File name
        assert "3 B" in remaining_lines[0]  # File size

    def test_update_references__add_tags_but_dont_apply_changes__print_tags(
        self, mocker: MockerFixture, capsys: pytest.CaptureFixture, fs: FakeFilesystem
    ) -> None:
        # Arrange
        test_case = helper.make_test_case("foo")
        fs.create_file(test_case.case_dir / "foo.txt", contents="foo")
        index = TestCaseIndex({Path("configs/foo.xml"): IndexItem([test_case], [])})
        tags = Tags()
        tags.update({"foo": "bar"})
        rewinder = mocker.Mock(spec=Rewinder)
        rewinder.build_plan.return_value = Plan(
            local_dir=test_case.case_dir,
            minio_prefix=test_case.reference_prefix,
            items=[PlanItem.create(test_case.case_dir / "foo.txt", test_case.reference_prefix / "foo.txt")],
            tags=tags,
        )
        prompt = mocker.Mock(spec=Prompt)
        prompt.yes_no.return_value = Answer.NO  # No, don't apply these changes.
        minio_tool = helper.make_minio_tool(
            test_case_index=index, rewinder=rewinder, prompt=prompt, tags={"foo": "bar"}
        )

        # Act
        minio_tool.update_references(TestCasePattern(name_filter="foo"))

        # Assert
        rewinder.build_plan.assert_called_once_with(
            src_dir=test_case.case_dir,
            dst_prefix=test_case.reference_prefix,
            tags=tags,
            allow_create_and_delete=False,
        )
        rewinder.execute_plan.assert_not_called()
        cap = capsys.readouterr()
        assert "The following tags" in cap.out
        assert "foo=bar" in cap.out

    def test_update_references__apply_changes_dont_save_configs(
        self, mocker: MockerFixture, capsys: pytest.CaptureFixture, fs: FakeFilesystem
    ) -> None:
        # Arrange
        now = datetime.now(timezone.utc)
        local_dir = Path("local")
        config_path = Path("configs/config.xml")
        fs.create_file(local_dir / "foo.txt", contents="foo")
        fs.create_file(config_path, contents="old")
        bucket = S3Path.from_bucket("my-bucket")
        index = TestCaseIndex({config_path: IndexItem([helper.make_test_case("foo")], [])})
        plan = Plan(
            local_dir=Path("local"),
            minio_prefix=bucket / "references",
            items=[PlanItem.update(local_dir / "foo.txt", bucket / "references/foo.txt")],  # No changes.
        )
        rewinder = mocker.Mock(spec=Rewinder)
        rewinder.build_plan.return_value = plan
        rewinder.list_objects.return_value = [helper.make_object("references/foo.txt", last_modified=now)]
        prompt = mocker.Mock(spec=Prompt)
        prompt.yes_no.side_effect = [Answer.YES, Answer.NO]  # Yes, apply changes. No, don't save the configs.
        writer = mocker.Mock(spec=TestCaseWriter)
        writer.update_versions.return_value = {config_path: io.StringIO("new")}
        minio_tool = helper.make_minio_tool(
            bucket=bucket, test_case_index=index, rewinder=rewinder, prompt=prompt, test_case_writer=writer
        )

        # Act
        minio_tool.update_references(TestCasePattern(name_filter="foo"), local_dir=local_dir)

        # Assert
        rewinder.build_plan.assert_called_once_with(
            src_dir=local_dir,
            dst_prefix=bucket / "references/win64/foo",
            tags=None,
            allow_create_and_delete=False,
        )
        rewinder.execute_plan.assert_called_once_with(plan)
        writer.update_versions.assert_called_once_with({"foo": now + timedelta(milliseconds=1)}, [config_path])

        cap = capsys.readouterr()
        assert "-old+new" in cap.out  # The diff is printed to the output.
        assert config_path.read_text() == "old"  # The config file's content is still old.

    def test_update_references__apply_changes_save_configs(
        self, mocker: MockerFixture, capsys: pytest.CaptureFixture, fs: FakeFilesystem
    ) -> None:
        # Arrange
        now = datetime.now(timezone.utc)
        local_dir = Path("local")
        config_path = Path("configs/config.xml")
        fs.create_file(local_dir / "foo.txt", contents="foo")
        fs.create_file(config_path, contents="old")
        bucket = S3Path.from_bucket("my-bucket")
        index = TestCaseIndex({config_path: IndexItem([helper.make_test_case("foo")], [])})
        plan = Plan(
            local_dir=local_dir,
            minio_prefix=bucket / "references",
            items=[PlanItem.create(local_dir / "foo.txt", bucket / "references/foo.txt")],  # No changes.
        )
        rewinder = mocker.Mock(spec=Rewinder)
        rewinder.build_plan.return_value = plan
        rewinder.list_objects.return_value = [helper.make_object("references/foo.txt", last_modified=now)]
        prompt = mocker.Mock(spec=Prompt)
        prompt.yes_no.return_value = Answer.YES  # Yes to all prompts.
        writer = mocker.Mock(spec=TestCaseWriter)
        writer.update_versions.return_value = {config_path: io.StringIO("new")}
        minio_tool = helper.make_minio_tool(
            bucket=bucket, test_case_index=index, rewinder=rewinder, prompt=prompt, test_case_writer=writer
        )

        # Act
        minio_tool.update_references(TestCasePattern(name_filter="foo"))

        # Assert
        cap = capsys.readouterr()
        assert "Unified diff of config files" in cap.out
        assert "-old+new" in cap.out  # The diff is printed to the output.
        assert config_path.read_text() == "new"  # The config file's content is still old.

    def test_update_references__conflicts_detected_dont_continue__return_before_build_plan(
        self, mocker: MockerFixture, capsys: pytest.CaptureFixture
    ) -> None:
        # Arrange
        now = datetime.now(timezone.utc)
        index = TestCaseIndex(
            {Path("configs/foo.xml"): IndexItem([helper.make_test_case("foo", version=now - timedelta(days=3))], [])}
        )
        rewinder = mocker.Mock(spec=Rewinder)
        rewinder.detect_conflicts.return_value = [
            VersionPair(
                rewinded_version=None,
                latest_version=helper.make_object(
                    "references/win64/foo/bar.txt", size=42 * 1024 * 1024, tags={"jira-issue-id": "FOO-123"}
                ),  # 42 MiB
            ),
        ]
        prompt = mocker.Mock(spec=Prompt)
        prompt.yes_no.return_value = Answer.NO  # Don't continue after detecting conflicts
        minio_tool = helper.make_minio_tool(test_case_index=index, rewinder=rewinder, prompt=prompt)
        # Act
        minio_tool.update_references(TestCasePattern(name_filter="foo"))

        # Assert
        rewinder.build_plan.assert_not_called()  # Aborted before making the plan.
        cap = capsys.readouterr()
        out_lines: List[str] = cap.out.splitlines()
        message = "Conflicts detected."
        linenr = next((idx for idx, line in enumerate(out_lines) if message in line), None)
        assert linenr is not None
        remaining_lines = out_lines[linenr + 1 :]
        assert any(line.startswith("+ bar.txt") for line in remaining_lines)
        assert any("42 MiB" in line for line in remaining_lines)
        assert any("FOO-123" in line for line in remaining_lines)

    def test_update_references__conflicts_detected_and_apply_changes_and_save_configs(
        self, mocker: MockerFixture, capsys: pytest.CaptureFixture, fs: FakeFilesystem
    ) -> None:
        # Arrange
        local_dir = Path("local")
        config_path = Path("configs/config.xml")
        fs.create_file(local_dir / "foo.txt", contents="foo")
        fs.create_file(config_path, contents="old")
        bucket = S3Path.from_bucket("my-bucket")
        now = datetime.now(timezone.utc)
        index = TestCaseIndex(
            {config_path: IndexItem([helper.make_test_case("foo", version=now - timedelta(days=3))], [])}
        )
        rewinder = mocker.Mock(spec=Rewinder)
        rewinder.detect_conflicts.return_value = [
            VersionPair(
                rewinded_version=None,
                latest_version=helper.make_object(
                    "cases/bar.txt", size=42 * 1024 * 1024, tags={"jira-issue-id": "FOO-123"}
                ),  # 42 MiB
            ),
        ]
        plan = Plan(
            local_dir=Path("local"),
            minio_prefix=bucket / "references",
            items=[PlanItem.create(local_dir / "foo.txt", bucket / "references/foo.txt")],  # No changes.
        )
        rewinder.build_plan.return_value = plan
        rewinder.list_objects.return_value = [helper.make_object("references/foo.txt", last_modified=now)]
        prompt = mocker.Mock(spec=Prompt)
        prompt.yes_no.return_value = Answer.YES  # Yes to all prompts.
        writer = mocker.Mock(spec=TestCaseWriter)
        writer.update_versions.return_value = {config_path: io.StringIO("new")}
        minio_tool = helper.make_minio_tool(
            bucket=bucket, test_case_index=index, rewinder=rewinder, prompt=prompt, test_case_writer=writer
        )

        # Act
        minio_tool.update_references(TestCasePattern(name_filter="foo"), local_dir=local_dir)

        # Assert
        cap = capsys.readouterr()
        assert "FOO-123" in cap.out
        assert "-old+new" in cap.out  # The diff is printed to the output.
        assert config_path.read_text() == "new"  # The config file's content is still old.

    @pytest.mark.parametrize(
        ("local_dir", "new_test_case_name"),
        [
            pytest.param(Path("data/cases/my_dir"), "e42_f042_c042_new_test_case", id="use-test-case-name"),
            pytest.param(Path("data/cases/e42_f042_c042_new_test_case"), None, id="use-directory-name"),
        ],
    )
    def test_new__test_case_exists_already__raise_error(self, local_dir: Path, new_test_case_name: str | None) -> None:
        # Arrange
        config = Path("configs/my-config.xml")
        old_test_case_name = "e42_f042_c042_life_the_universe_and_everything"
        index = TestCaseIndex({config: IndexItem([helper.make_test_case(old_test_case_name)], [])})
        minio_tool = helper.make_minio_tool(test_case_index=index)

        # Act
        with pytest.raises(MinioToolError, match="already exists"):
            minio_tool.new(local_dir=local_dir, config_path=config, test_case_name=new_test_case_name)

    def test_new__engine_directory_does_not_exist__raise_error(self, mocker: MockerFixture) -> None:
        # Arrange
        config = Path("configs/my-config.xml")
        local_dir = Path("data/cases/e42_f042_c042_new_test_case")
        index = TestCaseIndex({config: IndexItem([], [])})

        rewinder = mocker.Mock(spec=Rewinder)
        rewinder.autocomplete_prefix.return_value = []

        minio_tool = helper.make_minio_tool(test_case_index=index, rewinder=rewinder)

        # Act
        with pytest.raises(ValueError, match="No test case found with the prefix"):
            minio_tool.new(local_dir=local_dir, config_path=config)

    def test_new__test_case_files_already_exist_in_minio__raise_error(self, mocker: MockerFixture) -> None:
        # Arrange
        config = Path("configs/my-config.xml")
        local_dir = Path("data/cases/e42_f042_c042_new_test_case")
        index = TestCaseIndex({config: IndexItem([], [])})

        rewinder = mocker.Mock(spec=Rewinder)

        def autocomplete_prefix(hint: S3Path) -> list[S3Path]:
            return [hint.parent / f"{hint.name}_foo"]

        rewinder.autocomplete_prefix.side_effect = autocomplete_prefix

        minio_tool = helper.make_minio_tool(test_case_index=index, rewinder=rewinder)

        # Act
        with pytest.raises(ValueError, match="already exists"):
            minio_tool.new(local_dir=local_dir, config_path=config)

    def test_new__test_case_name_ambiguous_engine__choose(
        self, mocker: MockerFixture, capsys: pytest.CaptureFixture
    ) -> None:
        # Arrange
        bucket = S3Path.from_bucket("my-bucket")
        config = Path("configs/my-config.xml")
        local_dir = Path("data/cases/e42_f042_c042_new_test_case")
        index = TestCaseIndex({config: IndexItem([], [])})

        rewinder = mocker.Mock(spec=Rewinder)

        def autocomplete_prefix(hint: S3Path) -> list[S3Path]:
            parent = hint.parent
            if hint.name == "e42":
                return [parent / f"{hint.name}_foo", parent / f"{hint.name}_bar"]
            elif hint.name == "f042":
                return [parent / f"{hint.name}_baz"]
            elif hint.name == "c042":
                return []
            else:
                raise RuntimeError("Impossible")

        rewinder.autocomplete_prefix.side_effect = autocomplete_prefix

        prompt = mocker.Mock(spec=Prompt)
        prompt.choose.side_effect = [
            bucket / "cases/e42_bar",
            bucket / "references/win64/e42_bar",
        ]

        minio_tool = helper.make_minio_tool(bucket=bucket, test_case_index=index, rewinder=rewinder, prompt=prompt)

        # Act
        with pytest.raises(MinioToolError):
            minio_tool.new(local_dir=local_dir, config_path=config, default_test_case_name="teapot")

        output: str = capsys.readouterr().out
        assert str(bucket / "cases/e42_bar/f042_baz/c042_new_test_case") in output
        assert str(bucket / "references/win64/e42_bar/f042_baz/c042_new_test_case") in output

    def test_new__choose_default_test_case(self, mocker: MockerFixture, capsys: pytest.CaptureFixture) -> None:
        # Arrange
        bucket = S3Path.from_bucket("my-bucket")
        config = Path("configs/my-config.xml")
        local_dir = Path("data/cases/e42_f042_c042_new_test_case")
        default_cases = [helper.make_default_test_case(name) for name in ("foo", "bar", "baz")]
        index = TestCaseIndex({config: IndexItem([], default_cases)})
        case_prefix = bucket / "cases/e42_f042_c042"
        reference_prefix = bucket / "references/e42_f042_c042"

        rewinder = mocker.Mock(spec=Rewinder)
        rewinder.build_plan.return_value = Plan(local_dir=local_dir, minio_prefix=case_prefix, items=[])

        prompt = mocker.Mock(spec=Prompt)
        prompt.choose.return_value = default_cases[1]
        prompt.input.return_value = (Command.CANCEL, None)

        minio_tool = helper.make_minio_tool(bucket=bucket, test_case_index=index, rewinder=rewinder, prompt=prompt)

        # Act
        with pytest.raises(MinioToolError, match="cancelled"):
            minio_tool.new(
                local_dir=local_dir, config_path=config, case_prefix=case_prefix, reference_prefix=reference_prefix
            )

        # Assert
        call = prompt.choose.call_args
        assert call
        assert call.args[1] == default_cases

    def test_new__successful_flow(self, mocker: MockerFixture, fs: FakeFilesystem) -> None:
        # Arrange
        now = datetime.now(timezone.utc)
        bucket = S3Path.from_bucket("my-bucket")
        config = Path("configs/my-config.xml")
        local_case_dir = Path("data/cases/e42_f042_c042_new_test_case")
        fs.create_file(config, contents="old")
        fs.create_file(local_case_dir / "foo.txt", contents="foo")
        fs.create_file(local_case_dir / "bar.txt", contents="bar")
        default_test_case = helper.make_default_test_case("default")
        index = TestCaseIndex({config: IndexItem([], [default_test_case])})
        case_prefix = bucket / "cases/e42_f042_c042"
        reference_prefix = bucket / "references/e42_f042_c042"

        case_plan = Plan(
            local_dir=local_case_dir,
            minio_prefix=case_prefix,
            items=[PlanItem.create(local_case_dir / "foo.txt", case_prefix / "foo.txt")],
            allow_create_and_delete=True,
        )
        reference_plan = Plan(
            local_dir=local_case_dir,
            minio_prefix=reference_prefix,
            items=[PlanItem.create(local_case_dir / "bar.txt", reference_prefix / "bar.txt")],
            allow_create_and_delete=True,
        )
        rewinder = mocker.Mock(spec=Rewinder)
        rewinder.list_objects.return_value = [helper.make_object("cases/e42_f042_c042/foo.txt", last_modified=now)]
        rewinder.build_plan.side_effect = [case_plan, reference_plan]

        prompt = mocker.Mock(spec=Prompt)
        prompt.input.return_value = (Command.OK, None)
        prompt.yes_no.return_value = Answer.YES
        writer = mocker.Mock(spec=TestCaseWriter)
        writer.new_test_case.return_value = {config: io.StringIO("new")}
        minio_tool = helper.make_minio_tool(
            bucket=bucket, test_case_index=index, rewinder=rewinder, prompt=prompt, test_case_writer=writer
        )

        # Act
        minio_tool.new(
            local_dir=local_case_dir,
            config_path=config,
            case_prefix=case_prefix,
            reference_prefix=reference_prefix,
            default_test_case_name=default_test_case.name,
        )

        # Assert
        assert rewinder.execute_plan.call_args_list[0] == mocker.call(case_plan)
        assert rewinder.execute_plan.call_args_list[1] == mocker.call(reference_plan)

        test_case = TestCaseData(
            name="e42_f042_c042_new_test_case",
            case_dir=local_case_dir,
            reference_dir=local_case_dir,
            case_prefix=case_prefix,
            reference_prefix=reference_prefix,
            version=now + timedelta(milliseconds=1),
            file_checks=[],
            default_test_case=default_test_case.name,
            max_run_time=default_test_case.max_run_time,
        )
        writer.new_test_case.assert_called_once_with(test_case, config)


class TestPlannerCommandInput:
    def test_parse__line_does_not_end_in_line_feed__cancel(self) -> None:
        parser = PlannerCommandParser([])
        command, argument = parser.parse("I'm pressing CTRL-D!")
        assert command == Command.CANCEL
        assert argument is None

    def test_parse__empty_line__ok(self) -> None:
        parser = PlannerCommandParser([])
        command, argument = parser.parse("  \n")
        assert command == Command.OK
        assert argument is None

    @pytest.mark.parametrize("input_command", [Command.IGNORE, Command.REFERENCE])
    def test_parse__command_with_argument(self, input_command: Command) -> None:
        parser = PlannerCommandParser([])
        command, argument = parser.parse(f"{input_command.value} *.nc\n")
        assert command == input_command
        assert argument == "*.nc"

    @pytest.mark.parametrize("input_command", [Command.IGNORE, Command.REFERENCE])
    def test_parse__command_with_argument__no_argument(self, input_command: Command) -> None:
        parser = PlannerCommandParser([])
        with pytest.raises(ValueError, match="requires"):
            parser.parse(f"{input_command.value}\n")

    @pytest.mark.parametrize("input_command", [Command.RESET, Command.CANCEL, Command.HELP, Command.SHOW, Command.OK])
    def test_parse__command_without_argument(self, input_command: Command) -> None:
        parser = PlannerCommandParser([])
        command, argument = parser.parse(f"{input_command.value}\n")
        assert command == input_command
        assert argument is None

    @pytest.mark.parametrize("input_command", [Command.RESET, Command.CANCEL, Command.HELP, Command.SHOW, Command.OK])
    def test_parse__command_without_argument__pass_argument_anyway(self, input_command: Command) -> None:
        parser = PlannerCommandParser([])
        with pytest.raises(ValueError, match="does not accept"):
            parser.parse(f"{input_command.value} argument\n")

    def test_complete(self) -> None:
        parser = PlannerCommandParser(["foo", "bar", "baz"])
        result = list(parser.complete("ba"))
        assert result == ["bar", "baz"]

    def test_complete__no_completions(self) -> None:
        parser = PlannerCommandParser(["foo", "bar", "baz"])
        result = list(parser.complete("q"))
        assert result == []


class TestMultiInput:
    def test_parse__no_newline(self) -> None:
        parser = MultiInputParser([])
        result = parser.parse("I'm pressing CTRL-D")
        assert result == []

    def test_parse__split_on_whitespace(self) -> None:
        parser = MultiInputParser([])
        result = parser.parse(" foo\tbar  baz \n")
        assert result == ["foo", "bar", "baz"]

    def test_complete(self) -> None:
        parser = MultiInputParser(["foo", "bar", "baz"])
        result = list(parser.complete("ba"))
        assert result == ["bar", "baz"]

    def test_complete__no_completions(self) -> None:
        parser = MultiInputParser(["foo", "bar", "baz"])
        result = list(parser.complete("q"))
        assert result == []
