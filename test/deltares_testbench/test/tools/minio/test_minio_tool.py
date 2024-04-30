import io
from datetime import datetime, timedelta, timezone
from pathlib import Path
from typing import Optional
from unittest.mock import Mock
from uuid import uuid4

import pytest
from minio.datatypes import Object as MinioObject
from pyfakefs.fake_filesystem import FakeFilesystem
from pytest import CaptureFixture, FixtureRequest
from pytest_mock import MockerFixture
from s3_path_wrangler.paths import S3Path
from tools.minio.config import TestCaseData, TestCaseLoader, TestCaseWriter
from tools.minio.minio_tool import MinioTool, MinioToolError
from tools.minio.prompt import Prompt

from src.config.types.path_type import PathType
from src.utils.minio_rewinder import Plan, PlanItem, Rewinder, VersionPair


@pytest.fixture
def rewinder(mocker: MockerFixture) -> Mock:
    return mocker.Mock(spec=Rewinder)


@pytest.fixture
def test_case_loader(mocker: MockerFixture) -> Mock:
    return mocker.Mock(spec=TestCaseLoader)


@pytest.fixture
def test_case_writer(mocker: MockerFixture) -> Mock:
    return mocker.Mock(spec=TestCaseWriter)


@pytest.fixture
def prompt(mocker: MockerFixture) -> Mock:
    return mocker.Mock(spec=Prompt)


@pytest.fixture
def minio_tool(
    request: FixtureRequest,
    rewinder: Mock,
    test_case_loader: Mock,
    test_case_writer: Mock,
    prompt: Mock,
) -> MinioTool:
    """Provide MinioTool with mocked rewinder, loader, etc."""
    color = False
    tags = None
    if getattr(request, "param", None):
        color = request.param.get("color", False)
        tags = request.param.get("tags")

    return MinioTool(
        rewinder=rewinder,
        test_case_loader=test_case_loader,
        test_case_writer=test_case_writer,
        prompt=prompt,
        tags=tags,
        color=color,
    )


def make_object(
    object_name: str,
    bucket_name: str = "my-bucket",
    version_id: Optional[str] = None,
    last_modified: datetime = datetime.min.replace(tzinfo=timezone.utc),
    is_delete_marker: bool = False,
    etag: Optional[str] = None,
    size: Optional[int] = None,
) -> MinioObject:
    version_id = version_id or uuid4().hex

    return MinioObject(
        bucket_name=bucket_name,
        object_name=object_name,
        version_id=version_id,
        last_modified=last_modified,
        is_delete_marker=is_delete_marker,
        etag=etag,
        size=size,
    )


def make_test_case(
    name: str,
    case_dir: Optional[Path] = None,
    reference_dir: Optional[Path] = None,
    case_prefix: Optional[S3Path] = None,
    reference_prefix: Optional[S3Path] = None,
    version: Optional[datetime] = None,
) -> TestCaseData:
    case_dir = case_dir or Path("data/cases") / name
    reference_dir = reference_dir or Path("data/references") / name
    default_bucket = S3Path.from_bucket("my-bucket")
    return TestCaseData(
        name=name,
        case_dir=case_dir,
        reference_dir=reference_dir,
        case_prefix=case_prefix or default_bucket / "cases",
        reference_prefix=reference_prefix or default_bucket / "references",
        version=version,
    )


class TestMinioTool:
    def test_push__non_existent_test_case__raise_error(
        self,
        minio_tool: MinioTool,
        test_case_loader: Mock,
    ) -> None:
        # Arrange
        test_case_loader.get_test_cases.return_value = []

        # Act
        with pytest.raises(MinioToolError, match="does not match"):
            minio_tool.push("foo", PathType.INPUT)

    def test_push__multiple_matching_test_cases__raise_error(
        self,
        minio_tool: MinioTool,
        test_case_loader: Mock,
    ) -> None:
        # Arrange
        test_case_loader.get_test_cases.return_value = [
            make_test_case("foo"),
            make_test_case("foobar"),
        ]

        # Act
        with pytest.raises(MinioToolError, match="matches multiple"):
            minio_tool.push("foo", PathType.INPUT, Path("local"))

    def test_push__unsupported_path_type__raise_error(
        self,
        minio_tool: MinioTool,
        test_case_loader: Mock,
    ) -> None:
        # Arrange
        test_case_loader.get_test_cases.return_value = [make_test_case("foo")]

        # Act
        with pytest.raises(ValueError, match="Unsupported path type"):
            minio_tool.push("foo", PathType.NONE)

    def test_push__test_case_has_version_timestamp_from_future__raise_error(
        self,
        minio_tool: MinioTool,
        test_case_loader: Mock,
    ) -> None:
        # Arrange
        now = datetime.now(timezone.utc)
        test_case_loader.get_test_cases.return_value = [make_test_case("foo", version=now + timedelta(seconds=43))]

        # Act
        with pytest.raises(MinioToolError, match="future"):
            minio_tool.push("foo", PathType.INPUT)

    def test_push__no_changes__print_up_to_date_message(
        self,
        capsys: CaptureFixture,
        minio_tool: MinioTool,
        test_case_loader: Mock,
        rewinder: Mock,
    ) -> None:
        # Arrange
        local_dir = Path("local")
        test_case_loader.get_test_cases.return_value = [make_test_case("foo")]
        rewinder.build_plan.return_value = Plan(
            local_dir=Path("local"),
            minio_prefix=S3Path.from_bucket("my-bucket") / "references",
            items=[],  # No changes.
        )

        # Act
        minio_tool.push("foo", PathType.REFERENCE, local_dir)

        # Assert
        cap = capsys.readouterr()
        assert "`local` is already up to date with `s3://my-bucket/references`" in cap.out

    def test_push__dont_apply_changes(
        self,
        capsys: CaptureFixture,
        minio_tool: MinioTool,
        test_case_loader: Mock,
        rewinder: Mock,
        prompt: Mock,
        fs: FakeFilesystem,
    ) -> None:
        # Arrange
        local_dir = Path("local")
        fs.create_file(local_dir / "foo.txt", contents="foo")
        bucket = S3Path.from_bucket("my-bucket")
        test_case_loader.get_test_cases.return_value = [make_test_case("foo")]
        rewinder.build_plan.return_value = Plan(
            local_dir=Path("local"),
            minio_prefix=bucket / "references",
            items=[PlanItem.create(local_dir / "foo.txt", bucket / "references/foo.txt")],  # No changes.
        )
        prompt.yes_no.return_value = False  # No, don't apply these changes.

        # Act
        minio_tool.push("foo", PathType.REFERENCE, local_dir)

        # Assert
        rewinder.build_plan.assert_called_once_with(
            src_dir=local_dir, dst_prefix=bucket / "references", tags=None, update_only=False
        )
        rewinder.execute_plan.assert_not_called()
        out_lines = capsys.readouterr().out.splitlines()
        assert "The following files from `local` will be uploaded to `s3://my-bucket/references`" in out_lines[0]
        assert "foo.txt" in out_lines[1]  # File name
        assert "3 B" in out_lines[1]  # File size

    @pytest.mark.parametrize("minio_tool", [{"tags": {"foo": "bar"}}], indirect=["minio_tool"])
    def test_push__add_tags_but_dont_apply_changes__print_tags(
        self,
        capsys: CaptureFixture,
        minio_tool: MinioTool,
        test_case_loader: Mock,
        rewinder: Mock,
        prompt: Mock,
        fs: FakeFilesystem,
    ) -> None:
        # Arrange
        local_dir = Path("local")
        fs.create_file(local_dir / "foo.txt", contents="foo")
        bucket = S3Path.from_bucket("my-bucket")
        test_case_loader.get_test_cases.return_value = [make_test_case("foo")]
        rewinder.build_plan.return_value = Plan(
            local_dir=Path("local"),
            minio_prefix=bucket / "references",
            items=[PlanItem.update(local_dir / "foo.txt", bucket / "references/foo.txt")],  # No changes.
            tags={"foo": "bar"},  # type: ignore
        )
        prompt.yes_no.return_value = False  # No, don't apply these changes.

        # Act
        minio_tool.push("foo", PathType.INPUT, local_dir, update_only=True)

        # Assert
        rewinder.build_plan.assert_called_once_with(
            src_dir=local_dir,
            dst_prefix=bucket / "cases",
            tags={"foo": "bar"},  # type: ignore
            update_only=True,
        )
        rewinder.execute_plan.assert_not_called()
        cap = capsys.readouterr()
        assert "The following tags" in cap.out
        assert "foo=bar" in cap.out

    def test_push__apply_changes_dont_save_configs(
        self,
        capsys: CaptureFixture,
        minio_tool: MinioTool,
        test_case_loader: Mock,
        test_case_writer: Mock,
        rewinder: Mock,
        prompt: Mock,
        fs: FakeFilesystem,
        mocker: MockerFixture,
    ) -> None:
        # Arrange
        local_dir = Path("local")
        config_path = Path("configs/config.xml")
        fs.create_file(local_dir / "foo.txt", contents="foo")
        fs.create_file(config_path, contents="old")
        bucket = S3Path.from_bucket("my-bucket")
        test_case_loader.get_test_cases.return_value = [make_test_case("foo")]
        plan = Plan(
            local_dir=Path("local"),
            minio_prefix=bucket / "references",
            items=[PlanItem.create(local_dir / "foo.txt", bucket / "references/foo.txt")],  # No changes.
        )
        rewinder.build_plan.return_value = plan
        prompt.yes_no.side_effect = [True, False]  # Yes, apply changes. No, don't save the configs.
        test_case_writer.config_updates.return_value = {config_path: io.StringIO("new")}

        # Act
        minio_tool.push("foo", PathType.REFERENCE, local_dir, update_only=False)

        # Assert
        rewinder.build_plan.assert_called_once_with(
            src_dir=local_dir,
            dst_prefix=bucket / "references",
            tags=None,
            update_only=False,
        )
        rewinder.execute_plan.assert_called_once_with(plan)
        test_case_writer.config_updates.assert_called_once_with({"foo": mocker.ANY})
        new_timestamp = test_case_writer.config_updates.call_args.args[0]["foo"]
        now = datetime.now(timezone.utc)
        assert timedelta(seconds=59) <= new_timestamp - now <= timedelta(minutes=2)

        cap = capsys.readouterr()
        assert "-old+new" in cap.out  # The diff is printed to the output.
        assert config_path.read_text() == "old"  # The config file's content is still old.

    def test_push__apply_changes_save_configs(
        self,
        capsys: CaptureFixture,
        minio_tool: MinioTool,
        test_case_loader: Mock,
        test_case_writer: Mock,
        rewinder: Mock,
        prompt: Mock,
        fs: FakeFilesystem,
    ) -> None:
        # Arrange
        local_dir = Path("local")
        config_path = Path("configs/config.xml")
        fs.create_file(local_dir / "foo.txt", contents="foo")
        fs.create_file(config_path, contents="old")
        bucket = S3Path.from_bucket("my-bucket")
        test_case_loader.get_test_cases.return_value = [make_test_case("foo")]
        plan = Plan(
            local_dir=local_dir,
            minio_prefix=bucket / "references",
            items=[PlanItem.create(local_dir / "foo.txt", bucket / "references/foo.txt")],  # No changes.
        )
        rewinder.build_plan.return_value = plan
        prompt.yes_no.return_value = True  # Yes to all prompts.
        test_case_writer.config_updates.return_value = {config_path: io.StringIO("new")}

        # Act
        minio_tool.push("foo", PathType.REFERENCE)

        # Assert
        cap = capsys.readouterr()
        assert "Unified diff of config files" in cap.out
        assert "-old+new" in cap.out  # The diff is printed to the output.
        assert config_path.read_text() == "new"  # The config file's content is still old.

    def test_push__conflicts_detected_dont_continue__return_before_build_plan(
        self,
        capsys: CaptureFixture,
        minio_tool: MinioTool,
        test_case_loader: Mock,
        rewinder: Mock,
        prompt: Mock,
    ) -> None:
        # Arrange
        now = datetime.now(timezone.utc)
        test_case_loader.get_test_cases.return_value = [
            make_test_case("foo", version=now - timedelta(days=3)),
        ]
        rewinder.detect_conflicts.return_value = [
            VersionPair(
                rewinded_version=None,
                latest_version=make_object("cases/bar.txt", size=42 * 1024 * 1024),  # 42 MiB
            ),
        ]
        prompt.yes_no.return_value = False  # Don't continue after detecting conflicts

        # Act
        minio_tool.push("foo", PathType.INPUT)

        # Assert
        rewinder.build_plan.assert_not_called()  # Aborted before making the plan.
        cap = capsys.readouterr()
        out_lines = cap.out.splitlines()
        assert "Conflicts detected." in out_lines[0]
        assert "+ bar.txt" in out_lines[1]
        assert "42 MiB" in out_lines[1]

    def test_push__conflicts_detected_and_apply_changes_and_save_configs(
        self,
        capsys: CaptureFixture,
        minio_tool: MinioTool,
        test_case_loader: Mock,
        test_case_writer: Mock,
        rewinder: Mock,
        prompt: Mock,
        fs: FakeFilesystem,
    ) -> None:
        # Arrange
        local_dir = Path("local")
        config_path = Path("configs/config.xml")
        fs.create_file(local_dir / "foo.txt", contents="foo")
        fs.create_file(config_path, contents="old")
        bucket = S3Path.from_bucket("my-bucket")
        test_case_loader.get_test_cases.return_value = [make_test_case("foo")]
        rewinder.detect_conflicts.return_value = [
            VersionPair(
                rewinded_version=None,
                latest_version=make_object("cases/bar.txt", size=42 * 1024 * 1024),  # 42 MiB
            ),
        ]
        plan = Plan(
            local_dir=Path("local"),
            minio_prefix=bucket / "references",
            items=[PlanItem.create(local_dir / "foo.txt", bucket / "references/foo.txt")],  # No changes.
        )
        rewinder.build_plan.return_value = plan
        prompt.yes_no.return_value = True  # Yes to all prompts.
        test_case_writer.config_updates.return_value = {config_path: io.StringIO("new")}

        # Act
        minio_tool.push("foo", PathType.REFERENCE, local_dir)

        # Assert
        cap = capsys.readouterr()
        assert "-old+new" in cap.out  # The diff is printed to the output.
        assert config_path.read_text() == "new"  # The config file's content is still old.

    def test_pull__non_existent_test_case__raise_error(
        self,
        minio_tool: MinioTool,
        test_case_loader: Mock,
    ) -> None:
        # Arrange
        test_case_loader.get_test_cases.return_value = []

        # Act
        with pytest.raises(MinioToolError, match="does not match"):
            minio_tool.pull("foo", PathType.INPUT)

    def test_pull__multiple_matching_test_cases__raise_error(
        self,
        minio_tool: MinioTool,
        test_case_loader: Mock,
    ) -> None:
        # Arrange
        test_case_loader.get_test_cases.return_value = [
            make_test_case("foo"),
            make_test_case("foobar"),
        ]

        # Act
        with pytest.raises(MinioToolError, match="matches multiple"):
            minio_tool.pull("foo", PathType.INPUT)

    @pytest.mark.parametrize(
        "path_type,prefix",
        [
            (PathType.INPUT, "cases"),
            (PathType.REFERENCE, "references"),
        ],
    )
    def test_pull__directory_empty__download_files(
        self,
        path_type: PathType,
        prefix: str,
        minio_tool: MinioTool,
        test_case_loader: Mock,
        rewinder: Mock,
        prompt: Mock,
        fs: FakeFilesystem,
    ) -> None:
        # Arrange
        local_dir = Path("local")
        fs.create_dir(local_dir)
        test_case_loader.get_test_cases.return_value = [make_test_case("foo")]

        # Act
        minio_tool.pull("foo", path_type, local_dir)

        # Assert
        prompt.yes_no.assert_not_called()
        rewinder.download.assert_called_once_with("my-bucket", prefix, local_dir, None)

    @pytest.mark.parametrize(
        "path_type,prefix,rewind",
        [
            (PathType.INPUT, "cases", None),
            (PathType.REFERENCE, "references", datetime.now(timezone.utc)),
        ],
    )
    def test_pull__directory_not_empty__prompt_before_downloading_files(
        self,
        path_type: PathType,
        prefix: str,
        rewind: Optional[datetime],
        minio_tool: MinioTool,
        test_case_loader: Mock,
        rewinder: Mock,
        prompt: Mock,
        fs: FakeFilesystem,
    ) -> None:
        # Arrange
        local_dir = Path("local")
        fs.create_file(local_dir / "bar.txt")
        test_case_loader.get_test_cases.return_value = [make_test_case("foo")]
        prompt.yes_no.return_value = True  # Yes, download files.

        # Act
        minio_tool.pull("foo", path_type, local_dir, rewind)

        # Assert
        prompt.yes_no.assert_called_once()
        rewinder.download.assert_called_once_with("my-bucket", prefix, local_dir, rewind)

    def test_pull__directory_not_empty_dont_download(
        self,
        minio_tool: MinioTool,
        test_case_loader: Mock,
        rewinder: Mock,
        prompt: Mock,
        fs: FakeFilesystem,
    ) -> None:
        # Arrange
        test_case = make_test_case("foo")
        fs.create_file(test_case.reference_dir / "foo.txt")
        test_case_loader.get_test_cases.return_value = [test_case]
        prompt.yes_no.return_value = False  # No, don't download files.

        # Act
        minio_tool.pull("foo", PathType.REFERENCE)

        # Assert
        prompt.yes_no.assert_called_once()
        rewinder.download.assert_not_called()

    def test_pull__unsupported_path_type__raise_error(
        self,
        minio_tool: MinioTool,
        test_case_loader: Mock,
    ) -> None:
        # Arrange
        test_case_loader.get_test_cases.return_value = [make_test_case("foo")]

        # Act
        with pytest.raises(ValueError, match="Unsupported path type"):
            minio_tool.pull("foo", PathType.NONE)

    def test_pull__not_latest_no_timestamp_no_version__skip_detect_conflicts(
        self,
        minio_tool: MinioTool,
        test_case_loader: Mock,
        rewinder: Mock,
    ) -> None:
        # Arrange
        test_case = make_test_case("foo")
        test_case_loader.get_test_cases.return_value = [test_case]

        # Act
        minio_tool.pull("foo", PathType.REFERENCE)

        # Assert
        bucket = test_case.reference_prefix.bucket
        key = test_case.reference_prefix.key
        rewinder.detect_conflicts.assert_not_called()
        rewinder.download.assert_called_once_with(bucket, key, test_case.reference_dir, None)

    def test_pull__not_latest_no_timestamp_with_version__detect_conflicts__no_conflict(
        self, minio_tool: MinioTool, test_case_loader: Mock, rewinder: Mock, fs: FakeFilesystem
    ) -> None:
        # Arrange
        three_days_ago = datetime.now(timezone.utc) - timedelta(days=3)
        test_case = make_test_case("foo", version=three_days_ago)
        test_case_loader.get_test_cases.return_value = [test_case]
        rewinder.detect_conflicts.return_value = []

        # Act
        minio_tool.pull("foo", PathType.REFERENCE)

        # Assert
        bucket = test_case.reference_prefix.bucket
        key = test_case.reference_prefix.key
        rewinder.detect_conflicts.assert_called_once_with(test_case.reference_prefix, three_days_ago)
        rewinder.download.assert_called_once_with(bucket, key, test_case.reference_dir, three_days_ago)
        assert fs.exists(Path(test_case.reference_dir))

    def test_pull__not_latest_no_timestamp_with_version__detect_conflicts__accept(
        self,
        minio_tool: MinioTool,
        test_case_loader: Mock,
        rewinder: Mock,
        prompt: Mock,
        fs: FakeFilesystem,
    ) -> None:
        # Arrange
        three_days_ago = datetime.now(timezone.utc) - timedelta(days=3)
        test_case = make_test_case("foo", version=three_days_ago)
        test_case_loader.get_test_cases.return_value = [test_case]
        conflict = VersionPair(
            rewinded_version=None,
            latest_version=make_object("references/bar.txt", size=42 * 1024 * 1024),  # 42 MiB
        )
        rewinder.detect_conflicts.return_value = [conflict]
        prompt.yes_no.return_value = True

        # Act
        minio_tool.pull("foo", PathType.REFERENCE)

        # Assert
        bucket = test_case.reference_prefix.bucket
        key = test_case.reference_prefix.key
        rewinder.detect_conflicts.assert_called_once_with(test_case.reference_prefix, three_days_ago)
        rewinder.download.assert_called_once_with(bucket, key, test_case.reference_dir, three_days_ago)
        assert fs.exists(Path(test_case.reference_dir))

    def test_pull__not_latest_no_timestamp_with_version__detect_conflicts__decline(
        self,
        minio_tool: MinioTool,
        test_case_loader: Mock,
        rewinder: Mock,
        prompt: Mock,
        fs: FakeFilesystem,
    ) -> None:
        # Arrange
        three_days_ago = datetime.now(timezone.utc) - timedelta(days=3)
        test_case = make_test_case("foo", version=three_days_ago)
        test_case_loader.get_test_cases.return_value = [test_case]
        conflict = VersionPair(
            rewinded_version=None,
            latest_version=make_object("cases/bar.txt", size=42 * 1024 * 1024),  # 42 MiB
        )
        rewinder.detect_conflicts.return_value = [conflict]
        prompt.yes_no.return_value = False

        # Act
        minio_tool.pull("foo", PathType.INPUT)

        # Assert
        rewinder.detect_conflicts.assert_called_once_with(test_case.case_prefix, three_days_ago)
        rewinder.download.assert_not_called()
        assert not fs.exists(Path(test_case.case_dir))

    def test_pull__not_latest_with_timestamp_and_version__timestamp_takes_precedence(
        self,
        minio_tool: MinioTool,
        test_case_loader: Mock,
        rewinder: Mock,
        fs: FakeFilesystem,
    ) -> None:
        # Arrange
        now = datetime.now(timezone.utc)
        test_case = make_test_case("foo", version=now - timedelta(days=3))
        test_case_loader.get_test_cases.return_value = [test_case]

        # Act
        minio_tool.pull("foo", PathType.INPUT, timestamp=now - timedelta(days=42))

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

    def test_pull__latest_and_version__latest_takes_precedence(
        self,
        minio_tool: MinioTool,
        test_case_loader: Mock,
        rewinder: Mock,
        fs: FakeFilesystem,
    ) -> None:
        # Arrange
        now = datetime.now(timezone.utc)
        test_case = make_test_case("foo", version=now - timedelta(days=3))
        test_case_loader.get_test_cases.return_value = [test_case]

        # Act
        minio_tool.pull("foo", PathType.INPUT, latest=True)

        # Assert
        prefix = test_case.case_prefix
        rewinder.detect_conflicts.assert_not_called()
        rewinder.download.assert_called_once_with(
            prefix.bucket,
            prefix.key,
            test_case.case_dir,
            None,
        )
        assert fs.exists(Path(test_case.case_dir))

    def test_pull__latest_and_timestamp__latest_takes_precedence(
        self,
        minio_tool: MinioTool,
        test_case_loader: Mock,
        rewinder: Mock,
        fs: FakeFilesystem,
    ) -> None:
        # Arrange
        now = datetime.now(timezone.utc)
        test_case = make_test_case("foo", version=now - timedelta(days=3))
        test_case_loader.get_test_cases.return_value = [test_case]

        # Act
        minio_tool.pull("foo", PathType.REFERENCE, timestamp=now - timedelta(days=42), latest=True)

        # Assert
        prefix = test_case.reference_prefix
        rewinder.detect_conflicts.assert_not_called()
        rewinder.download.assert_called_once_with(
            prefix.bucket,
            prefix.key,
            test_case.reference_dir,
            None,
        )
        assert fs.exists(Path(test_case.reference_dir))

    def test_update_references__non_existent_test_case__raise_error(
        self,
        minio_tool: MinioTool,
        test_case_loader: Mock,
    ) -> None:
        # Arrange
        test_case_loader.get_test_cases.return_value = []

        # Act
        with pytest.raises(MinioToolError, match="does not match"):
            minio_tool.update_references("foo")

    def test_update_references__multiple_matching_test_cases__raise_error(
        self,
        minio_tool: MinioTool,
        test_case_loader: Mock,
    ) -> None:
        # Arrange
        test_case_loader.get_test_cases.return_value = [
            make_test_case("foo"),
            make_test_case("foobar"),
        ]

        # Act
        with pytest.raises(MinioToolError, match="matches multiple"):
            minio_tool.update_references("foo", Path("local"))

    def test_update_references__test_case_has_version_timestamp_from_future__raise_error(
        self,
        minio_tool: MinioTool,
        test_case_loader: Mock,
    ) -> None:
        # Arrange
        now = datetime.now(timezone.utc)
        test_case_loader.get_test_cases.return_value = [make_test_case("foo", version=now + timedelta(seconds=43))]

        # Act
        with pytest.raises(MinioToolError, match="future"):
            minio_tool.update_references("foo")

    def test_update_references__no_changes__print_up_to_date_message(
        self,
        capsys: CaptureFixture,
        minio_tool: MinioTool,
        test_case_loader: Mock,
        rewinder: Mock,
    ) -> None:
        # Arrange
        local_dir = Path("local")
        test_case_loader.get_test_cases.return_value = [make_test_case("foo")]
        rewinder.build_plan.return_value = Plan(
            local_dir=Path("local"),
            minio_prefix=S3Path.from_bucket("my-bucket") / "references",
            items=[],  # No changes.
        )

        # Act
        minio_tool.update_references("foo", local_dir)

        # Assert
        cap = capsys.readouterr()
        assert "`local` is already up to date with `s3://my-bucket/references`" in cap.out

    def test_update_references__dont_apply_changes(
        self,
        capsys: CaptureFixture,
        minio_tool: MinioTool,
        test_case_loader: Mock,
        rewinder: Mock,
        prompt: Mock,
        fs: FakeFilesystem,
    ) -> None:
        # Arrange
        local_dir = Path("local")
        fs.create_file(local_dir / "foo.txt", contents="foo")
        bucket = S3Path.from_bucket("my-bucket")
        test_case_loader.get_test_cases.return_value = [make_test_case("foo")]
        rewinder.build_plan.return_value = Plan(
            local_dir=Path("local"),
            minio_prefix=bucket / "references",
            items=[PlanItem.create(local_dir / "foo.txt", bucket / "references/foo.txt")],  # No changes.
        )
        prompt.yes_no.return_value = False  # No, don't apply these changes.

        # Act
        minio_tool.update_references("foo", local_dir)

        # Assert
        rewinder.build_plan.assert_called_once_with(
            src_dir=local_dir, dst_prefix=bucket / "references", tags=None, update_only=True
        )
        rewinder.execute_plan.assert_not_called()
        out_lines = capsys.readouterr().out.splitlines()
        assert "The following files from `local` will be uploaded to `s3://my-bucket/references`" in out_lines[0]
        assert "foo.txt" in out_lines[1]  # File name
        assert "3 B" in out_lines[1]  # File size

    @pytest.mark.parametrize("minio_tool", [{"tags": {"foo": "bar"}}], indirect=["minio_tool"])
    def test_update_references__add_tags_but_dont_apply_changes__print_tags(
        self,
        capsys: CaptureFixture,
        minio_tool: MinioTool,
        test_case_loader: Mock,
        rewinder: Mock,
        prompt: Mock,
        fs: FakeFilesystem,
    ) -> None:
        # Arrange
        test_case = make_test_case("foo")
        fs.create_file(test_case.case_dir / "foo.txt", contents="foo")
        test_case_loader.get_test_cases.return_value = [test_case]
        rewinder.build_plan.return_value = Plan(
            local_dir=test_case.case_dir,
            minio_prefix=test_case.reference_prefix,
            items=[PlanItem.create(test_case.case_dir / "foo.txt", test_case.reference_prefix / "foo.txt")],
            tags={"foo": "bar"},  # type: ignore
        )
        prompt.yes_no.return_value = False  # No, don't apply these changes.

        # Act
        minio_tool.update_references("foo")

        # Assert
        rewinder.build_plan.assert_called_once_with(
            src_dir=test_case.case_dir,
            dst_prefix=test_case.reference_prefix,
            tags={"foo": "bar"},  # type: ignore
            update_only=True,
        )
        rewinder.execute_plan.assert_not_called()
        cap = capsys.readouterr()
        assert "The following tags" in cap.out
        assert "foo=bar" in cap.out

    def test_update_references__apply_changes_dont_save_configs(
        self,
        capsys: CaptureFixture,
        minio_tool: MinioTool,
        test_case_loader: Mock,
        test_case_writer: Mock,
        rewinder: Mock,
        prompt: Mock,
        fs: FakeFilesystem,
        mocker: MockerFixture,
    ) -> None:
        # Arrange
        local_dir = Path("local")
        config_path = Path("configs/config.xml")
        fs.create_file(local_dir / "foo.txt", contents="foo")
        fs.create_file(config_path, contents="old")
        bucket = S3Path.from_bucket("my-bucket")
        test_case_loader.get_test_cases.return_value = [make_test_case("foo")]
        plan = Plan(
            local_dir=Path("local"),
            minio_prefix=bucket / "references",
            items=[PlanItem.create(local_dir / "foo.txt", bucket / "references/foo.txt")],  # No changes.
        )
        rewinder.build_plan.return_value = plan
        prompt.yes_no.side_effect = [True, False]  # Yes, apply changes. No, don't save the configs.
        test_case_writer.config_updates.return_value = {config_path: io.StringIO("new")}

        # Act
        minio_tool.update_references("foo", local_dir)

        # Assert
        rewinder.build_plan.assert_called_once_with(
            src_dir=local_dir,
            dst_prefix=bucket / "references",
            tags=None,
            update_only=True,
        )
        rewinder.execute_plan.assert_called_once_with(plan)
        test_case_writer.config_updates.assert_called_once_with({"foo": mocker.ANY})
        new_timestamp = test_case_writer.config_updates.call_args.args[0]["foo"]
        assert datetime.now(timezone.utc) - new_timestamp < timedelta(seconds=2)

        cap = capsys.readouterr()
        assert "-old+new" in cap.out  # The diff is printed to the output.
        assert config_path.read_text() == "old"  # The config file's content is still old.

    def test_update_references__apply_changes_save_configs(
        self,
        capsys: CaptureFixture,
        minio_tool: MinioTool,
        test_case_loader: Mock,
        test_case_writer: Mock,
        rewinder: Mock,
        prompt: Mock,
        fs: FakeFilesystem,
    ) -> None:
        # Arrange
        local_dir = Path("local")
        config_path = Path("configs/config.xml")
        fs.create_file(local_dir / "foo.txt", contents="foo")
        fs.create_file(config_path, contents="old")
        bucket = S3Path.from_bucket("my-bucket")
        test_case_loader.get_test_cases.return_value = [make_test_case("foo")]
        plan = Plan(
            local_dir=local_dir,
            minio_prefix=bucket / "references",
            items=[PlanItem.create(local_dir / "foo.txt", bucket / "references/foo.txt")],  # No changes.
        )
        rewinder.build_plan.return_value = plan
        prompt.yes_no.return_value = True  # Yes to all prompts.
        test_case_writer.config_updates.return_value = {config_path: io.StringIO("new")}

        # Act
        minio_tool.update_references("foo")

        # Assert
        cap = capsys.readouterr()
        assert "Unified diff of config files" in cap.out
        assert "-old+new" in cap.out  # The diff is printed to the output.
        assert config_path.read_text() == "new"  # The config file's content is still old.

    def test_update_references__conflicts_detected_dont_continue__return_before_build_plan(
        self,
        capsys: CaptureFixture,
        minio_tool: MinioTool,
        test_case_loader: Mock,
        rewinder: Mock,
        prompt: Mock,
    ) -> None:
        # Arrange
        now = datetime.now(timezone.utc)
        test_case_loader.get_test_cases.return_value = [
            make_test_case("foo", version=now - timedelta(days=3)),
        ]
        rewinder.detect_conflicts.return_value = [
            VersionPair(
                rewinded_version=None,
                latest_version=make_object("references/bar.txt", size=42 * 1024 * 1024),  # 42 MiB
            ),
        ]
        prompt.yes_no.return_value = False  # Don't continue after detecting conflicts

        # Act
        minio_tool.update_references("foo")

        # Assert
        rewinder.build_plan.assert_not_called()  # Aborted before making the plan.
        cap = capsys.readouterr()
        out_lines = cap.out.splitlines()
        assert "Conflicts detected." in out_lines[0]
        assert "+ bar.txt" in out_lines[1]
        assert "42 MiB" in out_lines[1]

    def test_update_references__conflicts_detected_and_apply_changes_and_save_configs(
        self,
        capsys: CaptureFixture,
        minio_tool: MinioTool,
        test_case_loader: Mock,
        test_case_writer: Mock,
        rewinder: Mock,
        prompt: Mock,
        fs: FakeFilesystem,
    ) -> None:
        # Arrange
        local_dir = Path("local")
        config_path = Path("configs/config.xml")
        fs.create_file(local_dir / "foo.txt", contents="foo")
        fs.create_file(config_path, contents="old")
        bucket = S3Path.from_bucket("my-bucket")
        test_case_loader.get_test_cases.return_value = [make_test_case("foo")]
        rewinder.detect_conflicts.return_value = [
            VersionPair(
                rewinded_version=None,
                latest_version=make_object("cases/bar.txt", size=42 * 1024 * 1024),  # 42 MiB
            ),
        ]
        plan = Plan(
            local_dir=Path("local"),
            minio_prefix=bucket / "references",
            items=[PlanItem.create(local_dir / "foo.txt", bucket / "references/foo.txt")],  # No changes.
        )
        rewinder.build_plan.return_value = plan
        prompt.yes_no.return_value = True  # Yes to all prompts.
        test_case_writer.config_updates.return_value = {config_path: io.StringIO("new")}

        # Act
        minio_tool.update_references("foo", local_dir)

        # Assert
        cap = capsys.readouterr()
        assert "-old+new" in cap.out  # The diff is printed to the output.
        assert config_path.read_text() == "new"  # The config file's content is still old.
