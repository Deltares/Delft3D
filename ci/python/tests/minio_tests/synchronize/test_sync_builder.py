import logging
from datetime import datetime, timedelta, timezone
from pathlib import Path

import pytest
from minio import Minio
from pyfakefs.fake_filesystem import FakeFilesystem
from pytest_mock import MockerFixture
from s3_path_wrangler.paths import S3Path

from ci_tools.minio import DEFAULT_MULTIPART_UPLOAD_PART_SIZE
from ci_tools.minio.matcher import GlobMatcher
from ci_tools.minio.synchronize import DEFAULT_QUEUE_SIZE, DEFAULT_WORKER_COUNT
from ci_tools.minio.synchronize.local_to_remote import LocalToRemote
from ci_tools.minio.synchronize.remote_to_local import RemoteToLocal
from ci_tools.minio.synchronize.sync_builder import SyncBuilder
from ci_tools.minio.synchronize.sync_plan import Changes, Mode
from tests.helpers import minio as helper


@pytest.fixture
def builder(mocker: MockerFixture, fs: FakeFilesystem) -> SyncBuilder:
    """Make a `SyncBuilder` that targets two empty locations."""
    minio_client = mocker.Mock(spec=Minio)
    minio_client.list_objects.return_value = iter(
        [helper.make_object("my_objects/foo.txt")],
    )
    local_dir = Path("my_files")
    fs.create_file(local_dir / "foo.txt")
    return (
        SyncBuilder()
        .set_minio_client(minio_client)
        .set_local_directory(local_dir)
        .set_remote_prefix(S3Path("s3://my-bucket/my_objects"))
    )


class TestSyncBuilder:
    def test_set_remote_timestamp__naive_datetime__replace_with_utc(self, builder: SyncBuilder) -> None:
        naive = datetime.now()  # noqa: DTZ005
        builder.set_remote_timestamp(naive)
        assert builder._remote_timestamp is not None
        assert builder._remote_timestamp.tzinfo == timezone.utc

    def test_build_local_to_remote__defaults(self, builder: SyncBuilder) -> None:
        # Act
        local_to_remote = builder.build_local_to_remote()

        # Assert
        assert isinstance(local_to_remote, LocalToRemote)

        plan = local_to_remote._plan
        assert plan.mode == Mode.NO_DELETE
        assert plan.part_size == DEFAULT_MULTIPART_UPLOAD_PART_SIZE
        assert plan.changes == Changes(creations=[], updates=[], deletions=[])

        assert local_to_remote._show_progress
        assert local_to_remote._reader_count == DEFAULT_WORKER_COUNT // 4
        assert local_to_remote._uploader_count == DEFAULT_WORKER_COUNT - (DEFAULT_WORKER_COUNT // 4) - 1
        assert local_to_remote._uploader_queue_max_size == DEFAULT_QUEUE_SIZE
        assert local_to_remote._logger == logging.getLogger(LocalToRemote.__module__)

    def test_build_local_to_remote__override_defaults(self, builder: SyncBuilder, mocker: MockerFixture) -> None:
        # Arrange
        logger = mocker.Mock(spec=logging.Logger)
        (
            builder.set_mode(Mode.UPDATE_ONLY)
            .set_part_size(3 * 2**20)
            .set_thread_count(5)
            .set_show_progress(False)
            .set_max_queue_size(64)
            .set_logger(logger)
        )

        # Act
        local_to_remote = builder.build_local_to_remote()

        # Assert
        assert isinstance(local_to_remote, LocalToRemote)

        plan = local_to_remote._plan
        assert plan.mode == Mode.UPDATE_ONLY
        assert plan.part_size == 3 * 2**20
        assert plan.changes == Changes(creations=[], updates=[], deletions=[])

        assert not local_to_remote._show_progress
        assert local_to_remote._reader_count == 1
        assert local_to_remote._uploader_count == 3
        assert local_to_remote._uploader_queue_max_size == 64
        assert local_to_remote._logger == logger

    def test_build_local_to_remote__raise_error_on_empty_local_dir(
        self, builder: SyncBuilder, fs: FakeFilesystem
    ) -> None:
        # Arrange
        fs.remove_object("my_files/foo.txt")  # This file is made in the `builder` fixture.

        # Act
        with pytest.raises(ValueError, match="No files to upload"):
            builder.build_local_to_remote()

    def test_build_local_to_remote__raise_error_on_empty_local_dir__with_filter(self, builder: SyncBuilder) -> None:
        builder.add_include(GlobMatcher("*.md"))
        with pytest.raises(ValueError, match="include"):
            builder.build_local_to_remote()

    def test_build_remote_to_local__defaults(self, builder: SyncBuilder) -> None:
        # Act
        remote_to_local = builder.build_remote_to_local()

        # Assert
        assert isinstance(remote_to_local, RemoteToLocal)

        plan = remote_to_local._plan
        assert plan.mode == Mode.NO_DELETE
        assert plan.part_size == DEFAULT_MULTIPART_UPLOAD_PART_SIZE
        assert plan.changes == Changes(creations=[], updates=[], deletions=[])

        assert remote_to_local._show_progress
        assert remote_to_local._thread_count == DEFAULT_WORKER_COUNT
        assert remote_to_local._collector_queue_max_size == DEFAULT_QUEUE_SIZE
        assert remote_to_local._logger == logging.getLogger(RemoteToLocal.__module__)

    def test_build_remote_to_local__override_defaults(self, mocker: MockerFixture, fs: FakeFilesystem) -> None:
        # Arrange
        minio_client = mocker.Mock(spec=Minio)
        now = datetime.now(timezone.utc)
        timestamp = now - timedelta(days=1)
        # Only return one 'new' object that will not show up in the listing because it's too new.
        minio_client.list_objects.return_value = iter(
            [
                helper.make_object("my_objects/foo.txt", last_modified=now - timedelta(days=2)),
                helper.make_object("my_objects/new.txt", last_modified=now),
            ],
        )

        fs.create_file("my_files/foo.txt")

        logger = mocker.Mock(spec=logging.Logger)
        builder = (
            SyncBuilder()
            .set_minio_client(minio_client)
            .set_local_directory(Path("my_files"))
            .set_remote_prefix(S3Path("s3://my-bucket/my_objects"))
            .set_remote_timestamp(timestamp)
            .set_mode(Mode.DELETE)
            .set_part_size(3 * 2**20)
            .set_thread_count(5)
            .set_show_progress(False)
            .set_max_queue_size(64)
            .set_logger(logger)
        )

        # Act
        remote_to_local = builder.build_remote_to_local()

        # Assert
        assert isinstance(remote_to_local, RemoteToLocal)

        plan = remote_to_local._plan
        assert plan.mode == Mode.DELETE
        assert plan.part_size == 3 * 2**20
        assert plan.changes == Changes(creations=[], updates=[], deletions=[])

        assert not remote_to_local._show_progress
        assert remote_to_local._thread_count == 5
        assert remote_to_local._collector_queue_max_size == 64
        assert remote_to_local._logger == logger

    def test_build_remote_to_local__raise_error_on_empty_prefix(
        self, builder: SyncBuilder, mocker: MockerFixture
    ) -> None:
        # Arrange
        minio_client = mocker.Mock(spec=Minio)
        minio_client.list_objects.return_value = iter([])
        builder.set_minio_client(minio_client)

        # Act
        with pytest.raises(ValueError, match="No objects to download"):
            builder.build_remote_to_local()

    def test_build_remote_to_local__raise_error_on_empty_prefix__with_filter(self, builder: SyncBuilder) -> None:
        builder.add_include(GlobMatcher("*.md"))
        with pytest.raises(ValueError, match="include"):
            builder.build_remote_to_local()

    def test_build_local_to_remote__set_timestamp__raise_error(self, builder: SyncBuilder) -> None:
        builder.set_remote_timestamp(datetime.now(timezone.utc))
        with pytest.raises(ValueError, match="not supported"):
            builder.build_local_to_remote()

    def test_include_exclude_filter__no_filter(self, builder: SyncBuilder) -> None:
        # Arrange
        objects = [helper.make_list_item(path) for path in ("bar", "baz", "foo")]

        # Act
        result = list(filter(builder._include_exclude_filter, objects))

        # Assert
        assert result == objects

    def test_include_exclude_filter__include_filter(self, builder: SyncBuilder) -> None:
        # Arrange
        objects = [helper.make_list_item(path) for path in ("bar.bmp", "baz.jpeg", "foo.png")]
        builder.add_include(GlobMatcher("*.png"))

        # Act
        foo, *others = filter(builder._include_exclude_filter, objects)

        # Assert
        assert not others
        assert foo.relative_path == "foo.png"

    def test_include_exclude_filte__exclude_filter(self, builder: SyncBuilder) -> None:
        # Arrange
        objects = [helper.make_list_item(path) for path in ("bar.bmp", "baz.jpeg", "foo.png")]
        builder.add_exclude(GlobMatcher("*.png"))

        # Act
        bar, baz, *others = filter(builder._include_exclude_filter, objects)

        # Assert
        assert not others
        assert bar.relative_path == "bar.bmp"
        assert baz.relative_path == "baz.jpeg"

    def test_include_exclude_filte__multiple_include_filters(self, builder: SyncBuilder) -> None:
        # Arrange
        objects = [helper.make_list_item(path) for path in ("bar.bmp", "baz.jpeg", "foo.png")]
        builder.add_includes([GlobMatcher("*.png"), GlobMatcher("bar.*")])

        # Act
        bar, foo, *others = filter(builder._include_exclude_filter, objects)

        # Assert
        assert not others
        assert bar.relative_path == "bar.bmp"
        assert foo.relative_path == "foo.png"

    def test_build_local_to_remote__multiple_exclude_filters(self, builder: SyncBuilder) -> None:
        # Arrange
        objects = [helper.make_list_item(path) for path in ("bar.bmp", "baz.jpeg", "foo.png")]
        builder.add_excludes([GlobMatcher("*.png"), GlobMatcher("baz.*")])

        # Act
        bar, *others = filter(builder._include_exclude_filter, objects)

        # Assert
        assert not others
        assert bar.relative_path == "bar.bmp"

    def test_build_local_to_remote__combined_include_exclude(self, builder: SyncBuilder) -> None:
        # Arrange
        objects = [helper.make_list_item(path) for path in ("bar.bmp", "baz.jpeg", "foo.png")]
        builder.add_include(GlobMatcher("ba*"))
        builder.add_exclude(GlobMatcher("*g"))

        # Act
        bar, *others = filter(builder._include_exclude_filter, objects)

        # Assert
        assert not others
        assert bar.relative_path == "bar.bmp"

    def test_get_local_directory(self) -> None:
        builder = SyncBuilder()
        with pytest.raises(ValueError, match="must be set"):
            builder._get_local_directory()

    def test_get_remote_prefix(self, builder: SyncBuilder) -> None:
        builder = SyncBuilder()
        with pytest.raises(ValueError, match="must be set"):
            builder._get_remote_prefix()
