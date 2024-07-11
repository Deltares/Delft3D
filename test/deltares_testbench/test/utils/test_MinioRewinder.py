import hashlib
import itertools
import random
from datetime import datetime, timedelta, timezone
from pathlib import Path
from typing import List, Optional
from uuid import uuid4

import pytest
from minio import Minio
from minio.commonconfig import Tags
from minio.datatypes import Object as MinioObject
from pyfakefs.fake_filesystem import FakeFilesystem
from pytest_mock import MockerFixture
from s3_path_wrangler.paths import S3Path

from src.utils.logging.i_logger import ILogger
from src.utils.minio_rewinder import (
    DEFAULT_MULTIPART_UPLOAD_PART_SIZE,
    Operation,
    Plan,
    PlanItem,
    Rewinder,
    VersionPair,
)


def make_object(
    object_name: str,
    bucket_name: str = "my-bucket",
    version_id: Optional[str] = None,
    last_modified: datetime = datetime.min.replace(tzinfo=timezone.utc),
    is_delete_marker: bool = False,
    etag: Optional[str] = None,
) -> MinioObject:
    version_id = version_id or uuid4().hex

    return MinioObject(
        bucket_name=bucket_name,
        object_name=object_name,
        version_id=version_id,
        last_modified=last_modified,
        is_delete_marker=is_delete_marker,
        etag=etag,
    )


def minio_objects_equal(this: MinioObject, that: MinioObject) -> bool:
    """Check equality of many properties of two MinIO objects.

    Does not check equality of `tags`.
    """
    return all(
        [
            this.bucket_name == that.bucket_name,
            this.object_name == that.object_name,
            this.version_id == that.version_id,
            this.is_delete_marker == that.is_delete_marker,
            this.last_modified == that.last_modified,
            this.etag == that.etag,
            this.size == that.size,
        ]
    )


class TestMinioRewinder:
    def test_rewind_download_delete_marker(self, mocker: MockerFixture, fs: FakeFilesystem):
        # Arrange
        mock_minio_client = mocker.Mock(spec=Minio)

        object1 = make_object(
            object_name="object1_name",
            version_id="version1",
            last_modified=datetime(2023, 10, 20, 10, 0, tzinfo=timezone.utc),
        )
        object2 = make_object(
            object_name="object2_name",
            version_id="version1",
            last_modified=datetime(2023, 10, 20, 10, 0, tzinfo=timezone.utc),
            is_delete_marker=True,
        )

        # Create a list of mock objects to be returned by the list_objects method
        object_list = [object1, object2]

        mock_minio_client.list_objects.return_value = object_list
        logger = mocker.Mock(spec=ILogger)

        # Act
        rewinder_instance = Rewinder(mock_minio_client, logger)
        timestamp = datetime(2023, 10, 20, 12, tzinfo=timezone.utc)
        rewinder_instance.download("my-bucket", "prefix", Path("."), timestamp)

        # Assert
        mock_minio_client.fget_object.assert_called_once_with(
            "my-bucket", "object1_name", str(Path("./object1_name")), version_id=mocker.ANY
        )

    def test_rewind_download_before_rewind(self, mocker: MockerFixture):
        # Create a Mock for the Minio client
        mock_minio_client = mocker.Mock(spec=Minio)

        object1 = make_object(
            object_name="object1_name",
            version_id="version1",
            last_modified=datetime(2023, 10, 20, 10, 0, tzinfo=timezone.utc),
        )
        object2 = make_object(
            object_name="object2_name",
            version_id="version1",
            last_modified=datetime(2023, 10, 20, 10, 0, tzinfo=timezone.utc),
        )

        # Create a list of mock objects to be returned by the list_objects method
        object_list = [object1, object2]

        mock_minio_client.list_objects.return_value = object_list
        logger = mocker.Mock(spec=ILogger)

        rewinder_instance = Rewinder(mock_minio_client, logger)
        timestamp = datetime(2022, 10, 20, 12, tzinfo=timezone.utc)
        rewinder_instance.download("my-bucket", "prefix", Path("."), timestamp)

        mock_minio_client.fget_object.assert_not_called()

    def test_rewind_remove_file_not_in_download(self, mocker: MockerFixture, fs: FakeFilesystem):
        # Create a Mock for the Minio client
        mock_minio_client = mocker.Mock(spec=Minio)

        object1 = make_object(
            object_name="object1_name",
            version_id="version1",
            last_modified=datetime(2023, 10, 20, 10, 0, tzinfo=timezone.utc),
        )
        object2 = make_object(
            object_name="object2_name",
            version_id="version1",
            last_modified=datetime(2023, 10, 20, 10, 0, tzinfo=timezone.utc),
        )

        # Create a list of mock objects to be returned by the list_objects method
        object_list = [object1, object2]

        mock_minio_client.list_objects.return_value = object_list
        logger = mocker.Mock(spec=ILogger)

        # Create a file not in objects for removal
        file = "result.txt"
        fs.create_file(file)

        # Act
        rewinder_instance = Rewinder(mock_minio_client, logger)
        timestamp = datetime(2023, 10, 20, 12, tzinfo=timezone.utc)
        rewinder_instance.download("my-bucket", "prefix", Path("."), timestamp)

        # Assert
        assert (
            mocker.call(f"Removing local file that is not present in MinIO bucket: {file}")
            in logger.debug.call_args_list
        )
        assert not fs.exists(file)

    def test_rewind_download_after_rewind(self, mocker: MockerFixture, fs: FakeFilesystem):
        # Arrange
        mock_minio_client = mocker.Mock(spec=Minio)

        object1 = make_object(
            object_name="object1_name",
            version_id="version1",
            last_modified=datetime(2023, 10, 20, 10, 0, tzinfo=timezone.utc),
        )
        object2 = make_object(
            object_name="object2_name",
            version_id="version1",
            last_modified=datetime(2023, 10, 20, 10, 0, tzinfo=timezone.utc),
        )

        # Create a list of mock objects to be returned by the list_objects method
        object_list = [object1, object2]

        mock_minio_client.list_objects.return_value = object_list
        logger = mocker.Mock(spec=ILogger)

        # Act
        rewinder_instance = Rewinder(mock_minio_client, logger)
        timestamp = datetime(2024, 10, 20, 12, tzinfo=timezone.utc)
        rewinder_instance.download("my-bucket", "prefix", Path("."), timestamp)

        # Assert
        mock_minio_client.fget_object.assert_any_call(
            "my-bucket", "object1_name", str(Path("object1_name")), version_id=mocker.ANY
        )
        mock_minio_client.fget_object.assert_any_call(
            "my-bucket", "object2_name", str(Path("object2_name")), version_id=mocker.ANY
        )

    def test_rewind_download_on_rewind(self, mocker: MockerFixture):
        # Create a Mock for the Minio client
        mock_minio_client = mocker.Mock(spec=Minio)

        object1 = make_object(
            object_name="object1_name",
            version_id="version1",
            last_modified=datetime(2023, 10, 20, 10, 0, tzinfo=timezone.utc),
        )
        object2 = make_object(
            object_name="object2_name",
            version_id="version1",
            last_modified=datetime(2023, 10, 20, 10, 0, tzinfo=timezone.utc),
        )

        # Create a list of mock objects to be returned by the list_objects method
        object_list = [object1, object2]

        mock_minio_client.list_objects.return_value = object_list
        logger = mocker.Mock(spec=ILogger)

        # Mock Minio and Rewinder
        rewinder_instance = Rewinder(mock_minio_client, logger)
        timestamp = datetime(2023, 10, 20, 9, 59, 59, tzinfo=timezone.utc)
        rewinder_instance.download("my-bucket", "prefix", Path("."), timestamp)

        mock_minio_client.fget_object.assert_not_called()

    def test_download__use_multiple_keys__download_multiple_files(
        self, mocker: MockerFixture, fs: FakeFilesystem
    ) -> None:
        """It should list the objects in `source/path` and download them to directory `destination/path`."""
        # Arrange
        minio_client = mocker.Mock(spec=Minio)
        rewinder = Rewinder(minio_client, mocker.Mock(spec=ILogger))
        minio_client.list_objects.return_value = (
            make_object(f"source/path/{key}", bucket_name="my-bucket", version_id=f"v{i}")
            for i, key in enumerate(("bar", "baz", "foo"))
        )

        # Act
        rewinder.download("my-bucket", "source/path", Path("destination/path"))

        # Assert
        assert fs.exists("destination/path")  # Destination directory has been created.
        assert sorted(minio_client.fget_object.call_args_list, key=lambda call: call.args[1]) == [
            mocker.call(
                "my-bucket",
                f"source/path/{name}",
                str(Path(f"destination/path/{name}")),
                version_id=f"v{i}",
            )
            for i, name in enumerate(("bar", "baz", "foo"))
        ]

    def test_download__source_path_has_trailing_slash(self, mocker: MockerFixture, fs: FakeFilesystem) -> None:
        """It should list the objects in `source/path/` and download them to directory `destination/path`."""
        # Arrange
        logger = mocker.Mock(spec=ILogger)
        minio_client = mocker.Mock(spec=Minio)
        rewinder = Rewinder(minio_client, logger)
        minio_client.list_objects.return_value = iter(
            [make_object("source/path/foo", bucket_name="my-bucket", version_id="v1")]
        )

        # Act
        rewinder.download("my-bucket", "source/path/", Path("destination/path"))

        # Assert
        assert fs.exists("destination/path")  # Destination directory has been created.
        minio_client.fget_object.assert_called_once_with(
            "my-bucket",
            "source/path/foo",
            str(Path("destination/path/foo")),
            version_id="v1",
        )

    def test_download__destination_dir_exists_and_empty__no_message(
        self, mocker: MockerFixture, fs: FakeFilesystem
    ) -> None:
        """It should not print a message saying the destination dir is not empty."""
        # Arrange
        logger = mocker.Mock(spec=ILogger)
        minio_client = mocker.Mock(spec=Minio)
        rewinder = Rewinder(minio_client, logger)
        minio_client.list_objects.return_value = iter(
            [make_object("source/path/foo", bucket_name="my-bucket", version_id="v1")]
        )
        fs.create_dir("destination/path")

        # Act
        rewinder.download("my-bucket", "source/path", Path("destination/path"))

        # Assert
        assert fs.exists("destination/path")  # Destination dir still exists.
        logger.error.assert_not_called()
        minio_client.fget_object.assert_called_once_with(
            "my-bucket", "source/path/foo", str(Path("destination/path/foo")), version_id="v1"
        )

    def test_download__destination_dir_exists_and_not_empty__print_not_empty_message(
        self, mocker: MockerFixture, fs: FakeFilesystem
    ) -> None:
        """It should not print a message saying the destination dir is not empty."""
        # Arrange
        logger = mocker.Mock(spec=ILogger)
        minio_client = mocker.Mock(spec=Minio)
        rewinder = Rewinder(minio_client, logger)
        minio_client.list_objects.return_value = iter(
            [make_object("source/path/foo", bucket_name="my-bucket", version_id="v1")]
        )
        fs.create_file("destination/path/bar")
        minio_client.fget_object.side_effect = self.create_file_side_effect("destination/path/foo")

        # Act
        rewinder.download("my-bucket", "source/path", Path("destination/path"))

        # Assert
        assert fs.exists("destination/path")  # Destination dir still exists.
        logger.warning.assert_called()  # Warn that destination dir is not empty.
        minio_client.fget_object.assert_called_once_with(
            "my-bucket", "source/path/foo", str(Path("destination/path/foo")), version_id="v1"
        )

    def create_file_side_effect(self, filename):
        def side_effect(*args, **kwargs):
            # Create the file (you can customize this logic)
            with open(filename, "w") as f:
                f.write("File content")
            return None  # Return None to mimic the function call

        return side_effect

    def test_download__same_key_multiple_versions__get_latest_versions(
        self, mocker: MockerFixture, fs: FakeFilesystem
    ) -> None:
        """It should download only the latest versions of objects in Minio."""
        # Arrange
        logger = mocker.Mock(spec=ILogger)
        minio_client = mocker.Mock(spec=Minio)
        rewinder = Rewinder(minio_client, logger)
        now = datetime.now(timezone.utc)
        minio_client.list_objects.return_value = (
            make_object(
                f"source/path/{key}",
                bucket_name="my-bucket",
                version_id=f"{key}-minus-{hours_ago}-hours",
                last_modified=now - timedelta(hours=hours_ago),
            )
            for key, hours_ago in itertools.product(
                ("foo", "bar", "baz"),
                (3, 5, 1, 4, 2),
            )
        )

        # Act
        rewinder.download("my-bucket", "source/path", Path("destination/path"))

        # Assert
        assert fs.exists("destination/path")  # Destination directory has been created.
        assert sorted(minio_client.fget_object.call_args_list, key=lambda call: call.args[1]) == [
            mocker.call(
                "my-bucket",
                f"source/path/{name}",
                str(Path(f"destination/path/{name}")),
                version_id=f"{name}-minus-1-hours",
            )
            for name in ("bar", "baz", "foo")
        ]

    def test_download__rewind__dont_download_latest(self, mocker: MockerFixture, fs: FakeFilesystem) -> None:
        """After a rewind, it shouldn't get future versions of objects."""
        # Arrange
        past = datetime.now(timezone.utc) - timedelta(days=30)
        logger = mocker.Mock(spec=ILogger)
        minio_client = mocker.Mock(spec=Minio)
        rewinder = Rewinder(minio_client, logger)
        minio_client.list_objects.return_value = (
            make_object(
                "source/path/foo",
                bucket_name="my-bucket",
                version_id=f"foo-minus-{30 - days}-days",
                last_modified=past + timedelta(days=days),
            )
            for days in (-2, -1, 1, 2)
        )

        # Act
        rewinder.download("my-bucket", "source/path", Path("destination/path"), past)

        # Assert
        assert fs.exists("destination/path")
        minio_client.fget_object.assert_called_once_with(
            "my-bucket",
            "source/path/foo",
            str(Path("destination/path/foo")),
            version_id="foo-minus-31-days",
        )

    def test_download__rewinded_object_has_delete_marker__dont_download_it(
        self, mocker: MockerFixture, fs: FakeFilesystem
    ) -> None:
        """After a rewind, it shouldn't get future versions of objects."""
        # Arrange
        past = datetime.now(timezone.utc) - timedelta(days=30)
        logger = mocker.Mock(spec=ILogger)
        minio_client = mocker.Mock(spec=Minio)
        rewinder = Rewinder(minio_client, logger)
        minio_client.list_objects.return_value = (
            make_object(
                "source/path/foo",
                bucket_name="my-bucket",
                version_id=f"foo-minus-{30 - days}-days",
                last_modified=past + timedelta(days=days),
                is_delete_marker=(days == -1),  # Only the targetted version has the delete marker.
            )
            for days in (-2, -1, 1, 2)
        )

        # Act
        rewinder.download("my-bucket", "source/path", Path("destination/path"), past)

        # Assert
        logger.error.assert_called()
        assert not fs.exists("destination/path")
        minio_client.fget_object.assert_not_called()

    def test_download__dont_get_objects_with_delete_marker(self, mocker: MockerFixture, fs: FakeFilesystem) -> None:
        """It should skip downloading objects that have a delete marker."""
        # Arrange
        logger = mocker.Mock(spec=ILogger)
        minio_client = mocker.Mock(spec=Minio)
        rewinder = Rewinder(minio_client, logger)
        minio_client.list_objects.return_value = (
            make_object(
                f"source/path/{key}",
                bucket_name="my-bucket",
                is_delete_marker=delete_marker,
                version_id=("" if delete_marker else "not-") + "deleted",
            )
            for key, delete_marker in (
                (name, i % 2 == 1) for i, name in enumerate(("foo", "bar", "baz", "qux", "quux"))
            )
        )

        # Act
        rewinder.download("my-bucket", "source/path", Path("destination/path"))

        # Assert
        assert fs.exists("destination/path")  # Destination directory has been created.
        assert sorted(minio_client.fget_object.call_args_list, key=lambda call: call.args[1]) == [
            mocker.call(
                "my-bucket",
                f"source/path/{name}",
                str(Path(f"destination/path/{name}")),
                version_id="not-deleted",
            )
            for name in ("baz", "foo", "quux")
        ]

    def test_download__object_already_exists_and_content_is_the_same__skip_download(
        self, mocker: MockerFixture, fs: FakeFilesystem
    ) -> None:
        """It should skip downloading objects that already exist in the destination directory."""
        # Arrange
        logger = mocker.Mock(spec=ILogger)
        minio_client = mocker.Mock(spec=Minio)
        rewinder = Rewinder(minio_client, logger)
        minio_client.list_objects.return_value = itertools.chain(
            (make_object("source/path/empty-file", "my-bucket", "v42", etag=hashlib.md5(b"").hexdigest()),),
            (
                make_object(
                    object_name=f"source/path/{key}",
                    bucket_name="my-bucket",
                    version_id=f"v{i}",
                    etag=hashlib.md5(key.encode("utf-8")).hexdigest(),
                )
                for i, key in enumerate(("foo", "bar", "baz", "qux", "quux"))
            ),
        )
        fs.create_file("destination/path/empty-file", contents="")
        fs.create_file("destination/path/bar", contents="bar")
        fs.create_file("destination/path/qux", contents="qux")
        fs.create_file("destination/baz", contents="baz")  # Not in destination directory.
        dest_path = Path("destination/path")

        # Act
        rewinder.download("my-bucket", "source/path", dest_path)

        # Assert
        assert fs.exists("destination/path")  # Destination directory has been created.
        debug = [call.args[0] for call in logger.debug.call_args_list]
        assert all(
            f"Skipping download: {dest_path / name}, local and online are the same version." in debug
            for name in ["empty-file", "bar", "qux"]
        )
        assert sorted(minio_client.fget_object.call_args_list, key=lambda call: call.kwargs["version_id"]) == [
            mocker.call(
                "my-bucket",
                f"source/path/{name}",
                str(Path(f"destination/path/{name}")),
                version_id=f"v{i}",
            )
            for i, name in ((0, "foo"), (2, "baz"), (4, "quux"))
        ]

    def test_download__object_already_exists_and_content_is_different__download_object(
        self, mocker: MockerFixture, fs: FakeFilesystem
    ) -> None:
        """It should download objects that already exist in the destination directory but have chagned."""
        # Arrange
        logger = mocker.Mock(spec=ILogger)
        minio_client = mocker.Mock(spec=Minio)
        rewinder = Rewinder(minio_client, logger)
        minio_client.list_objects.return_value = iter(
            [
                make_object(
                    "source/path/foo",
                    bucket_name="my-bucket",
                    version_id="v1",
                    etag=hashlib.md5(b"new_foo").hexdigest(),
                )
            ]
        )
        fs.create_file("destination/path/foo", contents="old_foo")

        # Act
        rewinder.download("my-bucket", "source/path", Path("destination/path"))

        # Assert
        assert fs.exists("destination/path")  # Destination directory has been created.
        warning = f"Destination directory '{Path('destination/path')}' is not empty."
        logger.warning.assert_called_once_with(warning)
        minio_client.fget_object.assert_called_once_with(
            "my-bucket",
            "source/path/foo",
            str(Path("destination/path/foo")),
            version_id="v1",
        )

    def test_download__large_object_already_exists_and_content_is_the_same__skip_download(
        self, mocker: MockerFixture, fs: FakeFilesystem
    ) -> None:
        """It should skip downloading large objects that already exist in the destination directory.

        This tests the ETag computation code, which depends on the size of the
        chunks used in the multipart uploads in MinIO.
        """
        # Arrange
        part_size = random.randrange(256, 512)
        content_size = 1024
        parts = (content_size + part_size - 1) // part_size

        # Generate content and compute expected ETag.
        content = random.getrandbits(content_size * 8).to_bytes(content_size, "big")
        digests = (hashlib.md5(content[i * part_size : (i + 1) * part_size]).digest() for i in range(parts))
        etag = hashlib.md5(b"".join(digests)).hexdigest() + f"-{parts}"

        logger = mocker.Mock(spec=ILogger)
        minio_client = mocker.Mock(spec=Minio)
        rewinder = Rewinder(minio_client, logger, part_size=part_size)
        minio_client.list_objects.return_value = iter(
            [make_object("source/path/foo", bucket_name="my-bucket", version_id="v1", etag=etag)]
        )
        fs.create_file("destination/path/foo", contents=content)

        # Act
        rewinder.download("my-bucket", "source/path", Path("destination/path"))

        # Assert
        assert fs.exists("destination/path")  # Destination directory has been created.
        debug = f'Skipping download: {Path("destination/path/foo")}, local and online are the same version.'
        assert mocker.call(debug) in logger.debug.call_args_list
        minio_client.fget_object.assert_not_called()

    @pytest.mark.parametrize("allow_create_and_delete", [False, True])
    def test_build_plan__no_objects_in_minio__create_objects(
        self, allow_create_and_delete: bool, mocker: MockerFixture, fs: FakeFilesystem
    ) -> None:
        """It should create new objects in MinIO if there's files in the current directory."""
        # Arrange
        local_dir = Path("path/to/local/data")
        fs.create_dir(local_dir)
        for path in ("foo.txt", "bar/baz.txt", "qux/quux/quuux.txt"):  # Create three local files.
            fs.create_file(local_dir / path, contents=path)

        minio_prefix = S3Path("s3://my-bucket/minio/path/prefix/")
        minio_client = mocker.Mock(spec=Minio)
        rewinder = Rewinder(minio_client, mocker.Mock(spec=ILogger))
        minio_client.list_objects.return_value = iter([])  # List objects returns no objects.

        # Act
        plan = rewinder.build_plan(local_dir, minio_prefix, allow_create_and_delete=allow_create_and_delete)

        # Assert
        assert plan.multipart_upload_part_size == DEFAULT_MULTIPART_UPLOAD_PART_SIZE
        expected_items = (
            []
            if not allow_create_and_delete
            else [
                PlanItem.create(local_dir / path, minio_prefix / path)
                for path in ("bar/baz.txt", "foo.txt", "qux/quux/quuux.txt")
            ]
        )
        assert sorted(plan.items, key=lambda op: str(op.minio_path)) == expected_items

    @pytest.mark.parametrize("allow_create_and_delete", [False, True])
    def test_build_plan__empty_directory__remove_objects(
        self, allow_create_and_delete: bool, mocker: MockerFixture, fs: FakeFilesystem
    ) -> None:
        """It should remove objects in MinIO if the local directory does not contain them."""
        # Arrange
        local_dir = Path("path/to/local/data")
        minio_prefix = S3Path("s3://my-bucket/minio/path/prefix/")
        fs.create_dir(local_dir)  # Create empty dir

        minio_client = mocker.Mock(spec=Minio)
        rewinder = Rewinder(minio_client, mocker.Mock(spec=ILogger))
        minio_client.list_objects.return_value = (
            MinioObject(
                bucket_name=minio_prefix.bucket,
                object_name=(minio_prefix / path).key,
                etag=hashlib.md5(path.encode("utf-8")).hexdigest(),
            )
            for path in ("foo.txt", "bar/baz.txt", "qux/quux/quuux.txt")
        )

        # Act
        plan = rewinder.build_plan(local_dir, minio_prefix, allow_create_and_delete=allow_create_and_delete)

        # Assert
        assert plan.multipart_upload_part_size == DEFAULT_MULTIPART_UPLOAD_PART_SIZE
        expected_items = (
            []
            if not allow_create_and_delete
            else [
                PlanItem.remove(minio_prefix / path)
                for path in (
                    "bar/baz.txt",
                    "foo.txt",
                    "qux/quux/quuux.txt",
                )
            ]
        )
        assert sorted(plan.items, key=lambda op: str(op.minio_path)) == expected_items

    @pytest.mark.parametrize("allow_create_and_delete", [False, True])
    def test_build_plan__local_files_exists_in_minio_with_no_changes__empty_plan(
        self,
        allow_create_and_delete: bool,
        mocker: MockerFixture,
        fs: FakeFilesystem,
    ) -> None:
        """It shouldn't re-upload files if the content is the same."""
        # Arrange
        local_dir = Path("path/to/local/data")
        minio_prefix = S3Path("s3://my-bucket/minio/path/prefix/")
        minio_client = mocker.Mock(spec=Minio)
        rewinder = Rewinder(minio_client, mocker.Mock(spec=ILogger))

        paths = ["foo.txt", "bar/baz.txt", "qux/quux/quuux.txt"]
        # Create objects in MinIO
        minio_client.list_objects.return_value = (
            MinioObject(
                bucket_name=minio_prefix.bucket,
                object_name=(minio_prefix / path).key,
                etag=hashlib.md5(path.encode("utf-8")).hexdigest(),
            )
            for path in paths
        )
        # Create files in filesystem
        for path in paths:
            fs.create_file(local_dir / path, contents=path)

        # Act
        plan = rewinder.build_plan(local_dir, minio_prefix, allow_create_and_delete=allow_create_and_delete)

        # Assert
        assert plan.multipart_upload_part_size == DEFAULT_MULTIPART_UPLOAD_PART_SIZE
        assert not plan.items

    @pytest.mark.parametrize("allow_create_and_delete", [False, True])
    def test_build_plan__local_files_exists_in_minio_with_changes__upload_files(
        self,
        allow_create_and_delete: bool,
        mocker: MockerFixture,
        fs: FakeFilesystem,
    ) -> None:
        """It should re-upload files if there's changes in the content."""
        # Arrange
        local_dir = Path("path/to/local/data")
        minio_prefix = S3Path("s3://my-bucket/minio/path/prefix/")
        minio_client = mocker.Mock(spec=Minio)
        rewinder = Rewinder(minio_client, mocker.Mock(spec=ILogger))

        paths = ["foo.txt", "bar/baz.txt", "qux/quux/quuux.txt"]
        # Create objects in MinIO
        minio_client.list_objects.return_value = (
            MinioObject(
                bucket_name=minio_prefix.bucket,
                object_name=(minio_prefix / path).key,
                etag=hashlib.md5(path.encode("utf-8")).hexdigest(),
            )
            for path in paths
        )
        # Create files in filesystem
        fs.create_dir(local_dir)  # Create empty dir
        for path in paths:
            fs.create_file(local_dir / path, contents=f"{path} changed!")

        # Act
        plan = rewinder.build_plan(local_dir, minio_prefix, allow_create_and_delete=allow_create_and_delete)

        # Assert
        assert plan.multipart_upload_part_size == DEFAULT_MULTIPART_UPLOAD_PART_SIZE
        assert sorted(plan.items, key=lambda op: str(op.minio_path)) == [
            PlanItem.update(local_dir / path, minio_prefix / path)
            for path in (
                "bar/baz.txt",
                "foo.txt",
                "qux/quux/quuux.txt",
            )
        ]

    @pytest.mark.parametrize("allow_create_and_delete", [False, True])
    def test_build_plan__mixed_operations(
        self, allow_create_and_delete: bool, mocker: MockerFixture, fs: FakeFilesystem
    ) -> None:
        """It should include all different kind of operations in the computed plan.

        This test covers all of the branches in the big `if` statement in the `build_plan`
        method of the `Rewinder`.
        """
        # Arrange
        local_dir = Path("path/to/local/data")
        minio_prefix = S3Path("s3://my-bucket/minio/path/prefix/")
        minio_client = mocker.Mock(spec=Minio)
        rewinder = Rewinder(minio_client, mocker.Mock(spec=ILogger))

        # Create files in filesystem.
        fs.create_dir(local_dir)  # Create empty dir
        fs.create_file(local_dir / "new-file", contents="This file is new.")
        fs.create_file(local_dir / "unchanged-file", contents="Content unchanged.")
        fs.create_file(local_dir / "changed-file", contents="This content is new.")

        # Create objects in MinIO
        minio_client.list_objects.return_value = [
            MinioObject(
                bucket_name=minio_prefix.bucket,
                object_name=(minio_prefix / "unchanged-file").key,
                etag=hashlib.md5(b"Content unchanged.").hexdigest(),
            ),
            MinioObject(
                bucket_name=minio_prefix.bucket,
                object_name=(minio_prefix / "changed-file").key,
                etag=hashlib.md5(b"This content is old.").hexdigest(),
            ),
            MinioObject(
                bucket_name=minio_prefix.bucket,
                object_name=(minio_prefix / "removed-file").key,
                etag=hashlib.md5(b"This object is removed.").hexdigest(),
            ),
        ]

        # Act
        plan = rewinder.build_plan(local_dir, minio_prefix, allow_create_and_delete=allow_create_and_delete)

        # Assert
        assert plan.multipart_upload_part_size == DEFAULT_MULTIPART_UPLOAD_PART_SIZE
        items = [
            PlanItem.update(local_dir / "changed-file", minio_prefix / "changed-file"),
            None if not allow_create_and_delete else PlanItem.create(local_dir / "new-file", minio_prefix / "new-file"),
            None if not allow_create_and_delete else PlanItem.remove(minio_prefix / "removed-file"),
        ]
        assert plan.items == list(filter(None, items))

    @pytest.mark.parametrize("allow_create_and_delete", [False, True])
    def test_build_plan__randomized_mixed_operations(
        self, allow_create_and_delete: bool, mocker: MockerFixture, fs: FakeFilesystem
    ) -> None:
        """It should include all different kind of operations in the computed plan.

        This test covers the case of a large amount of changes between the local
        directory and MinIO. It's randomized so it should be able to catch any remaining
        bugs in the implementation.
        """
        # Arrange
        local_dir = Path("path/to/local/data")
        minio_prefix = S3Path("s3://my-bucket/minio/path/prefix/")
        minio_client = mocker.Mock(spec=Minio)
        rewinder = Rewinder(minio_client, mocker.Mock(spec=ILogger))

        # Create files in filesystem and MinIO.
        file_count = random.randrange(10, 100)
        create_count, update_count, remove_count = 0, 0, 0
        minio_objects = []
        for i in range(file_count):
            name = f"file-{i}"

            file_content, object_content = name, name
            create_file, create_object = True, True
            rand = random.random()
            if 0.25 <= rand < 0.5:  # Changed object
                update_count += 1
                file_content = f"{name} changed!"
            elif 0.5 <= rand < 0.75:  # Deleted object
                remove_count += 1
                create_file = False
            elif 0.75 <= rand <= 1.0:  # Created object
                create_count += 1
                create_object = False

            if create_file:
                fs.create_file(local_dir / name, contents=file_content)
            if create_object:
                minio_objects.append(
                    MinioObject(
                        bucket_name=minio_prefix.bucket,
                        object_name=(minio_prefix / name).key,
                        etag=hashlib.md5(object_content.encode("utf-8")).hexdigest(),
                    )
                )

        minio_client.list_objects.return_value = minio_objects

        # Act
        plan = rewinder.build_plan(local_dir, minio_prefix, allow_create_and_delete=allow_create_and_delete)

        # Assert
        if not allow_create_and_delete:
            create_count = remove_count = 0
        assert plan.multipart_upload_part_size == DEFAULT_MULTIPART_UPLOAD_PART_SIZE
        assert sum(1 for op in plan.items if op.operation == Operation.CREATE) == create_count
        assert sum(1 for op in plan.items if op.operation == Operation.UPDATE) == update_count
        assert sum(1 for op in plan.items if op.operation == Operation.REMOVE) == remove_count
        assert len(plan.items) == create_count + update_count + remove_count

    def test_build_plan__multipart_upload_etag_computation(
        self,
        mocker: MockerFixture,
        fs: FakeFilesystem,
    ) -> None:
        """It should compute the ETag correctly in the case of a multipart upload."""
        # Arrange
        local_dir = Path("path/to/local/data/")
        minio_prefix = S3Path("s3://my-bucket/minio/path/prefix")
        minio_client = mocker.Mock(spec=Minio)
        rewinder = Rewinder(minio_client, mocker.Mock(spec=ILogger))

        size = 1024
        part_size = random.randrange(256, 1024)
        content = random.getrandbits(size * 8).to_bytes(size, "big")
        part_count = (size + part_size - 1) // part_size
        assert len(content) == 1024  # 1024 random bytes.
        hashes = [hashlib.md5(content[i * part_size : (i + 1) * part_size]).digest() for i in range(part_count)]
        expected_etag = hashlib.md5(b"".join(hashes)).hexdigest() + f"-{part_count}"

        fs.create_file(local_dir / "foo/bar.baz", contents=content)
        minio_client.list_objects.return_value = [
            MinioObject(
                bucket_name=minio_prefix.bucket,
                object_name=(minio_prefix / "foo/bar.baz").key,
                etag=expected_etag,
            )
        ]

        # Act
        plan = rewinder.build_plan(local_dir, minio_prefix, part_size=part_size)

        # Assert
        assert plan.multipart_upload_part_size == part_size
        assert not plan.items

    @pytest.mark.parametrize("type_", (Operation.CREATE, Operation.UPDATE))
    def test_execute_plan__create_or_update_operation__put_object(
        self, type_: Operation, mocker: MockerFixture, fs: FakeFilesystem
    ) -> None:
        """It should call `fput_object` on `CREATE` operations."""
        # Arrange
        local_path = Path("local/path/to/data.txt")
        minio_path = S3Path("s3://bucket/remote/path/to/data.txt")
        minio_client = mocker.Mock(spec=Minio)
        rewinder = Rewinder(minio_client, mocker.Mock(spec=ILogger))
        plan = Plan(
            local_dir=Path("local/path"),
            minio_prefix=S3Path("s3://bucket/remote/path"),
            items=[PlanItem(operation=type_, minio_path=minio_path, local_path=local_path)],
            multipart_upload_part_size=42 * 1024,
        )
        fs.create_file(local_path)

        # Act
        rewinder.execute_plan(plan)

        # Assert
        minio_client.fput_object.assert_called_once_with(
            bucket_name="bucket",
            object_name="remote/path/to/data.txt",
            file_path=str(local_path),
            part_size=42 * 1024,
            tags=None,
        )

    @pytest.mark.parametrize("type_", (Operation.CREATE, Operation.UPDATE))
    def test_execute_plan__create_or_update_operation_with_tags__put_object(
        self, type_: Operation, mocker: MockerFixture, fs: FakeFilesystem
    ) -> None:
        """It should call `fput_object` on `CREATE` operations."""
        # Arrange
        local_path = Path("local/path/to/data.txt")
        minio_path = S3Path("s3://bucket/remote/path/to/data.txt")
        minio_client = mocker.Mock(spec=Minio)
        rewinder = Rewinder(minio_client, mocker.Mock(spec=ILogger))
        plan = Plan(
            local_dir=Path("local/path"),
            minio_prefix=S3Path("s3://bucket/remote/path"),
            items=[PlanItem(operation=type_, minio_path=minio_path, local_path=local_path)],
            multipart_upload_part_size=42 * 1024,
            tags={"foo": "bar"},  # type: ignore
        )
        fs.create_file(local_path)

        # Act
        rewinder.execute_plan(plan)

        # Assert
        minio_client.fput_object.assert_called_once_with(
            bucket_name="bucket",
            object_name="remote/path/to/data.txt",
            file_path=str(local_path),
            part_size=42 * 1024,
            tags={"foo": "bar"},
        )

    def test_execute_plan__create_minio_keep_file__put_object(self, mocker: MockerFixture, fs: FakeFilesystem) -> None:
        """It should call `fput_object` on `CREATE` operations."""
        # Arrange
        local_path = Path("local/path/to/.miniokeep")
        minio_path = S3Path("s3://bucket/remote/path/to/.miniokeep")
        minio_client = mocker.Mock(spec=Minio)
        rewinder = Rewinder(minio_client, mocker.Mock(spec=ILogger))
        plan = Plan(
            local_dir=Path("local/path"),
            minio_prefix=S3Path("s3://bucket/remote/path"),
            items=[PlanItem.create(local_path, minio_path)],
            multipart_upload_part_size=42 * 1024,
        )

        # Act
        rewinder.execute_plan(plan)

        # Assert
        minio_client.put_object.assert_called_once_with(
            bucket_name="bucket",
            object_name="remote/path/to/.miniokeep",
            data=mocker.ANY,  # Empty BytesIO object.
            length=0,
            tags=None,
        )
        assert minio_client.put_object.call_args.kwargs["data"].read() == b""

    def test_execute_plan__create_minio_keep_file_with_tags__put_object(
        self, mocker: MockerFixture, fs: FakeFilesystem
    ) -> None:
        """It should call `fput_object` on `CREATE` operations."""
        # Arrange
        local_path = Path("local/path/to/.miniokeep")
        minio_path = S3Path("s3://bucket/remote/path/to/.miniokeep")
        minio_client = mocker.Mock(spec=Minio)
        rewinder = Rewinder(minio_client, mocker.Mock(spec=ILogger))
        plan = Plan(
            local_dir=Path("local/path"),
            minio_prefix=S3Path("s3://bucket/remote/path"),
            items=[PlanItem.create(local_path, minio_path)],
            multipart_upload_part_size=42 * 1024,
            tags={"foo": "bar"},  # type: ignore
        )

        # Act
        rewinder.execute_plan(plan)

        # Assert
        minio_client.put_object.assert_called_once_with(
            bucket_name="bucket",
            object_name="remote/path/to/.miniokeep",
            data=mocker.ANY,  # Empty BytesIO object.
            length=0,
            tags={"foo": "bar"},
        )
        assert minio_client.put_object.call_args.kwargs["data"].read() == b""

    @pytest.mark.parametrize("type_", (Operation.CREATE, Operation.UPDATE))
    def test_execute_plan__create_or_update_non_existent_file__raise_error(
        self, type_: Operation, mocker: MockerFixture
    ) -> None:
        """It should raise an error when creating/updating non-existent files."""
        # Arrange
        local_path = Path("local/path/to/data.txt")
        minio_path = S3Path("s3://bucket/remote/path/to/data.txt")
        minio_client = mocker.Mock(spec=Minio)
        rewinder = Rewinder(minio_client, mocker.Mock(spec=ILogger))
        plan = Plan(
            local_dir=Path("local/path"),
            minio_prefix=S3Path("s3://bucket/remote/path"),
            items=[PlanItem(operation=type_, minio_path=minio_path, local_path=local_path)],
        )

        # Act
        with pytest.raises(RuntimeError, match="non-existent local file"):
            rewinder.execute_plan(plan)

    @pytest.mark.parametrize("type_", (Operation.CREATE, Operation.UPDATE))
    def test_execute_plan__create_or_update_without_local_path__raise_error(
        self, type_: Operation, mocker: MockerFixture
    ) -> None:
        """It should call `fput_object` on `CREATE` operations."""
        # Arrange
        minio_path = S3Path("s3://bucket/remote/path/to/data.txt")
        minio_client = mocker.Mock(spec=Minio)
        rewinder = Rewinder(minio_client, mocker.Mock(spec=ILogger))
        plan = Plan(
            local_dir=Path("local/path"),
            minio_prefix=S3Path("s3://bucket/remote/path"),
            items=[PlanItem(operation=type_, minio_path=minio_path, local_path=None)],
        )

        # Act
        with pytest.raises(RuntimeError, match="must have"):
            rewinder.execute_plan(plan)

    def test_execute_plan__remove_operation__delete_object(self, mocker: MockerFixture) -> None:
        """It should call `remove_object` on `REMOVE` operations."""
        # Arrange
        minio_path = S3Path("s3://bucket/remote/path/to/data.txt")
        minio_client = mocker.Mock(spec=Minio)
        rewinder = Rewinder(minio_client, mocker.Mock(spec=ILogger))
        plan = Plan(
            local_dir=Path("local/path"),
            minio_prefix=S3Path("s3://bucket/remote/path"),
            items=[PlanItem.remove(minio_path)],
        )

        # Act
        rewinder.execute_plan(plan)

        # Assert
        minio_client.remove_object.assert_called_once_with(
            bucket_name="bucket",
            object_name="remote/path/to/data.txt",
        )

    def test_execute_plan__randomized_mixed_operations(self, mocker: MockerFixture, fs: FakeFilesystem) -> None:
        """It should call `fput_object` on `CREATE`/`UPDATE` and `delete_object` on `REMOVE`s."""
        # Arrange
        minio_prefix = S3Path("s3://bucket/prefix/")
        local_dir = Path("local/dir/")
        minio_client = mocker.Mock(spec=Minio)
        rewinder = Rewinder(minio_client, mocker.Mock(spec=ILogger), 42 * 1024)

        operations = [
            PlanItem(
                operation=type_,
                minio_path=minio_prefix / file_name,
                local_path=None if type_ == Operation.REMOVE else local_dir / file_name,
            )
            for type_, file_name in [
                (
                    random.choice([op for op in Operation if op != Operation.NONE]),
                    "".join(chr(random.randrange(ord("a"), ord("z") + 1)) for _ in range(random.randrange(2, 10))),
                )
                for _ in range(random.randrange(42))
            ]
        ]
        for op in operations:
            if op.operation != Operation.REMOVE and op.local_path is not None:
                fs.create_file(op.local_path)

        # Act
        rewinder.execute_plan(
            plan=Plan(
                local_dir=Path("local/path"),
                minio_prefix=S3Path("s3://bucket/remote/path"),
                items=operations,
            )
        )

        # Assert
        remove_count = len([op for op in operations if op.operation == Operation.REMOVE])
        assert minio_client.remove_object.call_count == remove_count
        assert minio_client.fput_object.call_count == len(operations) - remove_count

    @pytest.mark.parametrize("add_tags", [pytest.param(True, id="add_tags"), pytest.param(False, id="skip_tags")])
    def test_detect_conflicts__object_didnt_exist_but_now_does__create_conflict(
        self, add_tags: bool, mocker: MockerFixture
    ) -> None:
        # Arrange
        now = datetime.now(timezone.utc)
        minio_client = mocker.Mock(spec=Minio)
        rewinder = Rewinder(minio_client, mocker.Mock(spec=ILogger))
        prefix = S3Path.from_bucket("my-bucket") / "data"
        tags = Tags()
        tags["jira-issue-id"] = "FOO-123"
        new_object = MinioObject("my-bucket", "data/foo", now)
        minio_client.list_objects.return_value = [new_object]
        minio_client.get_object_tags.return_value = tags

        # Act
        conflict, *other_conflicts = rewinder.detect_conflicts(
            prefix, now - timedelta(days=1), add_tags_to_latest=add_tags
        )

        # Assert
        assert not other_conflicts
        assert conflict.rewinded_version is None
        assert minio_objects_equal(conflict.latest_version, new_object)
        assert conflict.latest_version.tags == (tags if add_tags else None)
        assert conflict.update_type == Operation.CREATE
        minio_client.get_object_tags.call_count == int(add_tags)

    @pytest.mark.parametrize("add_tags", [pytest.param(True, id="add_tags"), pytest.param(False, id="skip_tags")])
    def test_detect_conflicts__object_didnt_exist_but_is_now_a_delete_marker__no_conflict(
        self, add_tags: bool, mocker: MockerFixture
    ) -> None:
        # Arrange
        now = datetime.now(timezone.utc)
        minio_client = mocker.Mock(spec=Minio)
        rewinder = Rewinder(minio_client, mocker.Mock(spec=ILogger))
        prefix = S3Path.from_bucket("my-bucket") / "data"
        minio_client.list_objects.return_value = [
            MinioObject("my-bucket", "data/foo", now, is_delete_marker=True),
        ]

        # Act
        conflicts = rewinder.detect_conflicts(prefix, now - timedelta(days=1), add_tags_to_latest=add_tags)

        # Assert
        assert not conflicts
        minio_client.get_object_tags.assert_not_called()

    @pytest.mark.parametrize("add_tags", [pytest.param(True, id="add_tags"), pytest.param(False, id="skip_tags")])
    def test_detect_conflicts__rewinded_object_is_the_latest_version__no_conflict(
        self, add_tags: bool, mocker: MockerFixture
    ) -> None:
        # Arrange
        now = datetime.now(timezone.utc)
        yesterday = now - timedelta(days=1)
        minio_client = mocker.Mock(spec=Minio)
        rewinder = Rewinder(minio_client, mocker.Mock(spec=ILogger))
        prefix = S3Path.from_bucket("my-bucket") / "data"
        minio_client.list_objects.return_value = [
            MinioObject("my-bucket", "data/foo", yesterday, etag=hashlib.md5(b"old").hexdigest(), version_id="v1"),
            MinioObject("my-bucket", "data/foo", now, etag=hashlib.md5(b"new").hexdigest(), version_id="v2"),
        ]

        # Act
        conflicts = rewinder.detect_conflicts(prefix, now, add_tags_to_latest=add_tags)

        # Assert
        assert not conflicts
        minio_client.get_object_tags.assert_not_called()

    @pytest.mark.parametrize("add_tags", [pytest.param(True, id="add_tags"), pytest.param(False, id="skip_tags")])
    def test_detect_conflicts__new_version_exists__update_conflict(self, add_tags: bool, mocker: MockerFixture) -> None:
        # Arrange
        now = datetime.now(timezone.utc)
        yesterday = now - timedelta(days=1)
        minio_client = mocker.Mock(spec=Minio)
        rewinder = Rewinder(minio_client, mocker.Mock(spec=ILogger))
        prefix = S3Path.from_bucket("my-bucket") / "data"
        tags = Tags()
        tags["jira-issue-id"] = "FOO-123"
        objects = [
            MinioObject("my-bucket", "data/foo", now, etag=hashlib.md5(b"new!").hexdigest(), version_id="2"),
            MinioObject("my-bucket", "data/foo", yesterday, etag=hashlib.md5(b"old").hexdigest(), version_id="1"),
        ]
        minio_client.list_objects.return_value = objects
        minio_client.get_object_tags.return_value = tags

        # Act
        conflict, *conflicts = rewinder.detect_conflicts(prefix, yesterday, add_tags_to_latest=add_tags)

        # Assert
        assert not conflicts
        assert conflict.rewinded_version is not None
        assert minio_objects_equal(conflict.latest_version, objects[0])
        assert minio_objects_equal(conflict.rewinded_version, objects[1])
        assert conflict.latest_version.tags == (tags if add_tags else None)
        assert conflict.update_type == Operation.UPDATE

    @pytest.mark.parametrize("add_tags", [pytest.param(True, id="add_tags"), pytest.param(False, id="skip_tags")])
    def test_detect_conflicts__new_version_exists_but_content_not_different__no_conflict(
        self, add_tags: bool, mocker: MockerFixture
    ) -> None:
        # Arrange
        now = datetime.now(timezone.utc)
        yesterday = now - timedelta(days=1)
        minio_client = mocker.Mock(spec=Minio)
        rewinder = Rewinder(minio_client, mocker.Mock(spec=ILogger))
        prefix = S3Path.from_bucket("my-bucket") / "data"
        etag = hashlib.md5(b"old").hexdigest()
        minio_client.list_objects.return_value = [
            MinioObject("my-bucket", "data/foo", yesterday, etag=etag, version_id="1"),
            MinioObject("my-bucket", "data/foo", now, etag=etag, version_id="2"),
        ]

        # Act
        conflicts = rewinder.detect_conflicts(prefix, yesterday, add_tags_to_latest=add_tags)

        # Assert
        assert not conflicts
        minio_client.get_object_tags.assert_not_called()

    @pytest.mark.parametrize("add_tags", [pytest.param(True, id="add_tags"), pytest.param(False, id="skip_tags")])
    def test_detect_conflicts__new_version_exists_but_is_a_delete_marker__remove_conflict(
        self, add_tags: bool, mocker: MockerFixture
    ) -> None:
        # Arrange
        now = datetime.now(timezone.utc)
        yesterday = now - timedelta(days=1)
        minio_client = mocker.Mock(spec=Minio)
        rewinder = Rewinder(minio_client, mocker.Mock(spec=ILogger))
        prefix = S3Path.from_bucket("my-bucket") / "data"
        objects = [
            MinioObject("my-bucket", "data/foo", yesterday, version_id="1"),
            MinioObject("my-bucket", "data/foo", now, version_id="2", is_delete_marker=True),
        ]
        minio_client.list_objects.return_value = objects

        # Act
        conflict, *conflicts = rewinder.detect_conflicts(prefix, yesterday, add_tags_to_latest=add_tags)

        # Assert
        assert not conflicts
        assert conflict == VersionPair(objects[0], objects[1])
        assert conflict.update_type == Operation.REMOVE
        minio_client.get_object_tags.assert_not_called()

    @pytest.mark.parametrize("add_tags", [pytest.param(True, id="add_tags"), pytest.param(False, id="skip_tags")])
    def test_detect_conflicts__old_version_is_delete_marker_but_new_version_is_not__create_conflict(
        self, add_tags: bool, mocker: MockerFixture
    ) -> None:
        # Arrange
        now = datetime.now(timezone.utc)
        minio_client = mocker.Mock(spec=Minio)
        rewinder = Rewinder(minio_client, mocker.Mock(spec=ILogger))
        prefix = S3Path.from_bucket("my-bucket") / "data"
        tags = Tags()
        tags["jira-issue-id"] = "FOO-123"
        objects = [
            MinioObject("my-bucket", "data/foo", now - timedelta(days=2), version_id="1"),
            MinioObject("my-bucket", "data/foo", now - timedelta(days=1), version_id="2", is_delete_marker=True),
            MinioObject("my-bucket", "data/foo", now, version_id="3"),
        ]
        minio_client.list_objects.return_value = objects
        minio_client.get_object_tags.return_value = tags

        # Act
        conflict, *conflicts = rewinder.detect_conflicts(prefix, now - timedelta(hours=12), add_tags_to_latest=add_tags)

        # Assert
        assert not conflicts
        assert conflict.rewinded_version is not None
        assert minio_objects_equal(conflict.latest_version, objects[2])
        assert minio_objects_equal(conflict.rewinded_version, objects[1])
        assert conflict.latest_version.tags == (tags if add_tags else None)
        assert conflict.update_type == Operation.CREATE
        assert minio_client.get_object_tags.call_count == int(add_tags)

    @pytest.mark.parametrize("add_tags", [pytest.param(True, id="add_tags"), pytest.param(False, id="skip_tags")])
    def test_detect_conflicts__old_version_is_delete_marker_and_new_version_is_too__no_conflict(
        self, add_tags: bool, mocker: MockerFixture
    ) -> None:
        # Arrange
        now = datetime.now(timezone.utc)
        minio_client = mocker.Mock(spec=Minio)
        rewinder = Rewinder(minio_client, mocker.Mock(spec=ILogger))
        prefix = S3Path.from_bucket("my-bucket") / "data"
        minio_client.list_objects.return_value = [
            MinioObject("my-bucket", "data/foo", now - timedelta(days=2), version_id="1"),
            MinioObject("my-bucket", "data/foo", now - timedelta(days=1), version_id="2", is_delete_marker=True),
            MinioObject("my-bucket", "data/foo", now, version_id="3", is_delete_marker=True),
        ]

        # Act
        conflicts = rewinder.detect_conflicts(prefix, now - timedelta(hours=12), add_tags_to_latest=add_tags)

        # Assert
        assert not conflicts
        minio_client.get_object_tags.assert_not_called()

    def test_detect_conflicts__multiple_files_and_versions_randomized(self, mocker: MockerFixture) -> None:
        """It should detect object conflict across a large amount of objects and object versions.

        This test performs a more complicated setup by preparing a large list of
        object versions. The test data is randomized so we're not relying on the
        order of the returned data.
        """
        # Arrange
        now = datetime.now(timezone.utc)
        rewind = now - timedelta(days=3)  # Rewind three days in the past.
        minio_client = mocker.Mock(spec=Minio)
        prefix = S3Path.from_bucket("my-bucket") / "data"
        rewinder = Rewinder(minio_client, mocker.Mock(spec=ILogger))

        # Prepare output of `list_objects`
        objects: List[MinioObject] = []
        object_count = random.randrange(10, 42)
        create_count, update_count, remove_count = 0, 0, 0  # Initialize counters.
        for i in range(object_count):
            key = f"data/file-{i}"
            version_count = random.randrange(1, 9)
            versions = [
                MinioObject(
                    bucket_name="my-bucket",
                    object_name=key,
                    last_modified=now - timedelta(days=j),
                    etag=hashlib.md5(f"{key}-v{j}".encode("utf-8")).hexdigest(),
                    version_id=f"{key}-v{j}",
                    is_delete_marker=False,
                )
                for j in range(version_count)
            ]
            past_object_exists = version_count > 3

            rand = random.random()
            if rand < 0.25:  # Do nothing to most recent version
                if past_object_exists:
                    update_count += 1  # Object has been updated.
                else:
                    create_count += 1  # Now this object exists.
            if 0.25 <= rand < 0.5:  # Most recent version is delete marker.
                versions[0] = MinioObject("my-bucket", key, now, is_delete_marker=True, version_id=f"{key}-v0")
                if past_object_exists:
                    remove_count += 1
            if 0.5 <= rand < 0.75:  # The past version is a delete marker.
                if past_object_exists:
                    versions[3] = MinioObject(
                        "my-bucket", key, now - timedelta(days=3), is_delete_marker=True, version_id=f"{key}-v3"
                    )
                create_count += 1
            if 0.75 <= rand:  # The past and latest versions have the same content.
                if past_object_exists:
                    etag = hashlib.md5(b"same content!").hexdigest()
                    versions[0] = MinioObject("my-bucket", key, now, etag, version_id=f"{key}-v0")
                    versions[3] = MinioObject("my-bucket", key, now - timedelta(days=3), etag, version_id=f"{key}-v3")
                else:
                    create_count += 1

            objects.extend(versions)

        random.shuffle(objects)  # Randomize order of returned objects
        minio_client.list_objects.return_value = objects

        # Act
        conflicts = rewinder.detect_conflicts(prefix, rewind)

        # Assert
        assert sum(1 for elem in conflicts if elem.update_type == Operation.CREATE) == create_count
        assert sum(1 for elem in conflicts if elem.update_type == Operation.UPDATE) == update_count
        assert sum(1 for elem in conflicts if elem.update_type == Operation.REMOVE) == remove_count
