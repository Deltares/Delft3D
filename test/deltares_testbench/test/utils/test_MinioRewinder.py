import hashlib
import itertools
import os
import random
from datetime import datetime, timedelta, timezone
from typing import Optional
from uuid import uuid4

import minio
import pytest
from minio.datatypes import Object as MinioObject
from pyfakefs.fake_filesystem import FakeFilesystem
from pytest import CaptureFixture
from pytest_mock import MockerFixture

from src.utils.minio_rewinder import Rewinder
from src.utils.logging.i_logger import ILogger


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


def make_version(dt: Optional[datetime] = None) -> str:
    dt = dt or datetime.now(timezone.utc)
    return dt.isoformat().split(".", 1)[0].replace("-", ".")


class TestMinioRewinder:
    @pytest.mark.parametrize(
        "version, expected_seconds",
        [
            ("2023.10.20T09:12", 0),
            ("2023.10.20T09:12:33", 33),
        ],
    )
    def test_rewind_correct_version_constructs_rewinder(
        self, version: str, expected_seconds: int, mocker: MockerFixture
    ):
        # Mock Minio and Rewinder
        minio_instance = mocker.Mock(spec=minio.Minio)
        logger = mocker.Mock(spec=ILogger)
        rewinder_instance = Rewinder(minio_instance, logger, version)
        assert rewinder_instance.rewind.year == 2023
        assert rewinder_instance.rewind.month == 10
        assert rewinder_instance.rewind.day == 20
        assert rewinder_instance.rewind.hour == 9
        assert rewinder_instance.rewind.minute == 12
        assert rewinder_instance.rewind.second == expected_seconds

    @pytest.mark.parametrize(
        "version",
        [
            ("v1.2.3"),
            ("1.2.3"),
            ("abcd.ef.ghTij:kl"),
        ],
    )
    def test_rewind_incorrect_version_throws_value_error(self, version: str, mocker: MockerFixture):
        # Mock Minio
        with pytest.raises(ValueError):
            minio_instance = mocker.Mock(spec=minio.Minio)
            logger = mocker.Mock(spec=ILogger)
            Rewinder(minio_instance, logger, version)

    def test_rewind_download_delete_marker(self, mocker: MockerFixture):
        # Arrange
        mock_minio_client = mocker.Mock(spec=minio.Minio)

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
        rewinder_instance = Rewinder(mock_minio_client, logger, "2023.10.20T12:00")
        rewinder_instance.download("my_bucket", "https://minio/browser", ".")

        # Assert
        expected_path = os.path.join(".", "object1_name")
        mock_minio_client.fget_object.assert_called_once_with(
            "my_bucket", "object1_name", expected_path, version_id=mocker.ANY
        )

    def test_rewind_download_before_rewind(self, mocker: MockerFixture):
        # Create a Mock for the Minio client
        mock_minio_client = mocker.Mock(spec=minio.Minio)

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

        rewinder_instance = Rewinder(mock_minio_client, logger, "2022.10.20T12:00")
        rewinder_instance.download("my_bucket", "https://minio/browser", ".")

        mock_minio_client.fget_object.assert_not_called()

    def test_rewind_download_after_rewind(self, mocker: MockerFixture):
        # Arrange
        mock_minio_client = mocker.Mock(spec=minio.Minio)

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
        rewinder_instance = Rewinder(mock_minio_client, logger, "2024.10.20T12:00")
        rewinder_instance.download("my_bucket", "https://minio/browser", ".")

        # Assert
        expected_path1 = os.path.join(".", "object1_name")
        mock_minio_client.fget_object.assert_any_call(
            "my_bucket", "object1_name", expected_path1, version_id=mocker.ANY
        )
        expected_path2 = os.path.join(".", "object2_name")
        mock_minio_client.fget_object.assert_any_call(
            "my_bucket", "object2_name", expected_path2, version_id=mocker.ANY
        )

    def test_rewind_download_on_rewind(self, mocker: MockerFixture):
        # Create a Mock for the Minio client
        mock_minio_client = mocker.Mock(spec=minio.Minio)

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
        rewinder_instance = Rewinder(mock_minio_client, logger, "2023.10.20T10:00")
        rewinder_instance.download("my_bucket", "https://minio/browser", ".")

        mock_minio_client.fget_object.assert_not_called()

    def test_download__use_multiple_keys__download_multiple_files(
        self, mocker: MockerFixture, fs: FakeFilesystem
    ) -> None:
        """It should list the objects in `source/path` and download them to directory `destination/path`."""
        # Arrange
        version = make_version()
        minio_client = mocker.Mock(spec=minio.Minio)
        logger = mocker.Mock(spec=ILogger)
        rewinder = Rewinder(minio_client, logger, version)
        minio_client.list_objects.return_value = (
            make_object(f"source/path/{key}", bucket_name="my-bucket", version_id=f"v{i}")
            for i, key in enumerate(("foo", "bar", "baz"))
        )

        # Act
        rewinder.download("my-bucket", "source/path", "destination/path")

        # Assert
        assert fs.exists("destination/path")  # Destination directory has been created.
        assert minio_client.fget_object.call_args_list == [
            mocker.call(
                "my-bucket",
                f"source/path/{name}",
                f"destination/path/{name}",
                version_id=f"v{i}",
            )
            for i, name in enumerate(("foo", "bar", "baz"))
        ]

    def test_download__source_path_has_trailing_slash(self, mocker: MockerFixture, fs: FakeFilesystem) -> None:
        """It should list the objects in `source/path/` and download them to directory `destination/path`."""
        # Arrange
        version = make_version()
        minio_client = mocker.Mock(spec=minio.Minio)
        logger = mocker.Mock(spec=ILogger)
        rewinder = Rewinder(minio_client, logger, version)
        minio_client.list_objects.return_value = iter(
            [make_object("source/path/foo", bucket_name="my-bucket", version_id="v1")]
        )

        # Act
        rewinder.download("my-bucket", "source/path/", "destination/path")

        # Assert
        assert fs.exists("destination/path")  # Destination directory has been created.
        minio_client.fget_object.assert_called_once_with(
            "my-bucket",
            "source/path/foo",
            "destination/path/foo",
            version_id="v1",
        )

    def test_download__destination_dir_exists_and_empty__no_message(
        self, mocker: MockerFixture, fs: FakeFilesystem, capsys: CaptureFixture
    ) -> None:
        """It should not print a message saying the destination dir is not empty."""
        # Arrange
        version = make_version()
        minio_client = mocker.Mock(spec=minio.Minio)
        logger = mocker.Mock(spec=ILogger)
        rewinder = Rewinder(minio_client, logger, version)
        minio_client.list_objects.return_value = iter(
            [make_object("source/path/foo", bucket_name="my-bucket", version_id="v1")]
        )
        fs.create_dir("destination/path")

        # Act
        rewinder.download("my-bucket", "source/path", "destination/path")

        # Assert
        assert fs.exists("destination/path")  # Destination dir still exists.
        logger.error.assert_not_called()
        minio_client.fget_object.assert_called_once_with(
            "my-bucket", "source/path/foo", "destination/path/foo", version_id="v1"
        )

    def test_download__destination_dir_exists_and_not_empty__print_not_empty_message(
        self, mocker: MockerFixture, fs: FakeFilesystem, capsys: CaptureFixture
    ) -> None:
        """It should not print a message saying the destination dir is not empty."""
        # Arrange
        version = make_version()
        minio_client = mocker.Mock(spec=minio.Minio)
        logger = mocker.Mock(spec=ILogger)
        rewinder = Rewinder(minio_client, logger, version)
        minio_client.list_objects.return_value = iter(
            [make_object("source/path/foo", bucket_name="my-bucket", version_id="v1")]
        )
        fs.create_file("destination/path/bar")

        # Act
        rewinder.download("my-bucket", "source/path", "destination/path")

        # Assert
        assert fs.exists("destination/path")  # Destination dir still exists.
        logger.warning.assert_called()  # Warn that destination dir is not empty.
        minio_client.fget_object.assert_called_once_with(
            "my-bucket", "source/path/foo", "destination/path/foo", version_id="v1"
        )

    def test_download__same_key_multiple_versions__get_latest_versions(
        self, mocker: MockerFixture, fs: FakeFilesystem
    ) -> None:
        """It should download only the latest versions of objects in Minio."""
        # Arrange
        version = make_version()
        minio_client = mocker.Mock(spec=minio.Minio)
        logger = mocker.Mock(spec=ILogger)
        rewinder = Rewinder(minio_client, logger, version)
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
        rewinder.download("my-bucket", "source/path", "destination/path")

        # Assert
        assert fs.exists("destination/path")  # Destination directory has been created.
        assert minio_client.fget_object.call_args_list == [
            mocker.call(
                "my-bucket",
                f"source/path/{name}",
                f"destination/path/{name}",
                version_id=f"{name}-minus-1-hours",
            )
            for name in ("foo", "bar", "baz")
        ]

    def test_download__rewind__dont_download_latest(self, mocker: MockerFixture, fs: FakeFilesystem) -> None:
        """After a rewind, it shouldn't get future versions of objects."""
        # Arrange
        past = datetime.now(timezone.utc) - timedelta(days=30)
        minio_client = mocker.Mock(spec=minio.Minio)
        logger = mocker.Mock(spec=ILogger)
        rewinder = Rewinder(minio_client, logger, make_version(past))
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
        rewinder.download("my-bucket", "source/path", "destination/path")

        # Assert
        assert fs.exists("destination/path")
        minio_client.fget_object.assert_called_once_with(
            "my-bucket",
            "source/path/foo",
            "destination/path/foo",
            version_id="foo-minus-31-days",
        )

    def test_download__rewinded_object_has_delete_marker__dont_download_it(
        self, mocker: MockerFixture, fs: FakeFilesystem, capsys: CaptureFixture
    ) -> None:
        """After a rewind, it shouldn't get future versions of objects."""
        # Arrange
        past = datetime.now(timezone.utc) - timedelta(days=30)
        minio_client = mocker.Mock(spec=minio.Minio)
        logger = mocker.Mock(spec=ILogger)
        rewinder = Rewinder(minio_client, logger, make_version(past))
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
        rewinder.download("my-bucket", "source/path", "destination/path")

        # Assert
        logger.error.assert_called()
        assert not fs.exists("destination/path")
        minio_client.fget_object.assert_not_called()

    def test_download__dont_get_objects_with_delete_marker(self, mocker: MockerFixture, fs: FakeFilesystem) -> None:
        """It should skip downloading objects that have a delete marker."""
        # Arrange
        version = make_version()
        minio_client = mocker.Mock(spec=minio.Minio)
        logger = mocker.Mock(spec=ILogger)
        rewinder = Rewinder(minio_client, logger, version)
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
        rewinder.download("my-bucket", "source/path", "destination/path")

        # Assert
        assert fs.exists("destination/path")  # Destination directory has been created.
        assert minio_client.fget_object.call_args_list == [
            mocker.call(
                "my-bucket",
                f"source/path/{name}",
                f"destination/path/{name}",
                version_id="not-deleted",
            )
            for name in ("foo", "baz", "quux")
        ]

    def test_download__object_already_exists_and_content_is_the_same__skip_download(
        self, mocker: MockerFixture, fs: FakeFilesystem, capsys: CaptureFixture
    ) -> None:
        """It should skip downloading objects that already exist in the destination directory."""
        # Arrange
        version = make_version()
        minio_client = mocker.Mock(spec=minio.Minio)
        logger = mocker.Mock(spec=ILogger)
        rewinder = Rewinder(minio_client, logger, version)
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

        # Act
        rewinder.download("my-bucket", "source/path", "destination/path")

        # Assert
        assert fs.exists("destination/path")  # Destination directory has been created.
        assert mocker.call('Skipping download: destination/path/empty-file, it already exists.') in logger.warning.call_args_list
        assert mocker.call('Skipping download: destination/path/bar, it already exists.') in logger.warning.call_args_list
        assert mocker.call('Skipping download: destination/path/qux, it already exists.') in logger.warning.call_args_list
        assert minio_client.fget_object.call_args_list == [
            mocker.call(
                "my-bucket",
                f"source/path/{name}",
                f"destination/path/{name}",
                version_id=f"v{i}",
            )
            for i, name in ((0, "foo"), (2, "baz"), (4, "quux"))
        ]

    def test_download__object_already_exists_and_content_is_different__download_object(
        self, mocker: MockerFixture, fs: FakeFilesystem, capsys: CaptureFixture
    ) -> None:
        """It should downloading objects that already exist in the destination directory but have chagned."""
        # Arrange
        minio_client = mocker.Mock(spec=minio.Minio)
        logger = mocker.Mock(spec=ILogger)
        rewinder = Rewinder(minio_client, logger, make_version())
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
        rewinder.download("my-bucket", "source/path", "destination/path")
        captured = capsys.readouterr()

        # Assert
        assert fs.exists("destination/path")  # Destination directory has been created.
        assert "Skipping download: destination/path/foo" not in captured.out
        minio_client.fget_object.assert_called_once_with(
            "my-bucket",
            "source/path/foo",
            "destination/path/foo",
            version_id="v1",
        )

    def test_download__large_object_already_exists_and_content_is_the_same__skip_download(
        self, mocker: MockerFixture, fs: FakeFilesystem, capsys: CaptureFixture
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

        minio_client = mocker.Mock(spec=minio.Minio)
        logger = mocker.Mock(spec=ILogger)
        rewinder = Rewinder(minio_client, logger, make_version(), part_size=part_size)
        minio_client.list_objects.return_value = iter(
            [make_object("source/path/foo", bucket_name="my-bucket", version_id="v1", etag=etag)]
        )
        fs.create_file("destination/path/foo", contents=content)

        # Act
        rewinder.download("my-bucket", "source/path", "destination/path")
        captured = capsys.readouterr()

        # Assert
        assert fs.exists("destination/path")  # Destination directory has been created.
        assert mocker.call('Skipping download: destination/path/foo, it already exists.') in logger.warning.call_args_list
        minio_client.fget_object.assert_not_called()
