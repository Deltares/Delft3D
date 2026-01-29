import hashlib
import random
from datetime import datetime, timedelta, timezone
from pathlib import Path

import pytest
from minio import Minio
from minio.datatypes import Object as MinioObject
from pyfakefs.fake_filesystem import FakeFilesystem
from pytest_mock import MockerFixture
from s3_path_wrangler.paths import S3Path

from ci_tools.minio import etag
from ci_tools.minio.list_item import DirectoryListing, ListItem, MinioPrefixListing
from tests.helpers import minio as helper


class TestListItem:
    def test_from_local_path__path_not_a_file__raise_file_not_found(self, fs: FakeFilesystem) -> None:
        test_dir = Path("test")
        fs.create_dir(test_dir)

        with pytest.raises(FileNotFoundError):
            ListItem.from_local_path(test_dir, test_dir.parent)

    def test_from_local_path__ancestor_not_a_directory__raise_not_a_directory(self, fs: FakeFilesystem) -> None:
        test_file = Path("foo/bar/baz.txt")
        test_ancestor = Path("foo/qux")
        fs.create_file(test_file)
        fs.create_file(test_ancestor)

        with pytest.raises(NotADirectoryError):
            ListItem.from_local_path(test_file, test_ancestor)

    def test_from_local_path__ancestor_not_an_ancestor__raise_value_errror(self, fs: FakeFilesystem) -> None:
        test_file = Path("foo/bar/baz.txt")
        test_ancestor = Path("foo/qux")
        fs.create_file(test_file)
        fs.create_dir(test_ancestor)

        with pytest.raises(ValueError, match="not in directory"):
            ListItem.from_local_path(test_file, test_ancestor)

    def test_from_local_path(self, fs: FakeFilesystem) -> None:
        # Arrange
        now = datetime.now(timezone.utc)
        test_file = Path("foo/bar/baz.txt")
        test_ancestor = test_file.parent.parent
        contents = "Hello world!"
        expected_etag = hashlib.md5(contents.encode()).hexdigest()

        fake_file = fs.create_file(test_file, contents=contents)
        fake_file.st_mtime = now.timestamp()

        # Act
        list_item = ListItem.from_local_path(test_file, test_ancestor)

        # Assert
        assert list_item.relative_path == "bar/baz.txt"
        assert list_item.size == len(contents)
        assert list_item.version is None
        assert list_item.timestamp == now
        assert list_item.etag == expected_etag

    @pytest.mark.parametrize(
        ("object_name", "size", "etag", "last_modified"),
        [
            pytest.param(None, 123, "badc0de", datetime.now(timezone.utc), id="no_object_name"),
            pytest.param("foo.txt", None, "badc0de", datetime.now(timezone.utc), id="no_size"),
            pytest.param("foo.txt", 123, None, datetime.now(timezone.utc), id="no_etag"),
            pytest.param("foo.txt", 123, "badc0de", None, id="no_last_modified"),
        ],
    )
    def test_from_minio_object__missing_required_attributes__raise_value_error(
        self,
        object_name: str | None,
        size: int | None,
        etag: str | None,
        last_modified: datetime | None,
    ) -> None:
        with pytest.raises(ValueError, match="missing required attributes"):
            ListItem.from_minio_object(
                minio_object=MinioObject(
                    bucket_name="my-bucket",
                    object_name=object_name,
                    size=size,
                    etag=etag,
                    last_modified=last_modified,
                ),
                remote_prefix=S3Path("s3://my-bucket/"),
            )

    def test_from_minio_object__is_delete_marker__raise_value_error(self) -> None:
        with pytest.raises(ValueError, match="delete marker"):
            ListItem.from_minio_object(
                minio_object=helper.make_object("foo.txt", is_delete_marker=True),
                remote_prefix=S3Path("s3://my-bucket"),
            )

    def test_from_minio_object__is_directory__raise_value_error(self) -> None:
        minio_obj = helper.make_object("foo/")
        with pytest.raises(ValueError, match="directory"):
            ListItem.from_minio_object(
                minio_object=minio_obj,
                remote_prefix=S3Path("s3://my-bucket"),
            )

    def test_from_minio_object(self) -> None:
        # Arrange
        now = datetime.now(timezone.utc)

        # Act
        list_item = ListItem.from_minio_object(
            minio_object=helper.make_object(
                "some/prefix/foo/bar.txt",
                bucket_name="my-bucket",
                size=42,
                etag="badc0de",
                version_id="decafbad",
                last_modified=now,
            ),
            remote_prefix=S3Path("s3://my-bucket/some/prefix"),
        )

        # Assert
        assert list_item.relative_path == "foo/bar.txt"
        assert list_item.size == 42
        assert list_item.version == "decafbad"
        assert list_item.timestamp == now
        assert list_item.etag == "badc0de"

    @pytest.mark.parametrize(
        ("size", "part_size", "expected"),
        [
            pytest.param(100, 10, 10, id="exact"),
            pytest.param(101, 10, 11, id="overshoot"),
            pytest.param(99, 10, 10, id="undershoot"),
            pytest.param(1, 9001, 1, id="large_part_size"),
            pytest.param(0, 42, 1, id="zero_size"),
        ],
    )
    def test_part_count(self, size: int, part_size: int, expected: int) -> None:
        # Arrange
        list_item = ListItem(
            relative_path="foo/bar.txt",
            size=size,
            timestamp=datetime.now(timezone.utc),
            version=None,
            _etag=etag.ConstEtag("badc0de"),
        )

        # Act
        part_count = list_item.part_count(part_size)

        # Assert
        assert part_count == expected


class TestDirectoryListing:
    def test_constructor__local_directory__raise(self) -> None:
        # Arrange
        with pytest.raises(NotADirectoryError):
            DirectoryListing(Path("ceci, ce n'est pas un directory"))

    def test_list_objects__sort_by_relative_path(self, fs: FakeFilesystem) -> None:
        # Arrange
        directory = Path("test")
        for file_name in ("foo", "bar", "baz"):
            fs.create_file(directory / file_name)
        listing = DirectoryListing(directory)

        # Act, Assert
        assert [obj.relative_path for obj in listing] == ["bar", "baz", "foo"]

    def test_list_objects__skip_directories(self, fs: FakeFilesystem) -> None:
        # Arrange
        directory = Path("test")
        fs.create_file(directory / "foo" / "bar")
        fs.create_dir(directory / "qux")
        fs.create_file(directory / "quux" / "corge")
        listing = DirectoryListing(directory)

        # Act
        list_items = list(listing)

        # Assert
        assert len(list_items) == 2
        bar, corge = list_items
        assert bar.relative_path == "foo/bar"
        assert corge.relative_path == "quux/corge"


class TestMinioPrefixListing:
    def test_constructor__non_utc_timestamp__raise(self, mocker: MockerFixture) -> None:
        with pytest.raises(ValueError, match="UTC"):
            MinioPrefixListing(
                client=mocker.Mock(spec=Minio),
                prefix=S3Path("s3://my-bucket/"),
                timestamp=datetime.now(timezone(timedelta(hours=1))),
            )

    def test_list_objects__no_timestamp_get_latest(self, mocker: MockerFixture) -> None:
        # Arrange
        now = datetime.now(timezone.utc)
        objects = [
            helper.make_object("test_object", version_id=f"-{i} days", last_modified=now - timedelta(days=i))
            for i in (1, 2, 3)
        ]
        random.shuffle(objects)
        mock_client = mocker.Mock(spec=Minio)
        mock_client.list_objects.return_value = iter(objects)
        listing = MinioPrefixListing(client=mock_client, prefix=S3Path("s3://my-bucket/"))

        # Act
        list_item, *others = list(listing)

        # Assert
        assert not others, "Only one list item should be returned"
        assert list_item.version == "-1 days"

    def test_list_objects__no_timestamp_get_latest_delete_marker(self, mocker: MockerFixture) -> None:
        # Arrange
        now = datetime.now(timezone.utc)
        objects = [
            helper.make_object(
                "test_object",
                version_id=f"-{i} days",
                last_modified=now - timedelta(days=i),
                is_delete_marker=(i == 1),  # The latest version is a delete marker!
            )
            for i in (1, 2, 3)
        ]
        random.shuffle(objects)
        mock_client = mocker.Mock(spec=Minio)
        mock_client.list_objects.return_value = iter(objects)
        listing = MinioPrefixListing(client=mock_client, prefix=S3Path("s3://my-bucket/"))

        # Act, Assert
        assert not any(listing), "The latest version of is a delete marker, no objects should be returned"

    def test_list_objects__sort_objects_by_key_and_return_the_latest(self, mocker: MockerFixture) -> None:
        # Arrange
        now = datetime.now(timezone.utc)
        objects = [
            helper.make_object(
                object_name, version_id=f"{object_name} -{i} days", last_modified=now - timedelta(days=i)
            )
            for object_name in ("foo", "bar", "baz")
            for i in (1, 2, 3)
        ]
        random.shuffle(objects)
        mock_client = mocker.Mock(spec=Minio)
        mock_client.list_objects.return_value = iter(objects)
        listing = MinioPrefixListing(client=mock_client, prefix=S3Path("s3://my-bucket/"))

        # Act
        result = list(iter(listing))
        relative_paths = [obj.relative_path for obj in result]
        versions = [obj.version for obj in result]

        # Assert
        assert len(result) == 3
        assert relative_paths == ["bar", "baz", "foo"]
        assert versions == ["bar -1 days", "baz -1 days", "foo -1 days"]

    def test_list_objects__rewind_too_far_back__no_objects(self, mocker: MockerFixture) -> None:
        # Arrange
        now = datetime.now(timezone.utc)
        objects = [helper.make_object(object_name) for object_name in ("foo", "bar", "baz")]
        random.shuffle(objects)
        mock_client = mocker.Mock(spec=Minio)
        mock_client.list_objects.return_value = iter(objects)
        listing = MinioPrefixListing(
            client=mock_client,
            prefix=S3Path("s3://my-bucket/"),
            timestamp=now - timedelta(days=10),
        )

        # Assert
        assert not any(listing), "No objects should be returned, all are too new"
