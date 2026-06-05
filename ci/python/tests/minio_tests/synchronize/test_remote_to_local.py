import random
from pathlib import Path

from minio import Minio
from pyfakefs.fake_filesystem import FakeFilesystem
from pytest_mock import MockerFixture
from s3_path_wrangler.paths import S3Path

from ci_tools.minio.synchronize.sync_plan import Mode
from tests.helpers import minio as helper


class TestRemoteToLocal:
    def test_synchronize__download_empty_file(self, mocker: MockerFixture, fs: FakeFilesystem) -> None:
        # Arrange
        remote_prefix = S3Path("s3://my-bucket/my_objects")
        local_dir = Path("my_files")
        fs.create_dir(local_dir)

        list_item = helper.make_list_item("empty", size=0)
        changes = helper.make_changes(creations=[list_item])
        minio_client = mocker.Mock(spec=Minio)
        minio_client.get_object.return_value.read.return_value = b""
        remote_to_local = helper.make_remote_to_local(
            minio_client,
            source_prefix=remote_prefix,
            destination_directory=local_dir,
            plan=helper.make_sync_plan(changes=changes),
        )

        # Act
        remote_to_local.synchronize()

        # Assert
        empty_file = local_dir / "empty"
        assert empty_file.is_file()
        assert empty_file.read_bytes() == b""

    def test_synchronize__download_small_files(self, mocker: MockerFixture, fs: FakeFilesystem) -> None:
        # Arrange
        remote_prefix = S3Path("s3://my-bucket/my_objects")
        local_dir = Path("my_files")
        fs.create_dir(local_dir)

        objects = [helper.make_list_item(f"{name}", size=42) for name in ("bar", "baz", "foo")]
        changes = helper.make_changes(creations=objects)
        minio_client = mocker.Mock(spec=Minio)
        minio_client.get_object.return_value.read.side_effect = [random.randbytes(42) for _ in range(3)]
        remote_to_local = helper.make_remote_to_local(
            minio_client,
            source_prefix=remote_prefix,
            destination_directory=local_dir,
            plan=helper.make_sync_plan(changes=changes),
        )

        # Act
        remote_to_local.synchronize()

        # Assert
        files = [local_dir / name for name in ("bar", "baz", "foo")]
        assert all(len(file.read_bytes()) == 42 for file in files)

    def test_synchronize__download_big_file_in_multiple_parts(self, mocker: MockerFixture, fs: FakeFilesystem) -> None:
        # Arrange
        file_size = 42
        part_size = 10
        remote_prefix = S3Path("s3://my-bucket/my_objects")
        local_dir = Path("my_files")
        fs.create_dir(local_dir)

        big_object = helper.make_list_item("big_object", size=file_size)
        changes = helper.make_changes(creations=[big_object])
        minio_client = mocker.Mock(spec=Minio)

        object_chunks = list(map(random.randbytes, (file_size // part_size) * [part_size] + [file_size % part_size]))
        minio_client.get_object.return_value.read.side_effect = object_chunks
        remote_to_local = helper.make_remote_to_local(
            minio_client,
            source_prefix=remote_prefix,
            destination_directory=local_dir,
            plan=helper.make_sync_plan(changes=changes, part_size=part_size),
        )

        # Act
        remote_to_local.synchronize()

        # Assert
        big_file_path = local_dir / "big_object"
        assert big_file_path.exists()
        assert big_file_path.read_bytes() == b"".join(object_chunks)

    def test_synchronize__delete_files(self, mocker: MockerFixture, fs: FakeFilesystem) -> None:
        # Arrange
        remote_prefix = S3Path("s3://my-bucket/my_objects")
        local_dir = Path("my_files")
        rel_paths = [
            "foo/bar/baz.bmp",
            "foo/qux.png",
            "quux.jpeg",
        ]
        for rel_path in rel_paths:
            fs.create_file(local_dir / rel_path)

        objects_to_delete = [helper.make_list_item(rel_path) for rel_path in rel_paths]
        changes = helper.make_changes(deletions=objects_to_delete)

        minio_client = mocker.Mock(spec=Minio)

        remote_to_local = helper.make_remote_to_local(
            minio_client,
            source_prefix=remote_prefix,
            destination_directory=local_dir,
            plan=helper.make_sync_plan(mode=Mode.DELETE, changes=changes),
        )

        # Act
        remote_to_local.synchronize()

        # Assert
        assert not any(local_dir.iterdir()), f"{local_dir} should be empty."
