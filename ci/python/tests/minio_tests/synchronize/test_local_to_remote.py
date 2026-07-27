import random
from pathlib import Path

from minio import Minio
from pyfakefs.fake_filesystem import FakeFilesystem
from pytest_mock import MockerFixture
from s3_path_wrangler.paths import S3Path

from ci_tools.minio.synchronize.sync_plan import Mode
from tests.helpers import minio as helper


class TestLocalToRemote:
    def test_synchronize__upload_empty_file(self, mocker: MockerFixture, fs: FakeFilesystem) -> None:
        # Arrange
        local_dir = Path("my_files")
        for name in ("foo", "bar", "baz"):
            fs.create_file(local_dir / f"{name}.txt", contents=random.randbytes(42))

        list_items = [helper.make_list_item(f"{name}.txt", size=42) for name in ("bar", "baz", "foo")]
        minio_client = mocker.Mock(spec=Minio)
        changes = helper.make_changes(creations=list_items)
        local_to_remote = helper.make_local_to_remote(
            minio_client=minio_client,
            source_directory=local_dir,
            destination_prefix=S3Path("s3://my-bucket/my_objects"),
            plan=helper.make_sync_plan(changes=changes),
        )

        # Act
        local_to_remote.synchronize()

        # Assert
        put_objects = sorted(
            minio_client._put_object.call_args_list,
            key=lambda call: call.kwargs["object_name"],
        )
        assert len(put_objects) == 3
        assert [call.kwargs["object_name"] for call in put_objects] == [
            f"my_objects/{name}.txt" for name in ("bar", "baz", "foo")
        ]
        assert all(len(call.kwargs["data"]) == 42 for call in put_objects)

    def test_synchronize__upload_small_files(self, mocker: MockerFixture, fs: FakeFilesystem) -> None:
        # Arrange
        local_dir = Path("my_files")
        fs.create_file(local_dir / "empty", contents=random.randbytes(0))

        list_item = helper.make_list_item("empty", size=0)
        minio_client = mocker.Mock(spec=Minio)
        changes = helper.make_changes(creations=[list_item])
        local_to_remote = helper.make_local_to_remote(
            minio_client=minio_client,
            source_directory=local_dir,
            destination_prefix=S3Path("s3://my-bucket/my_objects"),
            plan=helper.make_sync_plan(changes=changes),
        )

        # Act
        local_to_remote.synchronize()

        # Assert
        minio_client._put_object.assert_called_once_with(
            bucket_name="my-bucket",
            object_name="my_objects/empty",
            data=b"",
            headers={"Content-Type": "application/octet-stream"},
        )

    def test_synchronize__multipart_upload_one_big_file(self, mocker: MockerFixture, fs: FakeFilesystem) -> None:
        # Arrange
        file_size = 42
        part_size = 10
        local_dir = Path("my_files")
        fs.create_file(local_dir / "big_file", contents=random.randbytes(file_size))

        list_item = helper.make_list_item("big_file", size=42)
        minio_client = mocker.Mock(spec=Minio)
        changes = helper.make_changes(creations=[list_item])
        local_to_remote = helper.make_local_to_remote(
            minio_client=minio_client,
            source_directory=local_dir,
            destination_prefix=S3Path("s3://my-bucket/my_objects"),
            plan=helper.make_sync_plan(changes=changes, part_size=part_size),
        )

        # Act
        local_to_remote.synchronize()

        # Assert
        minio_client._create_multipart_upload.assert_called_once_with(
            bucket_name="my-bucket",
            object_name="my_objects/big_file",
            headers={"Content-Type": "application/octet-stream"},
        )
        upload_parts = sorted(
            minio_client._upload_part.call_args_list,
            key=lambda call: call.kwargs["part_number"],
        )
        part_sizes = [len(call.kwargs["data"]) for call in upload_parts]
        assert part_sizes == (file_size // part_size) * [part_size] + [file_size % part_size]
        complete_call, *other_calls = minio_client._complete_multipart_upload.call_args_list
        assert not other_calls
        assert complete_call.kwargs["bucket_name"] == "my-bucket"
        assert complete_call.kwargs["object_name"] == "my_objects/big_file"

    def test_synchronize__delete_objects(self, mocker: MockerFixture) -> None:
        # Arrange
        list_items = [helper.make_list_item(f"{name}.txt", size=42) for name in ("bar", "baz", "foo")]
        minio_client = mocker.Mock(spec=Minio)
        changes = helper.make_changes(deletions=list_items)
        local_to_remote = helper.make_local_to_remote(
            minio_client=minio_client,
            destination_prefix=S3Path("s3://my-bucket/my_objects"),
            plan=helper.make_sync_plan(mode=Mode.DELETE, changes=changes),
        )

        # Act
        local_to_remote.synchronize()

        # Assert
        remove_objects = sorted(
            minio_client.remove_object.call_args_list,
            key=lambda call: call.kwargs["object_name"],
        )
        assert len(remove_objects) == 3
        assert [call.kwargs["object_name"] for call in remove_objects] == [
            f"my_objects/{name}.txt" for name in ("bar", "baz", "foo")
        ]
