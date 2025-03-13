import argparse
import logging
import sys
from datetime import datetime
from pathlib import Path

from minio import Minio
from minio.credentials import AWSConfigProvider
from minio.datatypes import Object as MinioObject
from minio.error import S3Error


def download_file(minio_client: Minio, bucket: str, local_path: str, minio_object: MinioObject) -> None:
    """Download a file from an S3 bucket using the MinIO client."""
    local_path = Path(local_path)
    if not local_path.parent.exists():
        local_path.parent.mkdir(parents=True, exist_ok=True)
    if not local_path.exists():
        minio_client.fget_object(bucket, minio_object.object_name, local_path, version_id=minio_object.version_id)
        print(f"Download file: {local_path}, last modified at: {minio_object.last_modified}")


def download_objects(minio_client: Minio, bucket: str, objects: list[MinioObject], local_dir: Path, prefix: str) -> None:
    """Download a list of objects from an S3 bucket using the MinIO client."""
    for obj in objects:
        local_path = Path(local_dir) / Path(obj.object_name).relative_to(prefix)
        try:
            download_file(minio_client, bucket, local_path, obj)
        except S3Error:
            logging.exception(f"Error occurred while downloading {obj.object_name}")
            return


def get_doc_file_list_from_minio(
    minio_client: Minio, bucket: str, prefix: str, filter_time: datetime
) -> list[MinioObject]:
    """Download files from MinIO based on a prefix and ISO8601 time."""
    objects = list(minio_client.list_objects(bucket, prefix=prefix, recursive=True, include_version=True))

    # Further filter objects based on key content
    filtered_objects = [obj for obj in objects if "/doc/" in obj.object_name]

    # Create a dictionary to store the latest version of each object before or on the filter_time
    latest_objects = {}
    for obj in filtered_objects:
        key = obj.object_name
        if obj.last_modified is not None and obj.last_modified <= filter_time:
            if key not in latest_objects or obj.last_modified > latest_objects[key].last_modified:
                latest_objects[key] = obj

    # Filter out objects where the latest version has a delete marker
    return [obj for obj in latest_objects.values() if not obj.is_delete_marker]


if __name__ == "__main__":
    # Parse script arguments
    parser = argparse.ArgumentParser(description="Download files from MinIO.")
    parser.add_argument("--engine_dir", required=True, help="Engine directory")
    parser.add_argument("--iso_time", required=True, help="ISO8601 time to filter files")
    args = parser.parse_args()

    try:
        # Set up Minio client with connection pooling
        minio_client = Minio("s3.deltares.nl", credentials=AWSConfigProvider())

        # Download files from MinIO
        bucket_name = "dsc-testbench"
        prefix = f"cases/{args.engine_dir}"
        local_dir = Path(args.engine_dir)
        filter_time = datetime.fromisoformat(args.iso_time)

        objects = get_doc_file_list_from_minio(minio_client, bucket_name, prefix, filter_time)
        download_objects(minio_client, bucket_name, objects, local_dir, prefix)

    except S3Error as e:
        print(f"Error occurred: {e}")
        sys.exit(1)
