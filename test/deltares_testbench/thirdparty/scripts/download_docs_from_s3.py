import argparse
import os
import sys
from datetime import datetime
from minio import Minio
from minio.error import S3Error

# Parse script arguments
parser = argparse.ArgumentParser(description="Download files from MinIO.")
parser.add_argument("--access_key", required=True, help="S3 access key")
parser.add_argument("--secret_key", required=True, help="S3 secret key")
parser.add_argument("--engine_dir", required=True, help="Engine directory")
parser.add_argument("--iso_time", required=True, help="ISO8601 time to filter files")
args = parser.parse_args()


def download_file(minio_client, bucket, key, local_path, version_id=None):
    dir_path = os.path.dirname(local_path)
    if not os.path.exists(dir_path):
        os.makedirs(dir_path, exist_ok=True)
        print(f"Download directory: {dir_path}")
    if not os.path.exists(local_path):
        minio_client.fget_object(bucket, key, local_path, version_id=version_id)

def download(minio_client, bucket, objects, local, prefix):
    for obj in objects:
        key = obj.object_name
        local_path = os.path.join(local, os.path.relpath(key, prefix))
        try:
            download_file(minio_client, bucket, key, local_path, obj.version_id)
        except S3Error as e:
            print(f"Error occurred: {e}")

def download_from_minio(minio_client, bucket: str, prefix: str, local: str, iso_time: str) -> None:
    objects = list(client.list_objects(bucket, prefix=prefix, recursive=True, include_version=True))
    filter_time = datetime.fromisoformat(iso_time)
    
    # Further filter objects based on key content
    filtered_objects = [obj for obj in objects if "doc" in obj.object_name]
    
    # Create a dictionary to store the latest version of each object before or on the filter_time
    latest_objects = {}
    for obj in filtered_objects:
        key = obj.object_name
        if obj.last_modified is not None and obj.last_modified <= filter_time:
            if key not in latest_objects or obj.last_modified > latest_objects[key].last_modified:
                latest_objects[key] = obj
    
    # Filter out objects where the latest version has a delete marker
    final_objects = [obj for obj in latest_objects.values() if not obj.is_delete_marker]

    # Download the filtered objects
    download(minio_client, bucket, final_objects, local, prefix)

try:
    # Set up Minio client with connection pooling
    client = Minio("s3.deltares.nl", access_key=args.access_key, secret_key=args.secret_key, secure=True)

    # Download files from MinIO
    bucket_name = "dsc-testbench"
    prefix = f"cases/{args.engine_dir}"
    local_dir = f"./{args.engine_dir}"

    download_from_minio(client, bucket_name, prefix, local_dir, args.iso_time)
except S3Error as e:
    print(f"Error occurred: {e}")
    sys.exit(1)
