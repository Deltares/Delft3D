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

# Set up Minio client with connection pooling
client = Minio("s3.deltares.nl", access_key=args.access_key, secret_key=args.secret_key, secure=True)

# Download files from MinIO
bucket_name = "dsc-testbench"
prefix = f"cases/{args.engine_dir}"
local_dir = f"./{args.engine_dir}"

def download_file(client, bucket, key, local_path, last_modified, version_id=None):
    os.makedirs(os.path.dirname(local_path), exist_ok=True)
    if not os.path.exists(local_path):
        client.fget_object(bucket, key, local_path, version_id=version_id)
        print(f"Downloaded file: {local_path} (Last Modified: {last_modified})")
    else:
        print(f"File already exists: {local_path} (Skipping download)")

def download_batch(client, bucket, objects, local, prefix):
    for obj in objects:
        key = obj.object_name
        local_path = os.path.join(local, os.path.relpath(key, prefix))
        try:
            download_file(client, bucket, key, local_path, obj.last_modified, obj.version_id)
        except S3Error as e:
            print(f"Error occurred: {e}")

def download_from_minio(bucket: str, prefix: str, local: str, iso_time: str) -> None:
    objects = list(client.list_objects(bucket, prefix=prefix, recursive=True, include_version=True))
    filter_time = datetime.fromisoformat(iso_time)
    
    # Filter objects based on the last modified time
    filtered_objects = [obj for obj in objects if obj.is_latest and obj.last_modified <= filter_time]
    
    # Batch processing
    batch_size = 10
    for i in range(0, len(filtered_objects), batch_size):
        batch = filtered_objects[i:i + batch_size]
        download_batch(client, bucket, batch, local, prefix)

try:
    download_from_minio(bucket_name, prefix, local_dir, args.iso_time)
except S3Error as e:
    print(f"Error occurred: {e}")
    sys.exit(1)