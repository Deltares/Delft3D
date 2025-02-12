import argparse
import os
import subprocess
import sys

from minio import Minio
from minio.error import S3Error

# Parse script arguments
parser = argparse.ArgumentParser(description="Download files from MinIO.")
parser.add_argument("--access_key", required=True, help="S3 access key")
parser.add_argument("--secret_key", required=True, help="S3 secret key")
parser.add_argument("--engine_dir", required=True, help="Engine directory")
args = parser.parse_args()

# Install Minio Client (mc)
print("Installing Minio Client (mc)")
subprocess.run(["pip", "install", "minio"], check=True)

# Set up Minio client
client = Minio("s3.deltares.nl", access_key=args.access_key, secret_key=args.secret_key, secure=True)

# Download files from MinIO
bucket_name = "dsc-testbench"
prefix = f"cases/{args.engine_dir}"
local_dir = f"./{args.engine_dir}"


def download_from_minio(bucket: str, prefix: str, local: str) -> None:
    """
    Download files from MinIO bucket to a local directory.

    Args:
        bucket (str): Name of the S3 bucket.
        prefix (str): Prefix of the files to download.
        local (str): Local directory to save the downloaded files.
    """
    objects = client.list_objects(bucket, prefix=prefix, recursive=True)
    for obj in objects:
        key = obj.object_name
        local_path = os.path.join(local, os.path.relpath(key, prefix))
        if not os.path.exists(os.path.dirname(local_path)):
            os.makedirs(os.path.dirname(local_path))
            print(f"Created directory: {os.path.dirname(local_path)}")
        client.fget_object(bucket, key, local_path)
        print(f"Downloaded file: {local_path}")


try:
    download_from_minio(bucket_name, prefix, local_dir)
except S3Error as e:
    print(f"Error occurred: {e}")
    sys.exit(1)
