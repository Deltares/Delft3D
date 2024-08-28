import os
import sys

from minio import Minio
from minio.error import S3Error

if __name__ == "__main__":
    # Check command-line arguments
    if len(sys.argv) < 4:
        print("Usage: download_folder.py <access_key> <secret_key> <bucket_name>")
        sys.exit(1)

    # MinIO server details
    minio_url = "s3.deltares.nl"
    access_key = sys.argv[1]
    secret_key = sys.argv[2]
    bucket_name = sys.argv[3]
    remote_folder_path = "TeamcityMinioStorage/h7_results"
    local_folder_path = "data/cases"

    # Initialize MinIO client
    client = Minio(
        minio_url,
        access_key=access_key,
        secret_key=secret_key,
        secure=True,  # Set to False if not using HTTPS
    )

    # Function to download files
    def download_folder(client: Minio, bucket_name: str, remote_folder_path: str, local_folder_path: str):
        objects = client.list_objects(bucket_name, prefix=remote_folder_path, recursive=True)
        for obj in objects:
            remote_file_path = obj.object_name
            local_file_path = os.path.join(local_folder_path, os.path.relpath(remote_file_path, remote_folder_path))

            # Create local directories if they don't exist
            os.makedirs(os.path.dirname(local_file_path), exist_ok=True)

            try:
                client.fget_object(bucket_name, remote_file_path, local_file_path)
                print(f"Downloaded {remote_file_path} to {local_file_path}")
            except S3Error as e:
                print(f"Failed to download {remote_file_path}: {e}")

    # Ensure the local folder exists
    os.makedirs(local_folder_path, exist_ok=True)

    # Download the folder
    download_folder(client, bucket_name, remote_folder_path, local_folder_path)
