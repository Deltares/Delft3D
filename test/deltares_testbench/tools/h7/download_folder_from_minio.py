import argparse
import os

from minio import Minio
from minio.credentials.providers import StaticProvider
from minio.error import S3Error

from src.utils.handlers.credential_handler import CredentialHandler

if __name__ == "__main__":
    # Create the parser
    parser = argparse.ArgumentParser(description="Process some paths.")

    # Add arguments
    parser.add_argument("--bucket", type=str, required=True, help="The name of the bucket")
    parser.add_argument("--minio-path", type=str, required=True, help="The path to the remote folder")
    parser.add_argument("--dest", type=str, required=True, help="The path to the local folder")
    parser.add_argument("-u", dest="username", type=str, help="Your MinIO access key")
    parser.add_argument("-p", dest="password", type=str, help="Your MinIO secret key")

    # Parse the arguments
    args = parser.parse_args()

    # Access the arguments
    bucket_name = args.bucket
    remote_folder_path = args.minio_path
    local_folder_path = args.dest

    username = args.username
    password = args.password

    # Set up provider use credential file if exists.
    if CredentialHandler().credential_file_exists():
        provider = CredentialHandler().get_credentials()
    elif username and password:
        provider = StaticProvider(username, password)  # type: ignore
    else:
        exit(1)
    print(f"Downloading objects from {bucket_name}/{remote_folder_path} to {local_folder_path}")

    # MinIO server details
    minio_url = "s3.deltares.nl"

    # Initialize MinIO client
    client = Minio(
        endpoint=minio_url,
        credentials=provider,
        secure=True,  # Set to False if not using HTTPS
    )

    def download_folder(client: Minio, bucket_name: str, remote_folder_path: str, local_folder_path: str) -> None:
        """Download a folder from MinIO."""
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
