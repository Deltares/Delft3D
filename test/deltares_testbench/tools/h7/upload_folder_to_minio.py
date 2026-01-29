import sys
from pathlib import Path

from minio import Minio
from minio.error import S3Error

from src.utils.handlers.credential_handler import CredentialHandler

if __name__ == "__main__":
    # Check command-line arguments
    if len(sys.argv) < 2:
        print("Usage: upload_folder.py <bucket_name>")
        sys.exit(1)

    # MinIO server details
    minio_url = "s3.deltares.nl"
    bucket_name = sys.argv[1]
    folder_path = Path("./upload")

    # Initialize MinIO client
    client = Minio(
        minio_url,
        credentials=CredentialHandler().get_credentials(),
        secure=True,  # Set to False if not using HTTPS
    )

    def upload_folder(client: Minio, bucket_name: str, folder_path: Path) -> None:
        """Upload files in folder path."""
        for path in folder_path.rglob("*"):
            if path.is_dir():
                continue
            object_name = "/".join(path.parts[1:])
            try:
                client.fput_object(bucket_name, object_name, path)
                print(f"Uploaded {path} to {bucket_name}/{object_name}")
            except S3Error as e:
                print(f"Failed to upload {path}: {e}")

    # Upload the folder
    upload_folder(client, bucket_name, folder_path)
