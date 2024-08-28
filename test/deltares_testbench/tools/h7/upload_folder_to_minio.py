import sys
from pathlib import Path

from minio import Minio
from minio.error import S3Error

if __name__ == "__main__":
    # Check command-line arguments
    if len(sys.argv) < 4:
        print("Usage: upload_folder.py <access_key> <secret_key> <bucket_name>")
        sys.exit(1)

    # MinIO server details
    minio_url = "s3.deltares.nl"
    access_key = sys.argv[1]
    secret_key = sys.argv[2]
    bucket_name = sys.argv[3]
    folder_path = Path("./upload")

    # Initialize MinIO client
    client = Minio(
        minio_url,
        access_key=access_key,
        secret_key=secret_key,
        secure=True,  # Set to False if not using HTTPS
    )

    # Function to upload files
    def upload_folder(client: Minio, bucket_name: str, folder_path: Path):
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
