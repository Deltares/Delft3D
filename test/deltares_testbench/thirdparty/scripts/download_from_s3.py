import argparse
import os
import subprocess
import sys
from datetime import datetime
from threading import Thread
from queue import Queue

from minio import Minio
from minio.error import S3Error

# Parse script arguments
parser = argparse.ArgumentParser(description="Download files from MinIO.")
parser.add_argument("--access_key", required=True, help="S3 access key")
parser.add_argument("--secret_key", required=True, help="S3 secret key")
parser.add_argument("--engine_dir", required=True, help="Engine directory")
parser.add_argument("--iso_time", required=True, help="ISO8601 time to filter files")
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

def download_file(client, bucket, key, local_path, version_id=None):
    if not os.path.exists(os.path.dirname(local_path)):
        os.makedirs(os.path.dirname(local_path))
        print(f"Created directory: {os.path.dirname(local_path)}")
    client.fget_object(bucket, key, local_path, version_id=version_id)
    print(f"Downloaded file: {local_path}")

def worker(queue):
    while not queue.empty():
        client, bucket, key, local_path, version_id = queue.get()
        try:
            download_file(client, bucket, key, local_path, version_id)
        except S3Error as e:
            print(f"Error occurred: {e}")
        queue.task_done()

def download_from_minio(bucket: str, prefix: str, local: str, iso_time: str, num_threads: int = 4) -> None:
    objects = client.list_objects(bucket, prefix=prefix, recursive=True, include_version=True)
    filter_time = datetime.fromisoformat(iso_time)
    queue = Queue()
    
    for obj in objects:
        if obj.is_latest and obj.last_modified <= filter_time:
            key = obj.object_name
            local_path = os.path.join(local, os.path.relpath(key, prefix))
            queue.put((client, bucket, key, local_path, obj.version_id))
    
    threads = []
    for _ in range(num_threads):
        thread = Thread(target=worker, args=(queue,))
        thread.start()
        threads.append(thread)
    
    queue.join()
    for thread in threads:
        thread.join()

try:
    download_from_minio(bucket_name, prefix, local_dir, args.iso_time)
except S3Error as e:
    print(f"Error occurred: {e}")
    sys.exit(1)