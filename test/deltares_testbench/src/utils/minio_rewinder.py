import os
import re
from collections import defaultdict
from minio import Minio
from pytz import timezone
from datetime import datetime, timezone
from typing import List, Dict


class Rewinder:
    """Implements the rewind feature of the Minio server."""

    def __init__(self, client: Minio, timestamp: str):
        self.client = client
        self.rewind = self.__parse_timestamp(timestamp)
        

    def download(self, bucket: str, source_path: str, destination_directory: str):
        """Downloads a directory from MinIO.

        Args:
            bucket (str): the minio bucket
            source_path (str): the path of the folder you want to download
            destination_directory (str): the destination directory
        """
        # Get list of all object-versions from Minio
        object_list = self.client.list_objects(
            bucket,
            source_path,
            recursive=True,
            include_version=True,
        )

        object_list_before_rewind = filter(
            lambda o: o._last_modified < self.rewind.replace(tzinfo=timezone.utc), object_list
        )
        obj_list = list(object_list_before_rewind)
        downloads = self.__get_latest_non_deleted_versions(obj_list)

        # Check if downloads is empty
        if not downloads:
            print(f"No dowloads found in bucket {bucket} at {source_path}")
            return

        self.__ensure_destination_directory(destination_directory)
        self.__download_objects(bucket, downloads, source_path, destination_directory)

    def __parse_timestamp(self, timestamp: str):
        isofmt_timestamp = re.sub(r'[./]', '-', timestamp)
        # Check if the timestamp includes seconds or not
        if len(isofmt_timestamp) == 16:
            format_str = "%Y-%m-%dT%H:%M"
        elif len(isofmt_timestamp) == 19:
            format_str = "%Y-%m-%dT%H:%M:%S"
        else:
            raise ValueError("Invalid timestamp format")

        dt = datetime.strptime(isofmt_timestamp, format_str)
        if dt.tzinfo is None:
            # Ensure naive timestamps are treated like UTC timestamps
            dt = dt.replace(tzinfo=timezone.utc)

        return dt

    def __get_latest_non_deleted_versions(self, object_list):
        object_versions = defaultdict(list)

        for object in object_list:
            object_versions[object._object_name].append(object)

        downloads = []

        for object_name, versions in object_versions.items():
            versions.sort(key=lambda version: version._last_modified, reverse=True)
            if not versions[0]._is_delete_marker:
                downloads.append(
                    {"object_name": object_name, "version_id": versions[0]._version_id}
                )

        return downloads

    def __ensure_destination_directory(self, destination_directory: str):
        # Check if the destination directory exists and is not empty
        if os.path.exists(destination_directory):
            if len(os.listdir(destination_directory)) > 0:
                print(f"Destination directory '{destination_directory}' is not empty.")
        else:
            # Directory doesn't exist, so create it
            os.makedirs(destination_directory, exist_ok=True)

    def __object_exists(self, bucket: str, object_name: str):
        try:
            self.client.stat_object(bucket, object_name)
            return True
        except Exception as e:
            print(f"Object {object_name} not in {bucket}: {e}.")
            return False

    def __download_objects(
        self,
        bucket: str,
        downloads: List[Dict[str, str]],
        source_path: str,
        destination_directory: str,
    ):
        for download in downloads:
            object_path = download["object_name"]
            if not self.__object_exists(bucket, object_path):
                continue

            filename_and_sub__dir_with_extension = object_path.replace(
                source_path + "/", ""
            )

            destination_file_path = os.path.join(
                destination_directory, filename_and_sub__dir_with_extension
            )

            self.__download_object(
                bucket,
                download,
                destination_file_path,
                object_path,
            )

    def __download_object(
        self, bucket: str, download: dict, destination_file_path: str, object_path: str
    ):
        if os.path.exists(destination_file_path):
            print(f"Skipping download: {destination_file_path}, it already exists.")
            return

        try:
            print(f"Downloading: {destination_file_path}")
            self.client.fget_object(
                bucket,
                object_path,
                destination_file_path,
                version_id=download["version_id"],
            )
        except Exception as exception:
            print(f"Exception: {exception}.")
