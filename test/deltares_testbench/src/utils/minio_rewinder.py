import hashlib
import io
import os
import re
from collections import defaultdict
from datetime import datetime, timezone
from typing import DefaultDict, Iterator, List

from minio import Minio
from minio.datatypes import Object as MinioObject
from src.utils.logging.i_logger import ILogger

# Large objects should be uploaded to MinIO with a *multipart upload*. This affects not only the
# performance of uploads, but also the computation of the `ETag` used to check data integrity.
# At this time of writing, it seems the MinIO recommendation for the size of the multipart upload
# parts is 100 MiB. This is the same size as AWS S3 recommends:
# https://min.io/docs/minio/linux/reference/minio-mc/mc-mv.html#mc.mv.-disable-multipart
# https://docs.aws.amazon.com/AmazonS3/latest/userguide/mpuoverview.html
DEFAULT_MULTIPART_UPLOAD_PART_SIZE = 100 * 1024 * 1024  # Bytes


class Rewinder:
    """Implements the rewind feature of the Minio server."""

    def __init__(self,
                 client: Minio,
                 logger: ILogger,
                 timestamp: str,
                 part_size: int = DEFAULT_MULTIPART_UPLOAD_PART_SIZE):
        self.client = client
        self.logger = logger
        self.rewind = self.__parse_timestamp(timestamp)
        self._multipart_upload_part_size = part_size

    def download(self, bucket: str, source_path: str, destination_directory: str) -> None:
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
            lambda o: o.last_modified < self.rewind.replace(tzinfo=timezone.utc), object_list
        )
        obj_list = list(object_list_before_rewind)
        downloads = self.__get_latest_non_deleted_versions(obj_list)

        # Check if downloads is empty
        if not downloads:
            self.logger.error(f"No downloads found in bucket {bucket} at {source_path}")
            return

        self.__ensure_destination_directory(destination_directory)
        self.__download_objects(bucket, downloads, source_path, destination_directory)

    def __parse_timestamp(self, timestamp: str) -> datetime:
        isofmt_timestamp = re.sub(r"[./]", "-", timestamp)
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

    def __get_latest_non_deleted_versions(self, object_list: List[MinioObject]) -> List[MinioObject]:
        object_versions: DefaultDict[str, List[MinioObject]] = defaultdict(list)

        for object in object_list:
            if object.object_name is not None:
                object_versions[object.object_name].append(object)

        downloads: List[MinioObject] = []
        min_dt = datetime.min.replace(tzinfo=timezone.utc)

        for _, versions in object_versions.items():
            versions.sort(key=lambda version: version.last_modified or min_dt, reverse=True)
            if not versions[0].is_delete_marker:
                downloads.append(versions[0])

        return downloads

    def __ensure_destination_directory(self, destination_directory: str):
        # Check if the destination directory exists and is not empty
        if os.path.exists(destination_directory):
            if len(os.listdir(destination_directory)) > 0:
                self.logger.warning(f"Destination directory '{destination_directory}' is not empty.")
        else:
            # Directory doesn't exist, so create it
            os.makedirs(destination_directory, exist_ok=True)

    def __download_objects(
        self,
        bucket: str,
        downloads: List[MinioObject],
        source_path: str,
        destination_directory: str,
    ) -> None:
        for download in downloads:
            object_path = download.object_name or ""
            filename_and_sub__dir_with_extension = object_path.replace(f"{source_path.rstrip('/')}/", "")

            destination_file_path = os.path.join(destination_directory, filename_and_sub__dir_with_extension)

            self.__download_object(
                bucket,
                download,
                destination_file_path,
                object_path,
            )

    def __download_object(
        self, bucket: str, object_info: MinioObject, destination_file_path: str, object_path: str
    ) -> None:
        if os.path.exists(destination_file_path) and object_info.etag == self.__etag(destination_file_path):
            self.logger.warning(f"Skipping download: {destination_file_path}, it already exists.")
            return

        try:
            self.logger.debug(f"Downloading: {destination_file_path}")
            self.client.fget_object(
                bucket,
                object_path,
                destination_file_path,
                version_id=object_info.version_id,
            )
        except Exception as exception:
            self.logger.error(f"Exception: {exception}.", exc_info=True)

    def __etag(self, path: str) -> str:
        """Compute the `ETag` of the contents of a file.

        Notes
        -----
        For "small" files, the ETag is the same as an MD5 `hexdigest`. Unfortunately, when files are large
        enough to be uploaded in multiple parts, the ETag computation changes:
        1. Compute the MD5 `digest` for each separate part
        2. Concatenate all of the MD5 digests for each uploaded part.
        3. Compute the hexdigest of the result, and add `-{n}`, where `n` is the number of parts.
        The value of the ETag depends on the size of the parts used during the multipart upload.
        See: https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html#large-object-checksums
        """

        def chunker(f: io.BufferedReader, size: int) -> Iterator[bytes]:
            chunk = f.read(size)
            while chunk:
                yield chunk
                chunk = f.read(size)

        with open(path, "rb") as f:
            hashes = [hashlib.md5(chunk) for chunk in chunker(f, self._multipart_upload_part_size)]

        if not hashes:
            return hashlib.md5(b"").hexdigest()  # Emtpy file
        elif len(hashes) == 1:
            return hashes[0].hexdigest()  # Regular file
        else:
            digests = b"".join(h.digest() for h in hashes)  # Files split in multiple parts
            return hashlib.md5(digests).hexdigest() + f"-{len(hashes)}"
