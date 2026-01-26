"""Executes MinIO commands.

Copyright (C)  Stichting Deltares, 2026
"""

import os
import re
from datetime import datetime, timezone
from pathlib import Path
from typing import Optional

import certifi
import urllib3
from minio import Minio

from src.config.credentials import Credentials
from src.utils.handlers.i_handler import IHandler
from src.utils.logging.i_logger import ILogger
from src.utils.minio_rewinder import Rewinder


class MinIOHandler(IHandler):
    """MinIO wrapper, has handler interface."""

    def download(
        self, from_path: str, to_path: str, credentials: Credentials, version: Optional[str], logger: ILogger
    ) -> None:
        """Set up a Minio client connection.

        You can specify the download source and destination.

        Parameters
        ----------
        from_path : str
            Minio URL.
        to_path : str
            Dowload location.
        credentials : Credentials
            Minio credentials.
        version : str
            Timestamp string e.g. "2023.10.20T12:00".
        logger : ILogger
            The logger that logs to a file.
        """
        match = re.match(r"^https://(?P<hostname>[^/]*)/(?P<bucket>[^/]*)/(?P<path>.*)$", from_path)
        if match is None:
            raise ValueError("Invalid `from_path` value. Must match pattern `https://{hostname}/{bucket-name}/{path}`")
        s3_storage = match.group("hostname")
        s3_bucket = match.group("bucket")
        s3_path = match.group("path")

        # Minio client connection
        my_client = Minio(
            s3_storage,
            access_key=credentials.username,
            secret_key=credentials.password,
            secure=True,
            http_client=urllib3.PoolManager(
                timeout=urllib3.Timeout.DEFAULT_TIMEOUT,
                cert_reqs="CERT_REQUIRED",
                ca_certs=os.environ.get("SSL_CERT_FILE") or certifi.where(),
                retries=urllib3.Retry(
                    total=5,
                    backoff_factor=0.2,
                    status_forcelist=[500, 502, 503, 504],  # Retry on temporary server errors
                ),
            ),
        )
        rewinder = Rewinder(my_client, logger)

        # Download the objects
        minio_path = s3_path.replace("/./", "/")
        logger.debug(f"downloading from minIO: {minio_path}")
        rewinder.download(s3_bucket, minio_path, Path(to_path), self.__parse_timestamp(version))

    @staticmethod
    def __parse_timestamp(version: Optional[str]) -> datetime:
        if version is None:
            return datetime.now(timezone.utc)
        timestamp = datetime.fromisoformat(version)
        if timestamp.tzinfo is None:
            timestamp = timestamp.replace(tzinfo=timezone.utc)
        return timestamp
