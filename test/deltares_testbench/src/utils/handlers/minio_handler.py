"""
Description: Executes MinIO commands
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2024
"""
import os.path
import re
from minio import Minio
from src.config.credentials import Credentials
from src.utils.handlers.i_handler import IHandler
from src.utils.logging.i_logger import ILogger
from src.utils.minio_rewinder import Rewinder


class MinIOHandler(IHandler):
    """ MinIO wrapper, has handler interface """
    def prepare_upload(
        self,
        from_path: str,
        to_path: str,
        logger: ILogger
    ) -> None:
        logger.debug("Preparing upload to MinIO not implemented yet")

    def upload(
        self,
        from_path: str,
        to_path: str,
        credentials: Credentials,
        logger: ILogger
    ) -> None:
        logger.debug("Uploading to MinIO not implemented yet")

    def download(
        self,
        from_path: str,
        to_path: str,
        credentials: Credentials,
        version: str,
        logger: ILogger
    ):
        """ Sets up a Minio client connection. You can specify the download
        source and destination

        Args:
            from_path (str): minio URL
            to_path (str): dowload location
            credentials (Credentials): minio credentials
            version (str): timestamp string e.g. "2023.10.20T12:00"
            logger (ILogger): The logger that logs to a file
        """

        match = re.match(r'^https://(?P<hostname>[^/]*)/(?P<bucket>[^/]*)/(?P<path>.*)$', from_path)
        if match is None:
            raise ValueError("Invalid `from_path` value. Must match pattern `https://{hostname}/{bucket-name}/{path}`")
        s3_storage = match.group('hostname')
        s3_bucket = match.group('bucket')
        s3_path = match.group('path')

        # Minio client connection
        my_client = Minio(
            s3_storage,
            access_key=credentials.username,
            secret_key=credentials.password)

        # Prepare the rewind-settings
        rewinder = Rewinder(my_client, version)

        # Download the objects
        minio_path = s3_path.replace('/./', '/')
        destination_path = os.path.normpath(to_path)
        logger.debug(f"downloading from minIO: {minio_path}")
        rewinder.download(s3_bucket, minio_path, destination_path)
