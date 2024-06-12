"""
Description: Handler Factory
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2023
"""

import copy
import os
from abc import ABC
from typing import List, Optional

from src.config.credentials import Credentials
from src.config.types.handler_type import HandlerType
from src.suite.program import Program
from src.utils.common import delete_directory
from src.utils.handlers.ftp_handler import FTPHandler
from src.utils.handlers.http_handler import HTTPHandler
from src.utils.handlers.i_handler import IHandler
from src.utils.handlers.local_net_handler import LocalNetHandler
from src.utils.handlers.resolve_handler import ResolveHandler
from src.utils.handlers.svn_handler import SvnHandler
from src.utils.handlers.minio_handler import MinIOHandler
from src.utils.logging.i_logger import ILogger
from src.utils.paths import Paths
from src.utils.unzipper import Unzipper


class HandlerFactory(ABC):
    """Chooses which type of handler is used for upload and download actions"""

    @classmethod
    def __get_handler(
        cls,
        to_path: str,
        programs: List[Program],
        logger: ILogger,
        credentials: Optional[Credentials] = None,
        autocommit: bool = False,
    ) -> IHandler:
        """Creates handler based on destination path

        Args:
            to_path (str): destination path
            credentials (Credentials, optional): credentials needed for connection.
                                                 Defaults to None.
            autocommit (bool): use auto commit (for svn)

        Raises:
            AttributeError: if handler could not be detected

        Returns:
            IHandler: Specific handler
        """
        handler_type = ResolveHandler.detect(to_path, logger, credentials)
        handler: IHandler

        if handler_type == HandlerType.WEB:
            logger.debug(f"using HTTP handler for {to_path}")
            handler = HTTPHandler()
        if handler_type == HandlerType.SVN:
            logger.debug(f"using SVN handler for {to_path}")
            svn_program = copy.deepcopy(next(p for p in programs if p.name == "svn"))
            handler = SvnHandler(svn_program, autocommit)
        if handler_type == HandlerType.FTP:
            logger.debug(f"using FTP handler for {to_path}")
            handler = FTPHandler()
        if handler_type == HandlerType.NET or handler_type == HandlerType.PATH:
            logger.debug(f"using LocalNet handler for {to_path}")
            handler = LocalNetHandler()
        if handler_type == HandlerType.MINIO:
            logger.debug(f"using MinIO handler for {to_path}")
            handler = MinIOHandler()
        if handler_type == HandlerType.NONE:
            raise AttributeError("upload :: no type specified")

        return handler

    @classmethod
    def prepare_upload(
        cls,
        from_path: str,
        to_path: str,
        programs: List[Program],
        credentials: Optional[Credentials],
        logger: ILogger,
    ):
        rfp = Paths().rebuildToLocalPath(from_path)

        handler = cls.__get_handler(to_path, programs, logger, credentials)
        handler.prepare_upload(rfp, to_path, credentials, logger)

    @classmethod
    def upload(
        cls,
        from_path: str,
        to_path: str,
        programs: List[Program],
        logger: ILogger,
        credentials: Optional[Credentials] = None,
        autocommit: bool = False,
    ):
        """Upload data to location

        Args:
            from_path (str): source path
            to_path (str): target path
            credentials (Optional[Credentials], optional): Credentials to use.
            Defaults to None.
            autocommit (bool, optional): Automatically commit. Defaults to False.
        """
        rfp = Paths().rebuildToLocalPath(from_path)
        handler = cls.__get_handler(to_path, programs, logger, credentials, autocommit)
        handler.upload(rfp, to_path, credentials, logger)

    @classmethod
    def download(
        cls,
        from_path: str,
        to_path: str,
        programs: List[Program],
        logger: ILogger,
        credentials: Optional[Credentials] = None,
        version: Optional[str] = None,
        unzip: bool = False,
        autocommit: bool = False,
    ):
        """Download data from location

        Args:
            from_path (str): source path
            to_path (str): target path
            credentials (Optional[Credentials], optional): Credentials to use.
            Defaults to None.
            version (Optional[str], optional): version. Defaults to None.
            unzip (bool, optional): try to unzip file. Defaults to False.
            autocommit (bool, optional): Automatically commit. Defaults to False.

        Raises:
            e: _description_
        """
        rtp = Paths().rebuildToLocalPath(to_path)
        os.makedirs(rtp, exist_ok=True)

        handler = cls.__get_handler(
            from_path, programs, logger, credentials, autocommit
        )
        handler.download(from_path, rtp, credentials, version, logger)
        if unzip:
            Unzipper().recursive(rtp, logger)
