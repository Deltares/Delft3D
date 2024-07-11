"""
Description: Local and Network handler
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
"""

import os
import shutil
from typing import Optional

from src.config.credentials import Credentials
from src.config.types.handler_type import HandlerType
from src.utils.common import mount_network_drive, unmount_network_drive
from src.utils.handlers.i_handler import IHandler
from src.utils.handlers.resolve_handler import ResolveHandler
from src.utils.logging.i_logger import ILogger
from src.utils.paths import Paths


# Upload and download for local and network paths
class LocalNetHandler(IHandler):
    # Download data from location
    # input: from, to and optional credentials
    def download(
        self,
        from_path: str,
        to_path: str,
        credentials: Credentials,
        version: Optional[str],
        logger: ILogger,
    ):
        handler = ResolveHandler.detect(from_path, logger, credentials)
        rtp = Paths().rebuildToLocalPath(to_path)
        if handler == HandlerType.PATH:
            rfp = Paths().rebuildToLocalPath(from_path)
            logger.debug(
                f"copying locally from {os.path.abspath(rfp)} to {os.path.abspath(rfp)}"
            )
            shutil.copytree(os.path.abspath(rfp), os.path.abspath(rtp))
        if handler == HandlerType.NET:
            server, folder, rest = Paths().splitNetworkPath(from_path)
            mp, nm = mount_network_drive(server, folder, credentials, logger)
            logger.debug(f"mounted share to {mp}")
            e = None
            try:
                netpath = os.path.join(mp + os.sep, rest)
                logger.debug(
                    f"copying from net from {os.path.abspath(netpath)} to {os.path.abspath(rtp)}"
                )
                shutil.copytree(netpath, os.path.abspath(rtp))
            except Exception as e:
                error_message = str(e).replace("'", "")
                logger.error(f"exception during network transfer {error_message}")
            finally:
                if nm:
                    unmount_network_drive(mp)
                    logger.debug("unmounted share")
                if e:
                    raise e
