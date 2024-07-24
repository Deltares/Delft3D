"""File unzipper.

Copyright (C)  Stichting Deltares, 2024
"""

import fnmatch
import os
import zipfile

from src.utils.logging.i_logger import ILogger


class Unzipper(object):
    """Compare files on ASCII content equality."""

    def __unzip__(self, zip_file_path: str) -> None:
        """Unzip file to path.

        Parameters
        ----------
        zfp : str
            Full path to zip file name.
        """
        fh = open(zip_file_path, "rb")
        z = zipfile.ZipFile(fh)
        z.extractall(os.path.dirname(zip_file_path))
        fh.close()

    def recursive(self, path: str, logger: ILogger) -> None:
        """Unzip all zip files in directory (recursive).

        Parameters
        ----------
        path : str
            Path to zip file
        """
        matches = []
        for dirpath, _, filenames in os.walk(path):
            for f in fnmatch.filter(filenames, "*.zip"):
                matches.append(os.path.abspath(os.path.join(dirpath, f)))
        for m in matches:
            logger.debug(f"unzipping {m}")
            self.__unzip__(m)
