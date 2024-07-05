"""
Description: FTP handler
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
"""

import os
from typing import Optional
import urllib.parse as parse
from ftplib import FTP, error_perm

from src.config.credentials import Credentials
from src.utils.handlers.i_handler import IHandler
from src.utils.logging.i_logger import ILogger


# Upload and download for ftp paths
class FTPHandler(IHandler):
    # Download data from location
    # input: from, to and credentials
    def download(
        self,
        from_path: str,
        to_path: str,
        credentials: Credentials,
        version: Optional[str],
        logger: ILogger,
    ):
        logger.debug(f"setting up connection to FTP: {from_path}")
        url = parse.urlparse(from_path)
        ftp = FTP(url.netloc)
        if credentials:
            ftp.login(credentials.username, credentials.password)
            logger.debug(f"connecting as: {credentials.username}")
        else:
            ftp.login()
        ftp.cwd(url.path)
        logger.debug(f"going to root: {url.path}")
        # create root on filesystem
        if not os.path.exists(to_path):
            os.makedirs(to_path)
        logger.debug("analysing directory structure on ftp")
        self.__traverseDirectoryDownload__(ftp, "/", to_path, logger)
        ftp.close()

    # recursive traverse, fills ftpdirs array
    # cwd is the local working directory
    # frompath is relative dir of local system
    # destination is abs dir of ftp
    # topath is relative dir of ftp
    def __traverseDirectoryUpload__(
        self, ftp, cwd, frompath, destination, topath, logger: ILogger
    ):
        try:
            ftp.cwd(destination)
            if topath:
                ftp.cwd(topath)
            ndir = os.path.basename(os.path.normpath(frompath))
            ftp.mkd(ndir)
            logger.debug(f"built ftp frompath : {frompath.replace(os.sep, '/')}")
        except error_perm:
            # invalid entry (ensure input form: "/dir/folder/something/")
            logger.debug(f"frompath {frompath.replace(os.sep, '/')} already exists")
        # list children:
        filelist = os.listdir(os.path.join(cwd, frompath))
        for locfile in filelist:
            if os.path.isdir(os.path.join(cwd, frompath, locfile)):
                topath = "/"
                if frompath:
                    parts = os.path.split(frompath)
                    if parts:
                        for part in parts:
                            topath = topath + part + "/"
                self.__traverseDirectoryUpload__(
                    ftp,
                    cwd,
                    os.path.join(frompath, locfile),
                    destination,
                    topath,
                    logger,
                )
            else:
                ftp.cwd("/" + destination + frompath.replace(os.sep, "/"))
                with open(os.path.join(cwd, frompath, locfile)) as lf:
                    ftp.storbinary("STOR " + locfile, lf)

                logger.debug(f"uploaded {locfile}")
        return

    # recursive traverse download files
    # frompath is str of the form "/dir/folder/something/"
    # frompath should be the abs frompath to the root FOLDER of the file tree to download
    def __traverseDirectoryDownload__(self, ftp, path, destination, logger: ILogger):
        topath = destination + path.replace("/", os.sep)
        try:
            ftp.cwd(path)
            # clone path to destination
            os.makedirs(topath)
            logger.debug(f"built path : {topath}")
        except OSError:
            # folder already exists at destination
            pass
        except error_perm:
            # invalid entry (ensure input form: "/dir/folder/something/")
            raise OSError("error: could not change to %s", path)
        # list children:
        filelist = ftp.nlst()
        for ftpfile in filelist:
            try:
                # this will check if ftpfile is folder:
                ftp.cwd(path + ftpfile + "/")
                # if so, explore it:
                self.__traverseDirectoryDownload__(
                    ftp, path + ftpfile + "/", destination, logger
                )
            except error_perm:
                # possibly need a permission exception catch:
                with open(os.path.join(topath, ftpfile), "wb") as ff:
                    ftp.retrbinary("RETR " + ftpfile, ff.write)

                logger.debug(f"downloaded {ftpfile}")
        return
