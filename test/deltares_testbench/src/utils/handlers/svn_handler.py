"""
Description: Executes SVN commands
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
"""

from typing import Optional
from src.config.credentials import Credentials
from src.config.program_config import ProgramConfig
from src.suite.program import Program
from src.utils.handlers.i_handler import IHandler
from src.utils.logging.i_logger import ILogger
from src.utils.common import stripPassword


# SVN wrapper, has handler interface
class SvnHandler(IHandler):
    def __init__(self, svn_program: Program):
        self.svn_program = svn_program

    # download from svn
    # input: svn path, local path, credentials, version
    def download(
        self,
        from_path: str,
        to_path: str,
        credentials: Credentials,
        version: Optional[str],
        logger: ILogger,
    ):
        logger.debug(f"downloading from svn: {from_path}")
        logger.debug(f"-                 to: {to_path}")
        arguments = None
        revision = "-r HEAD"
        if version and version != "":
            revision = "-r " + version
            logger.debug(f"-                 revison: {version}")

        svn_io = "export"

        # Even the "checkout" has the option "--force", to be sure that the files on teamcity are those from SVN
        arguments = self.__buildInitialArguments__(
            [svn_io, "--force", revision], credentials
        )
        arguments.extend([from_path, "."])
        prg = self.svn_program
        pcnf = ProgramConfig()
        pcnf.working_directory = to_path
        pcnf.arguments = arguments
        prg.overwriteConfiguration(pcnf)
        prg.run(logger)
        if prg.getError():
            error_string = stripPassword(str(prg.getError()))
            raise RuntimeError("Errors during svn download: " + error_string)

    # default svn arguments
    # input: initial command, credentials
    # output: argument string
    def __buildInitialArguments__(self, initial, credentials: Credentials):
        arguments = initial
        arguments.extend(
            ["--no-auth-cache", "--non-interactive", "--trust-server-cert"]
        )
        if credentials:
            arguments.extend(
                ["--username", credentials.username, "--password", credentials.password]
            )
        return arguments
