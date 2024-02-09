"""
Description: Executes SVN commands
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
"""

from src.config.credentials import Credentials
from src.config.program_config import ProgramConfig
from src.suite.program import Program
from src.utils.handlers.i_handler import IHandler
from src.utils.logging.i_logger import ILogger
from src.utils.common import stripPassword


# SVN wrapper, has handler interface
class SvnHandler(IHandler):
    def __init__(self, svn_program: Program, autocommit=False):
        self.autocommit = autocommit
        self.svn_program = svn_program

    def prepare_upload(
        self, from_path: str, to_path: str, credentials: Credentials, logger: ILogger
    ) -> None:
        logger.debug(f"Preparing svn upload from {from_path} to {to_path}")

        # Prepare svn and its configuration.
        pcnf = ProgramConfig()
        pcnf.working_directory = from_path
        prg = self.svn_program

        # Use the mkdir command to make sure that the reference directory exists.
        arguments = self.__buildInitialArguments__(["mkdir", "--parents"], credentials)
        arguments.extend([to_path])
        arguments.extend(['-m"Automated_creation_of_reference_directory_by_TestBench"'])
        pcnf.arguments = arguments
        pcnf.ignore_return_value = True  # In case the folder already exists in SVN, a reference run has already been executed. Suppress warnings and error return value.
        pcnf.ignore_standard_error = True
        prg.overwriteConfiguration(pcnf)
        prg.run(logger)

        self.download(to_path, from_path, credentials, "HEAD", logger)

    # upload to svn
    # input: local path, svn path, credentials
    def upload(
        self, from_path: str, to_path: str, credentials: Credentials, logger: ILogger
    ) -> None:
        logger.debug(f"Uploading from  {from_path} to {to_path}")

        # Prepare svn and its configuration.
        pcnf = ProgramConfig()
        pcnf.working_directory = from_path
        prg = self.svn_program

        # Add the new files
        logger.debug("Adding files...")
        arguments = self.__buildInitialArguments__(["add", "--force"], credentials)
        arguments.extend(["."])
        pcnf.arguments = arguments
        prg.overwriteConfiguration(pcnf)
        prg.run(logger)

        if self.autocommit:
            logger.debug("Auto-committing changes...")
            arguments = self.__buildInitialArguments__(["commit"], credentials)
            arguments.extend(["."])
            arguments.extend(['-m"Automated_commit_from_TestBench"'])
            pcnf.arguments = arguments
            prg.overwriteConfiguration(pcnf)
            prg.run(logger)
        else:
            logger.info("Autocommit not selected; user needs to commit manually.")

    # download from svn
    # input: svn path, local path, credentials, version
    def download(
        self,
        from_path: str,
        to_path: str,
        credentials: Credentials,
        version: str,
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
        if self.autocommit:
            svn_io = "checkout"

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
