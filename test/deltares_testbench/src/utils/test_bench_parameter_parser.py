"""
Description: parser for handling supplied arguments
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2023
"""

import getpass
import os
from argparse import ArgumentParser, Namespace
from typing import Any, Optional, List

from src.config.credentials import Credentials
from src.config.types.mode_type import ModeType
from src.utils.handlers.credential_handler import CredentialHandler
from src.suite.test_bench_settings import TestBenchSettings
from src.utils.common import get_log_level
from src.config.types.path_type import PathType


class TestBenchParameterParser:
    """Handles the parsing of the testbench parameters"""

    @classmethod
    def parse_arguments_to_settings(cls) -> TestBenchSettings:
        """Parses args (namespace) to a TestBenchSettings object

        Args:
            args (Namespace): namespace containing the parameter data

        Returns:
            TestBenchSettings: Parsed settings
        """

        parser = cls.__create_argument_parser()
        args: Namespace = parser.parse_args()

        settings = TestBenchSettings()

        # Store path of Testbench.py into os environment
        script_path, script_name = os.path.split(os.path.abspath(__file__))

        settings.test_bench_root = script_path
        settings.test_bench_script_name = script_name
        settings.test_bench_startup_dir = os.getcwd()

        settings.server_base_url = (
            cls.__get_argument_value("server_base_url", args) or ""
        )
        settings.override_paths = args.__dict__["or_paths"]

        # Loglevel from config.xml can be overruled by loglevel from arguments
        if args.__dict__["loglevel"] != "":
            settings.log_level = get_log_level(args.__dict__["loglevel"])

        # Do not run the programs associated with the testcase.
        settings.skip_run = cls.__get_argument_value("skip_run", args) or False

        # Skip specified downloads for the testcase xml.
        skip_download = cls.__get_argument_value("skip_download", args) or []
        for skip_string in skip_download:
            path_type = cls.get_path_type(skip_string)
            settings.skip_download.extend(path_type)

        # Enables running the tests in parallel (multi-process)
        settings.parallel = cls.__get_argument_value("parallel", args) or False

        # If option is used, all logging is decorated with TeamCity messages.
        # Additionally, extra TeamCity messages will be produced.
        settings.teamcity = cls.__get_argument_value("teamcity", args) or False

        settings.filter = args.filter
        # Determine type of run
        settings.run_mode = (
            cls.__get_argument_value("run_mode", args) or ModeType.LIST
        )
        settings.config_file = (
            cls.__get_argument_value("config", args) or "config.xml"
        )
        settings.credentials = cls.__get_credentials(args, settings.teamcity)

        return settings

    @classmethod
    def __get_argument_value(
        cls,
        name: str,
        args: Namespace,
        is_interactive: bool = False,
        secret_value: bool = False,
    ) -> Optional[Any]:
        return_value = None

        if hasattr(args, name):
            return_value = getattr(args, name)

        if not return_value and is_interactive:
            if secret_value:
                return getpass.getpass(f"{name} : ")

            return input(f"{name} : ")

        return return_value

    @classmethod
    def __get_credentials(cls, args: Namespace, is_active_directory_user: bool) -> Credentials:
        credentials = Credentials()
        credential_handler = CredentialHandler(credentials)
        credentials.name = "commandline"

        is_interactive = cls.__get_argument_value("interactive", args) or False
        make_interactive = not credential_handler.credential_file_exists() and is_interactive
        credentials.username = (
            cls.__get_argument_value("username", args, make_interactive) or ""
        )

        credentials.password = (
            cls.__get_argument_value("password", args, make_interactive, True) or ""
        )

        if not is_active_directory_user:
            credential_handler.setup_credentials(is_interactive)
        return credentials

    @classmethod
    def get_path_type(cls, skip_string) -> List[PathType]:
        if skip_string == "cases":
            return [PathType.INPUT]
        elif skip_string == "references":
            return [PathType.REFERENCE]
        elif skip_string == "dependency":
            return [PathType.DEPENDENCY]
        elif skip_string == "all":
            return [PathType.INPUT, PathType.REFERENCE, PathType.DEPENDENCY]
        else:
            return []

    @classmethod
    def __create_argument_parser(cls) -> ArgumentParser:
        parser = ArgumentParser(
            description="Test Bench Version 3, test runner for black box tests.",
            add_help=True,
        )

        # Compulsory arguments
        run_mode_group = parser.add_mutually_exclusive_group(required=True)
        run_mode_group.add_argument(
            "-r",
            "--reference",
            help="Execute a reference run",
            dest="run_mode",
            action="store_const",
            const=ModeType.REFERENCE,
        )
        run_mode_group.add_argument(
            "-c",
            "--compare",
            help="Execute a compare run",
            dest="run_mode",
            action="store_const",
            const=ModeType.COMPARE,
        )
        run_mode_group.add_argument(
            "-l",
            "--list",
            help="Only list parameters for filtering",
            dest="run_mode",
            action="store_const",
            const=ModeType.LIST,
        )
        run_mode_group.add_argument(
            "-t",
            "--testcaselist",
            help="Only list test cases to be run",
            dest="run_mode",
            action="store_const",
            const=ModeType.TEST_CASE_LIST,
        )

        # Optional arguments
        parser.add_argument(
            "--config",
            default="",
            help="Path to config file, if empty default config file is used",
            dest="config",
        )
        parser.add_argument(
            "--filter",
            default="",
            help="Specify what tests to run based on filter (--list for options)",
            dest="filter",
        )
        parser.add_argument(
            "--log-level",
            default="",
            help="CRITICAL, ERROR, WARNING, INFO or DEBUG",
            dest="loglevel",
        )
        parser.add_argument(
            "--override-paths",
            default="",
            help="root[name]=some path,from[name]=some path,to[name]=some path,path[name]=some path,....",
            dest="or_paths",
        )
        parser.add_argument(
            "--skip-run",
            help="Skips running of programs defined in xml configuration referenced by the testcases.",
            action="store_true",
            dest="skip_run",
        )
        parser.add_argument(
            "--skip-download",
            help="Skip downloads of [references,cases,dependency,all]",
            default="",
            choices=["cases", "references", "dependency", "all"],
            nargs="+",
            type=str,
            dest="skip_download",
        )
        parser.add_argument(
            "--parallel",
            action="store_true",
            help="Turns on running in parallel.",
            dest="parallel",
        )
        parser.add_argument(
            "--teamcity",
            action="store_true",
            help="Turns on specific TeamCity logging.",
            dest="teamcity",
        )
        parser.add_argument(
            "--server-base-url",
            help="e.g. SVN, S3, Git LFS",
            default="https://s3.deltares.nl/dsc-testbench",
            required=False,
            dest="server_base_url",
        )
        parser.add_argument(
            "--username",
            help="Server username (e.g. git, SVN, MinIO).",
            default=None,
            # required=True,
            dest="username",
        )
        parser.add_argument(
            "--password",
            help="Server password (e.g. git, SVN, MinIO).",
            default=None,
            # required=True,
            dest="password",
        )
        parser.add_argument(
            "-i",
            "--interactive",
            action="store_true",
            help="Must be True to enable username/password via keyboard.",
            dest="interactive",
        )

        return parser
