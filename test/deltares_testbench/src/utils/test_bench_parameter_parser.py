"""
Description: parser for handling supplied arguments
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2023
"""

import getpass
import operator
import os
import sys
from argparse import ArgumentParser, Namespace
from typing import Any, List, Optional

from src.config.credentials import Credentials
from src.config.test_case_config import TestCaseConfig
from src.config.types.mode_type import ModeType
from src.suite.test_bench_settings import TestBenchSettings
from src.utils.common import get_log_level
from src.utils.xml_config_parser import XmlConfigParser


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

        new_settings = TestBenchSettings()

        # Store path of Testbench.py into os environment

        script_path, script_name = os.path.split(os.path.abspath(__file__))

        new_settings.test_bench_root = script_path
        new_settings.test_bench_script_name = script_name
        new_settings.test_bench_startup_dir = os.getcwd()

        credentials = cls.__get_credentials(args)

        # Additionally, extra TeamCity messages will be produced.
        server_base_url = cls.__get_argument_value("server_base_url", args) or ""

        # Parse the xml file.
        xml_config_parser = XmlConfigParser()

        config_file = cls.__get_argument_value("config", args) or "config.xml"
        (
            new_settings.local_paths,
            new_settings.programs,
            new_settings.configs,
        ) = xml_config_parser.load(
            config_file, args.__dict__["or_paths"], credentials, server_base_url
        )

        new_settings.config_file = config_file

        # Filter the testcases to be run
        if args.filter != "":
            new_settings.configs = cls.__filter_configs__(
                new_settings.configs, args.filter
            )

        # Loglevel from config.xml can be overruled by loglevel from arguments
        if args.__dict__["loglevel"] != "":
            new_settings.log_level = get_log_level(args.__dict__["loglevel"])

        # Do not run the model, only run the post-processing (comparison).
        new_settings.only_post = cls.__get_argument_value("only_post", args) or False

        # automatically commit reference run if succesfull
        new_settings.autocommit = cls.__get_argument_value("autocommit", args) or False

        # Enables running the tests in parallel (multi-process)
        new_settings.parallel = cls.__get_argument_value("parallel", args) or False

        # If option is used, all logging is decorated with TeamCity messages.
        # Additionally, extra TeamCity messages will be produced.
        new_settings.teamcity = cls.__get_argument_value("teamcity", args) or False

        new_settings.filter = args.filter
        new_settings.config_file = config_file
        new_settings.user_name = credentials.username

        # Determine type of run
        new_settings.run_mode = (
            cls.__get_argument_value("run_mode", args) or ModeType.LIST
        )

        return new_settings

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

            return input(f"{name}")

        return return_value

    @classmethod
    def __get_credentials(cls, args: Namespace) -> Credentials:
        credentials = Credentials()
        credentials.name = "commandline"

        is_interactive = cls.__get_argument_value("interactive", args) or False
        credentials.username = (
            cls.__get_argument_value("username", args, is_interactive) or ""
        )
        if credentials.username == "":
            print(
                'No username on commandline. add "-i True" to enable interactive input'
            )

        credentials.password = (
            cls.__get_argument_value("password", args, is_interactive, True) or ""
        )
        if credentials.password == "":
            print(
                'No password on commandline. add "-i True" to enable interactive input'
            )

        return credentials

    @classmethod
    def __filter_configs__(cls, configs: List[TestCaseConfig], args):
        """check which filters to apply to configuration"""
        filtered: List[TestCaseConfig] = []
        filters = args.split(":")
        program_filter = None
        test_case_filter = None
        max_runtime = None
        operator = None
        start_at_filter = None

        for filter in filters:
            con, arg = filter.split("=")
            con = con.lower()
            if con == "program":
                program_filter = arg.lower()
            elif con == "testcase":
                test_case_filter = arg.lower()
            elif con == "maxruntime":
                max_runtime = float(arg[1:])
                operator = arg[:1]
            elif con == "startat":
                start_at_filter = arg.lower()
            else:
                error_message = "ERROR: Filter keyword " " + con + " " not recognised\n"
                sys.stderr.write(error_message)
                raise SyntaxError(error_message)

        # For each testcase (p, t, mrt filters):
        for config in configs:
            c = cls.__find_characteristics__(
                config, program_filter, test_case_filter, max_runtime, operator
            )
            if c:
                filtered.append(c)

        # StartAt filter:
        if start_at_filter:
            starti = len(filtered)
            for i, config in enumerate(filtered):
                if start_at_filter in config.name:
                    starti = i
                    break
            filtered[:] = filtered[starti:]
        return filtered

    @classmethod
    def __find_characteristics__(
        cls, config: TestCaseConfig, program: Optional[str], testcase, mrt, op
    ) -> Optional[TestCaseConfig]:
        """check if a test case matches given characteristics"""
        found_program = None
        found_testcase = None
        found_max_runtime = None

        if program:
            # Program is a string, containing names of programs, comma separated
            programs = program.split(",")
            for aprog in programs:
                found_program = any(
                    aprog in e.name.lower() for e in config.program_configs
                )
                if found_program:
                    break
        if testcase:
            # testcase is a string, containing (parts of) testcase names, comma separated
            testcases = testcase.split(",")
            for atestcase in testcases:
                found_testcase = atestcase in config.name.lower()
                if found_testcase:
                    break
        if mrt:
            mappings = {">": operator.gt, "<": operator.lt, "=": operator.eq}
            found_max_runtime = mappings[op](config.max_run_time, mrt)

        if (
            ((not program and not found_program) or (program and found_program))
            and ((not testcase and not found_testcase) or (testcase and found_testcase))
            and ((not mrt and not found_max_runtime) or (mrt and found_max_runtime))
        ):
            return config
        return None

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
            "--only-post-process",
            help="Skip running",
            action="store_true",
            dest="only_post",
        )
        parser.add_argument(
            "--parallel",
            action="store_true",
            help="Turns on running in parallel.",
            dest="parallel",
        )
        parser.add_argument(
            "--autocommit",
            action="store_true",
            help="Turns on autocommit when doing a reference run.",
            dest="autocommit",
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
