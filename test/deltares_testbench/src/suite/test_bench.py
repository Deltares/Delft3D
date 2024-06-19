"""
Description: Main testbench class for running Tests
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2023
"""
from typing import Optional

from src.config.types.mode_type import ModeType
from src.suite.comparison_runner import ComparisonRunner
from src.suite.reference_runner import ReferenceRunner
from src.suite.test_bench_settings import TestBenchSettings
from src.suite.test_set_runner import TestSetRunner
from src.utils.common import log_header, log_separator_with_name
from src.utils.logging.i_main_logger import IMainLogger


class TestBench:
    """Testbench instance"""

    def __init__(self, run_settings: TestBenchSettings, logger: IMainLogger) -> None:
        self.settings = run_settings
        self.logger = logger

    def run(self):
        """runs the testbench"""

        log_header("Start of test-bench", self.logger)
        if self.settings.run_mode == ModeType.LIST:
            self.__print_filter_usage()
            return

        if self.settings.run_mode == ModeType.TEST_CASE_LIST:
            self.__print_test_case_list()
            return

        runner: Optional[TestSetRunner] = None
        if self.settings.run_mode == ModeType.COMPARE:
            runner = ComparisonRunner(self.settings, self.logger)
        elif self.settings.run_mode == ModeType.REFERENCE:
            runner = ReferenceRunner(self.settings, self.logger)
        else:
            self.logger.error(
                f"Run mode {self.settings.run_mode} is currently unsupported"
            )

        try:
            if runner:
                runner.run()
                log_separator_with_name(
                    f"Testbench run finished normally {runner.duration}",
                    self.logger,
                    char="*",
                )

        except Exception as e:
            self.logger.exception(f"Could not run testbench: {repr(e)}")

    def __print_filter_usage(self):
        """Only return the list of test cases to be run."""
        message_lines = [
            "A filter pattern must be formatted as follows:",
            "",
            '--filter "program=waq,dflowfm:testcase=aaa,bbb:maxruntime=<10:startat=ccc"',
            "\tprogram    (program name   = at least one substring)",
            "\ttestcase   (test case name = at least one substring)",
            "\tmaxruntime (test run time  = float, larger, smaller, equals)",
            "\tstartat    (test case name = substring is the first testcase)",
        ]

        for line in message_lines:
            self.logger.error(line)

    def __print_test_case_list(self):
        for testcase_config in self.settings.configs_to_run:
            self.logger.info(testcase_config.name + "\n")
