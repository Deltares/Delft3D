"""
Description: Main Application for running Tests
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2023
"""
import logging

from src.suite.test_bench import TestBench
from src.utils.logging.logger import Logger
from src.utils.test_bench_parameter_parser import TestBenchParameterParser

if __name__ == "__main__":
    logging.getLogger("matplotlib").setLevel(level=logging.CRITICAL)

    # get settings from arguments
    settings = TestBenchParameterParser.parse_arguments_to_settings()
    logger = Logger(settings.log_level, settings.teamcity)

    settings.log_overview(logger)

    # create and run testbench
    test_bench = TestBench(settings, logger)
    test_bench.run()
