"""
Description: Main Application for running Tests
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2023
"""

import logging

from src.suite.test_bench import TestBench
from src.utils.logging.logger import Logger
from src.utils.test_bench_parameter_parser import TestBenchParameterParser
from src.utils.xml_config_parser import XmlConfigParser

if __name__ == "__main__":
    logging.getLogger("matplotlib").setLevel(level=logging.CRITICAL)

    settings = TestBenchParameterParser.parse_arguments_to_settings()
    logger = Logger(settings.log_level, settings.teamcity)

    (
        settings.local_paths,
        settings.programs,
        settings.configs,
    ) = XmlConfigParser().load(settings, logger)

    # Filter the testcases to be run
    if settings.filter != "":
        settings.configs = XmlConfigParser.filter_configs(
            settings.configs, settings.filter, logger
        )

    settings.log_overview(logger)

    # create and run testbench
    test_bench = TestBench(settings, logger)
    test_bench.run()
