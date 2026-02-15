"""Main Application for running Tests.

Copyright (C)  Stichting Deltares, 2026
"""

import logging

from src.suite.test_bench import TestBench
from src.suite.test_bench_settings import TestBenchSettings
from src.utils.logging.logger import Logger
from src.utils.test_bench_parameter_parser import TestBenchParameterParser
from src.utils.xml_config_parser import XmlConfigParser

if __name__ == "__main__":
    logging.getLogger("matplotlib").setLevel(level=logging.CRITICAL)

    command_line_settings = TestBenchParameterParser.parse_arguments_to_settings()

    logger = Logger(command_line_settings.log_level, command_line_settings.teamcity)
    xml_config = XmlConfigParser().load(command_line_settings, logger)

    run_settings = TestBenchSettings()
    run_settings.setup_runtime_settings(command_line_settings, xml_config, logger)
    run_settings.log_overview(logger)

    test_bench = TestBench(run_settings, logger)
    test_bench.run()
