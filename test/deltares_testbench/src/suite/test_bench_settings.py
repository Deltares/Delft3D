"""TestBenchSettings data class.

Copyright (C)  Stichting Deltares, 2026
"""

import sys
from typing import List

from src.config.local_paths import LocalPaths
from src.config.program_config import ProgramConfig
from src.config.test_case_config import TestCaseConfig
from src.config.types.path_type import PathType
from src.suite.command_line_settings import CommandLineSettings
from src.utils.common import log_separator, log_sub_header
from src.utils.logging.i_logger import ILogger
from src.utils.logging.logger import Logger
from src.utils.xml_config_parser import XmlConfig, XmlConfigParser


class TestBenchSettings:
    """Settings for a test bench run."""

    __test__ = False

    command_line_settings: CommandLineSettings = CommandLineSettings()
    configs_to_run: List[TestCaseConfig] = None
    local_paths: LocalPaths
    programs: List[ProgramConfig] = None

    def setup_runtime_settings(self, settings: CommandLineSettings, xml_config: XmlConfig, logger: ILogger) -> None:
        """Set up runtime settings based on command line settings and XML configuration."""
        self.command_line_settings = settings
        self.local_paths = xml_config.local_paths
        self.programs = xml_config.program_configs
        self.configs_to_run = XmlConfigParser.filter_configs(xml_config.testcase_configs, settings.filter, logger)

    def __init__(self) -> None:
        """Initialize with proper default values for mutable attributes."""
        if self.programs is None:
            self.programs = []
        if self.configs_to_run is None:
            self.configs_to_run = []

    def log_overview(self, logger: Logger) -> None:
        """Log overview of the parameters.

        Parameters
        ----------
        logger: Logger
            Logger to log to.
        """
        log_sub_header("Parsed arguments", logger)

        name_map = {
            PathType.DEPENDENCY: "dependency",
            PathType.INPUT: "input of cases",
            PathType.REFERENCE: "references",
        }
        skip_targets = set(self.command_line_settings.skip_download or [])
        downloads = [val for key, val in name_map.items() if key not in skip_targets]
        downloads_comma_separated = ", ".join(downloads)

        logger.info(f"Version  : {sys.version}")
        logger.info(f"Mode     : {self.command_line_settings.run_mode}")
        logger.info(f"Config   : {self.command_line_settings.config_file}")
        logger.info(f"Filter   : {self.command_line_settings.filter}")
        logger.info(f"LogLevel : {str(self.command_line_settings.log_level)}")
        logger.info(f"Download : [{downloads_comma_separated}]")
        if self.command_line_settings.skip_run:
            logger.info("Execute  : skip")
        logger.info(f"Username : {self.command_line_settings.credentials.username}")

        log_separator(logger, char="-", with_new_line=True)
