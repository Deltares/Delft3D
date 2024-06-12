"""
Description: TestBenchSettings data class
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2023
"""

import sys
from typing import List, Optional

from src.config.credentials import Credentials
from src.config.local_paths import LocalPaths
from src.config.program_config import ProgramConfig
from src.config.test_case_config import TestCaseConfig
from src.config.types.mode_type import ModeType
from src.utils.common import log_separator, log_sub_header
from src.utils.logging.log_level import LogLevel
from src.utils.logging.logger import Logger
from src.config.types.path_type import PathType


class TestBenchSettings:
    """Settings for a test bench run"""

    log_level: LogLevel = LogLevel.INFO
    local_paths: Optional[LocalPaths] = None
    programs: List[ProgramConfig] = []
    configs: List[TestCaseConfig] = []
    run_mode: ModeType = ModeType.COMPARE
    config_file: str = ""
    credentials: Credentials = Credentials()
    filter: str = ""
    autocommit: bool = False
    skip_run: bool = False
    skip_download: List[PathType] = []
    teamcity: bool = False
    parallel: bool = False
    test_bench_root: Optional[str] = None
    test_bench_script_name: Optional[str] = None
    test_bench_startup_dir: Optional[str] = None
    server_base_url: str = ""
    override_paths: str = ""

    def log_overview(self, logger: Logger):
        """Logs overview of the parameters

        Args:
            logger (Logger): logger to log to
        """
        log_sub_header("Parsed arguments", logger)

        name_map = {
            PathType.DEPENDENCY: "dependency",
            PathType.INPUT: "input of cases",
            PathType.REFERENCE: "references",
        }
        download = ", ".join(val for key, val in name_map.items() if key not in self.skip_download)

        logger.info(f"Version  : {sys.version}")
        logger.info(f"Mode     : {self.run_mode}")
        logger.info(f"Config   : {self.config_file}")
        logger.info(f"Filter   : {self.filter}")
        logger.info(f"LogLevel : {str(self.log_level)}")
        logger.info(f"Download : [{download}]")
        if self.skip_run:
            logger.info("Execute  : skip")
        logger.info(f"Username : {self.credentials.username}")

        log_separator(logger, char="-", with_new_line=True)
