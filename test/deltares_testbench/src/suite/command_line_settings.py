"""TestBenchSettings data class.

Copyright (C)  Stichting Deltares, 2025
"""

from typing import List

from src.config.credentials import Credentials
from src.config.types.mode_type import ModeType
from src.config.types.path_type import PathType
from src.utils.logging.log_level import LogLevel


class CommandLineSettings:
    """Settings from command line arguments."""

    log_level: LogLevel = LogLevel.INFO
    run_mode: ModeType = ModeType.COMPARE
    config_file: str = ""
    credentials: Credentials = Credentials()
    filter: str = ""
    skip_run: bool = False
    skip_download: List[PathType] | None = None
    teamcity: bool = False
    parallel: bool = False
    test_bench_root: str | None = None
    test_bench_script_name: str | None = None
    test_bench_startup_dir: str | None = None
    server_base_url: str = ""
    override_paths: str = ""
    skip_post_processing: bool = False

    def __init__(self) -> None:
        """Initialize with proper default values for mutable attributes."""
        if self.skip_download is None:
            self.skip_download = []
