from src.utils.logging.i_logger import ILogger
from src.utils.logging.log_level import LogLevel


class TestLogger(ILogger):
    def error(self, message: str) -> None:
        print(f"Error: {message}")

    def exception(self, message: str) -> None:
        print(f"exception: {message}")

    def warning(self, message: str) -> None:
        print(f"warning: {message}")

    def info(self, message: str) -> None:
        print(f"info: {message}")

    def debug(self, message: str) -> None:
        print(f"debug: {message}")

    def log(self, message: str, log_level: LogLevel, exc_info: bool = False) -> None:
        print(f"{log_level}: {message}")
