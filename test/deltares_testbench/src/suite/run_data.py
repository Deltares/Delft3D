"""Data class for storing test run data.

Copyright (C)  Stichting Deltares, 2024
"""

from datetime import datetime, timedelta
from typing import Optional


class RunData:
    """Data class for storing test run data."""

    def __init__(
        self,
        test_number: int,
        number_of_tests: int,
    ) -> None:
        self.__creation_time: datetime = datetime.now()
        self.__start_time: Optional[datetime] = None
        self.__end_time: Optional[datetime] = None
        self.__process_id: int = 0
        self.__process_name: str = "Unknown"
        self.__test_number: int = test_number
        self.__number_of_tests: int = number_of_tests

    @property
    def creation_time(self) -> datetime:
        """Time that the data object was created."""
        return self.__creation_time

    @creation_time.setter
    def creation_time(self, creation_time: datetime) -> None:
        self.__creation_time = creation_time

    @property
    def start_time(self) -> Optional[datetime]:
        """Time that the run started."""
        return self.__start_time

    @start_time.setter
    def start_time(self, start_time: datetime) -> None:
        self.__start_time = start_time

    @property
    def end_time(self) -> Optional[datetime]:
        """Time that the run ended."""
        return self.__end_time

    @end_time.setter
    def end_time(self, end_time: datetime) -> None:
        self.__end_time = end_time

    @property
    def process_id(self) -> int:
        """Id of the process that the test runs on."""
        return self.__process_id

    @process_id.setter
    def process_id(self, value: int) -> None:
        self.__process_id = value

    @property
    def process_name(self) -> str:
        """Name of the process that the test runs on."""
        return self.__process_name

    @process_name.setter
    def process_name(self, value: str) -> None:
        self.__process_name = value

    @property
    def test_number(self) -> int:
        """Test number (index of the test in the test list)."""
        return self.__test_number

    @property
    def number_of_tests(self) -> int:
        """Total number of tests that are run."""
        return self.__number_of_tests

    @property
    def test_duration(self) -> Optional[timedelta]:
        """Duration of the test run."""
        if self.__end_time and self.__start_time:
            return self.__end_time - self.__start_time

        return None

    @property
    def waiting_period(self) -> Optional[timedelta]:
        """Waiting period for test to start."""
        if self.start_time:
            return self.start_time - self.creation_time

        return None

    @property
    def absolute_duration(self) -> Optional[timedelta]:
        """Duration of the test run from creation to finish."""
        if self.end_time:
            return self.end_time - self.creation_time

        return None

    def timing_str(self) -> str:
        """Report timings as string."""
        duration = self.__chop_microseconds(self.test_duration)
        waiting_period = self.__chop_microseconds(self.waiting_period)

        return f"duration {duration} - waiting {waiting_period}"

    def absolute_duration_str(self) -> str:
        absolute_duration = self.__chop_microseconds(self.absolute_duration)
        return f"{absolute_duration}"

    def index_str(self) -> str:
        """Report test index."""
        max_str_length = len(str(self.number_of_tests))
        return f"{str(self.test_number).rjust(max_str_length)}/{self.number_of_tests}"

    def process_id_str(self) -> str:
        """Report process information."""
        return f"{str(self.process_id).rjust(6)} - {self.process_name}"

    def __chop_microseconds(self, delta: Optional[timedelta]) -> Optional[timedelta]:
        if delta:
            return delta - timedelta(microseconds=delta.microseconds)

        return None
