"""Shared types for TeamCity functionality."""

from typing import List


class ResultInfo:
    """A class to store result data.

    Parameters
    ----------
    passed : int
        Number of passed tests.
    failed : int
        Number of failed tests.
    ignored : int
        Number of ignored tests.
    muted : int
        Number of muted tests.
    exception : int
        Number of tests that raised an exception.
    muted_exception : int
        Number of muted exceptions.
    """

    def __init__(
        self, passed: int, failed: int, ignored: int, muted: int, exception: int, muted_exception: int
    ) -> None:
        self.passed = passed
        self.failed = failed
        self.ignored = ignored
        self.muted = muted
        self.exception = exception
        self.muted_exception = muted_exception

    def get_total(self) -> int:
        """Get total number of testcases.

        Returns
        -------
        int
            Total testcases.
        """
        return self.passed + self.failed + self.exception + self.ignored + self.muted - self.muted_exception

    def get_not_passed_total(self) -> int:
        """Get total number of testcases that did not pass.

        Returns
        -------
        int
            Total testcases that did not pass.
        """
        return self.failed + self.exception + self.ignored + self.muted


class ConfigurationTestResult:
    """A class to store configuration test results info.

    Parameters
    ----------
    name : str
        Name of the configuration.
    build_nr : str
        Build number.
    passed : int
        Number of passed tests.
    failed : int
        Number of failed tests.
    ignored : int
        Number of ignored tests.
    muted : int
        Number of muted tests.
    status_text : str
        Status text description.
    """

    def __init__(
        self,
        name: str,
        build_nr: str,
        passed: int,
        failed: int,
        ignored: int,
        muted: int,
        status_text: str,
    ) -> None:
        self.name = name
        self.build_nr = build_nr
        self.status_text = status_text
        self.exceptions: List[str] = []
        self.test_result = ResultInfo(passed, failed, ignored, muted, 0, 0)

    def get_total(self) -> int:
        """Get total number of testcases.

        Returns
        -------
        int
            Total testcases.
        """
        return self.test_result.get_total()

    def get_not_passed_total(self) -> int:
        """Get total number of testcases that did not pass.

        Returns
        -------
        int
            Total testcases that did not pass.
        """
        return self.test_result.get_not_passed_total()
