"""Tests for teamcity_test_results.py."""

from io import StringIO

from ci_tools.dimrset_delivery.teamcity_test_results import (
    ExecutiveSummary,
    ResultExecutiveSummary,
    ResultSummary,
    log_executive_summary,
    log_result_list,
    log_to_file,
)
from ci_tools.dimrset_delivery.teamcity_types import ConfigurationTestResult, ResultInfo


class TestResultExecutiveSummary:
    """Test cases for TestResultExecutiveSummary class."""

    def test_init_with_values(self) -> None:
        """Test TestResultExecutiveSummary initialization with values."""
        # Act
        summary = ResultExecutiveSummary(80, 20)

        # Assert
        assert summary.passed == 80
        assert summary.failed == 20
        assert summary.total == 100
        assert summary.percentage == 80.0

    def test_init_with_zero_total(self) -> None:
        """Test TestResultExecutiveSummary initialization with zero total."""
        # Act
        summary = ResultExecutiveSummary(0, 0)

        # Assert
        assert summary.passed == 0
        assert summary.failed == 0
        assert summary.total == 0
        assert summary.percentage == 0.0


class TestResultInfo:
    """Test cases for TestResult class."""

    def test_test_result_can_be_created(self) -> None:
        """Test that we can create a TestResult-like object for testing."""
        # Act
        test_result = ResultInfo(10, 9, 8, 7, 6, 5)

        # Assert
        assert test_result.get_total() == 35

    def test_get_total_with_muted_exception(self) -> None:
        """Test get_total method with muted exceptions."""
        # Arrange
        test_result = ResultInfo(10, 9, 8, 7, 6, 5)

        # Act & Assert
        assert test_result.get_total() == 35

    def test_get_not_passed_total(self) -> None:
        """Test get_not_passed_total method."""
        # Arrange
        test_result = ResultInfo(10, 9, 8, 7, 6, 5)

        # Act & Assert
        assert test_result.get_not_passed_total() == 30


class TestConfigurationTestResult:
    """Test cases for ConfigurationTestResult class."""

    def test_basic_functionality(self) -> None:
        """Test basic ConfigurationTestResult functionality."""
        # Arrange - no setup needed for constructor test

        # Act
        config_result = ConfigurationTestResult("Test Config", "123", 10, 3, 2, 1, "SUCCESS")

        # Assert
        assert config_result.name == "Test Config"
        assert config_result.build_nr == "123"
        assert config_result.status_text == "SUCCESS"
        assert config_result.test_result.passed == 10
        assert config_result.test_result.failed == 3
        assert config_result.test_result.ignored == 2
        assert config_result.test_result.muted == 1
        assert config_result.test_result.exception == 0
        assert config_result.test_result.muted_exception == 0
        assert config_result.exceptions == []
        assert config_result.get_total() == 16
        assert config_result.get_not_passed_total() == 6


class TestTeamcityTestResults:
    """Test cases for log_executive_summary function."""

    def test_log_to_file_single_arg(self) -> None:
        """Test log_to_file with a single argument."""
        # Arrange
        mock_file = StringIO()

        # Act
        log_to_file(mock_file, "Test message")

        # Assert
        output = mock_file.getvalue()
        assert output == "Test message\n"

    def test_log_to_file_multiple_args(self) -> None:
        """Test log_to_file with multiple arguments."""
        # Arrange
        mock_file = StringIO()

        # Act
        log_to_file(mock_file, "Test", "message", "with", "multiple", "args")

        # Assert
        output = mock_file.getvalue()
        assert output == "Test message with multiple args\n"

    def test_log_to_file_mixed_types(self) -> None:
        """Test log_to_file with mixed argument types."""
        # Arrange
        mock_file = StringIO()

        # Act
        log_to_file(mock_file, "Count:", 42, "Percentage:", 85.5)

        # Assert
        output = mock_file.getvalue()
        assert output == "Count: 42 Percentage: 85.5\n"

    def test_log_executive_summary_single_summary(self) -> None:
        """Test logging executive summary with a single summary."""
        # Arrange
        mock_file = StringIO()

        summary = ResultSummary("Windows Tests")
        summary.sum_passed = 85
        summary.sum_failed = 10
        summary.sum_exception = 3
        summary.sum_ignored = 2
        summary.sum_muted = 0

        executive_summary = ExecutiveSummary("DIMR Test Results", [summary])

        # Act
        log_executive_summary(mock_file, executive_summary)

        # Assert
        output = mock_file.getvalue()
        assert "Testbench root: DIMR Test Results" in output
        assert "Summary: Windows Tests" in output
        assert "Total tests   :    100" in output
        assert "    Passed    :     85" in output
        assert "    Not passed:     15" in output
        assert "    Failed    :     10" in output
        assert "    Exception :      3" in output
        assert "    Ignored   :      2" in output
        assert "    Muted     :      0" in output
        assert "    Percentage:  85.00" in output

    def test_log_executive_summary_multiple_summaries(self) -> None:
        """Test logging executive summary with multiple summaries."""
        # Arrange
        mock_file = StringIO()

        windows_summary = ResultSummary("Windows Tests")
        windows_summary.sum_passed = 85
        windows_summary.sum_failed = 10
        windows_summary.sum_exception = 3
        windows_summary.sum_ignored = 2
        windows_summary.sum_muted = 0

        linux_summary = ResultSummary("Linux Tests")
        linux_summary.sum_passed = 90
        linux_summary.sum_failed = 5
        linux_summary.sum_exception = 3
        linux_summary.sum_ignored = 1
        linux_summary.sum_muted = 1

        executive_summary = ExecutiveSummary("DIMR Test Results", [windows_summary, linux_summary])

        # Act
        log_executive_summary(mock_file, executive_summary)

        # Assert
        output = mock_file.getvalue()
        assert "Testbench root: DIMR Test Results" in output
        assert "Summary: Windows Tests" in output
        assert "Summary: Linux Tests" in output
        assert "    Percentage:  85.00" in output  # Windows percentage
        assert "    Percentage:  90.00" in output  # Linux percentage

    def test_log_executive_summary_zero_total(self) -> None:
        """Test logging executive summary with zero total tests."""
        # Arrange
        mock_file = StringIO()

        summary = ResultSummary("Empty Tests")
        # All values are 0 by default

        executive_summary = ExecutiveSummary("Empty Test Results", [summary])

        # Act
        log_executive_summary(mock_file, executive_summary)

        # Assert
        output = mock_file.getvalue()
        assert "Testbench root: Empty Test Results" in output
        assert "Summary: Empty Tests" in output
        assert "Total tests   :      0" in output
        assert "    Percentage:   0.00" in output

    def test_log_result_list_empty_engines(self) -> None:
        """Test log_result_list with empty engines list."""
        # Arrange
        mock_file = StringIO()
        engines = []

        # Act
        log_result_list(mock_file, "Test Results", engines)

        # Assert
        output = mock_file.getvalue()
        assert "Test Results" in output
        assert (
            "total   passed   failed   except  ignored    muted        %  ---  test case name           (#build)"
            in output
        )
        assert "    Total     :      0" in output
        assert "    Passed    :      0" in output
        assert "    Percentage:   0.00" in output

    def test_log_result_list_single_engine(self) -> None:
        """Test log_result_list with single engine."""
        # Arrange
        mock_file = StringIO()
        engine = ConfigurationTestResult("Test Config", "123", 50, 5, 3, 2, "SUCCESS")
        engines = [engine]

        # Act
        log_result_list(mock_file, "Single Engine Results", engines)

        # Assert
        output = mock_file.getvalue()
        assert "Single Engine Results" in output
        assert (
            "total   passed   failed   except  ignored    muted        %  ---  test case name           (#build)"
            in output
        )
        assert "          60       50        5        0        3        2    83.33  ---  Test Config" in output
        assert "    Total     :     60" in output
        assert "    Passed    :     50" in output
        assert "    Percentage:  83.33" in output

    def test_log_result_list_multiple_engines(self) -> None:
        """Test log_result_list with multiple engines."""
        # Arrange
        mock_file = StringIO()
        engine1 = ConfigurationTestResult("Windows Tests", "123", 40, 10, 5, 5, "SUCCESS")
        engine2 = ConfigurationTestResult("Linux Tests", "456", 60, 5, 3, 2, "SUCCESS")
        engines = [engine1, engine2]

        # Act
        log_result_list(mock_file, "Multiple Engine Results", engines)

        # Assert
        output = mock_file.getvalue()
        assert "Multiple Engine Results" in output
        assert (
            "total   passed   failed   except  ignored    muted        %  ---  test case name           (#build)"
            in output
        )
        assert "Windows Tests" in output
        assert "Linux Tests" in output
        assert "    Total     :    130" in output
        assert "    Passed    :    100" in output
        assert "    Percentage:  76.92" in output

    def test_log_result_list_engine_with_zero_total(self) -> None:
        """Test log_result_list with engine having zero total tests."""
        # Arrange
        mock_file = StringIO()
        engine = ConfigurationTestResult("Empty Config", "789", 0, 0, 0, 0, "FAILED")
        engines = [engine]

        # Act
        log_result_list(mock_file, "Zero Total Results", engines)

        # Assert
        output = mock_file.getvalue()
        assert "Zero Total Results" in output
        assert (
            "           x        x        x        x        x        x        x  ---  Empty Config             (#789)"
            in output
        )
        assert "                                                                            xxx  FAILED" in output
        assert "    Total     :      0" in output
        assert "    Passed    :      0" in output
        assert "    Percentage:   0.00" in output
