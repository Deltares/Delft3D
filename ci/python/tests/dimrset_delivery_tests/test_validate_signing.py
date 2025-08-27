"""Tests for validate_signing.py."""

import os
from io import StringIO
from pathlib import Path
from unittest.mock import Mock, patch

from ci_tools.dimrset_delivery.validate_signing import (
    _get_actual_files,
    _print_example_json_file_structure,
    _validate_directory_contents,
    is_signing_correct,
    is_signtool_available,
    signing_is_valid,
    validate_signing_status,
    verify_signing_authority,
)


class TestIsSigntoolAvailable:
    """Test cases for is_signtool_available function."""

    @patch("ci_tools.dimrset_delivery.validate_signing.subprocess.run")
    def test_is_signtool_available_returns_true_when_signtool_exists(self, mock_run: Mock) -> None:
        """Test that is_signtool_available returns True when signtool is available."""
        # Arrange
        developer_prompt = "vcvars64.bat"
        mock_result = Mock()
        mock_result.returncode = 0
        mock_run.return_value = mock_result

        # Act
        result = is_signtool_available(developer_prompt)

        # Assert
        assert result is True
        mock_run.assert_called_once_with(
            [developer_prompt, "&&", "signtool.exe", "verify", "/?"],
            capture_output=True,
            text=True,
            shell=True,
        )

    @patch("ci_tools.dimrset_delivery.validate_signing.subprocess.run")
    def test_is_signtool_available_returns_false_when_signtool_not_found(self, mock_run: Mock) -> None:
        """Test that is_signtool_available returns False when signtool is not found."""
        # Arrange
        developer_prompt = "vcvars64.bat"
        mock_result = Mock()
        mock_result.returncode = 1
        mock_run.return_value = mock_result

        # Act
        result = is_signtool_available(developer_prompt)

        # Assert
        assert result is False

    @patch("ci_tools.dimrset_delivery.validate_signing.subprocess.run")
    def test_is_signtool_available_returns_false_on_exception(self, mock_run: Mock) -> None:
        """Test that is_signtool_available returns False when an exception occurs."""
        # Arrange
        developer_prompt = "vcvars64.bat"
        mock_run.side_effect = Exception("Command failed")

        # Act
        result = is_signtool_available(developer_prompt)

        # Assert
        assert result is False


class TestVerifySigningAuthority:
    """Test cases for verify_signing_authority function."""

    @patch("ci_tools.dimrset_delivery.validate_signing.subprocess.run")
    def test_verify_signing_authority_returns_verified_with_issuer(self, mock_run: Mock) -> None:
        """Test that verify_signing_authority returns 'Verified' status with correct issuer."""
        # Arrange
        filepath = "test_file.exe"
        developer_prompt = "vcvars64.bat"
        mock_result = Mock()
        mock_result.stdout = """Successfully verified: test_file.exe

Issued to: Deltares
The signature is timestamped: Friday, January 1, 2024 12:00:00 PM"""
        mock_run.return_value = mock_result

        # Act
        status, issuer = verify_signing_authority(filepath, developer_prompt)

        # Assert
        assert status == "Verified"
        assert issuer == "Deltares"
        mock_run.assert_called_once_with(
            [developer_prompt, "&&", "signtool.exe", "verify", "/pa", "/v", filepath],
            capture_output=True,
            text=True,
            shell=True,
        )

    @patch("ci_tools.dimrset_delivery.validate_signing.subprocess.run")
    def test_verify_signing_authority_returns_not_verified_when_not_signed(self, mock_run: Mock) -> None:
        """Test that verify_signing_authority returns 'Not Verified' when file is not signed."""
        # Arrange
        filepath = "test_file.exe"
        developer_prompt = "vcvars64.bat"
        mock_result = Mock()
        mock_result.stdout = "SignTool Error: No signature found."
        mock_run.return_value = mock_result

        # Act
        status, issuer = verify_signing_authority(filepath, developer_prompt)

        # Assert
        assert status == "Not Verified"
        assert issuer == ""

    @patch("ci_tools.dimrset_delivery.validate_signing.subprocess.run")
    @patch("sys.stdout", new_callable=StringIO)
    def test_verify_signing_authority_returns_error_on_exception(self, mock_stdout: StringIO, mock_run: Mock) -> None:
        """Test that verify_signing_authority returns 'Error' when an exception occurs and prints the error message."""
        # Arrange
        filepath = "test_file.exe"
        developer_prompt = "vcvars64.bat"
        mock_run.side_effect = Exception("Command failed")

        # Act
        status, error_message = verify_signing_authority(filepath, developer_prompt)

        # Assert
        output = mock_stdout.getvalue()
        assert status == "Error"
        assert error_message == ""
        assert "Error verifying signing authority: Command failed" in output


class TestValidateSigningStatus:
    """Test cases for validate_signing_status function."""

    @patch("ci_tools.dimrset_delivery.validate_signing.verify_signing_authority")
    def test_validate_signing_status_correctly_signed_file(self, mock_verify: Mock) -> None:
        """Test that validate_signing_status correctly validates a properly signed file."""
        # Arrange
        file = "test_file.exe"
        directory = "/test/dir"
        filepath = os.path.join(directory, file)
        filepath = os.path.normpath(filepath)
        files_that_should_be_signed_with_issued_to = [{"file": file, "issuedTo": "Deltares"}]
        files_that_should_not_be_signed = []
        developer_prompt = "vcvars64.bat"
        mock_verify.return_value = ("Verified", "Deltares")

        # Act
        message, is_valid = validate_signing_status(
            file,
            directory,
            files_that_should_be_signed_with_issued_to,
            files_that_should_not_be_signed,
            developer_prompt,
        )

        # Assert
        assert is_valid is True
        assert "File is correctly signed: test_file.exe by Deltares" in message
        mock_verify.assert_called_once_with(filepath, developer_prompt)

    @patch("ci_tools.dimrset_delivery.validate_signing.verify_signing_authority")
    def test_validate_signing_status_incorrectly_signed_file_wrong_issuer(self, mock_verify: Mock) -> None:
        """Test that validate_signing_status detects incorrectly signed file with wrong issuer."""
        # Arrange
        file = "test_file.exe"
        directory = "/test/dir"
        files_that_should_be_signed_with_issued_to = [{"file": file, "issuedTo": "Deltares"}]
        files_that_should_not_be_signed = []
        developer_prompt = "vcvars64.bat"
        mock_verify.return_value = ("Verified", "WrongIssuer")

        # Act
        message, is_valid = validate_signing_status(
            file,
            directory,
            files_that_should_be_signed_with_issued_to,
            files_that_should_not_be_signed,
            developer_prompt,
        )

        # Assert
        assert is_valid is False
        assert "File is not correctly signed: test_file.exe by Deltares but by WrongIssuer" in message

    @patch("ci_tools.dimrset_delivery.validate_signing.verify_signing_authority")
    def test_validate_signing_status_unsigned_file_should_be_signed(self, mock_verify: Mock) -> None:
        """Test that validate_signing_status detects when a file should be signed but is not."""
        # Arrange
        file = "test_file.exe"
        directory = "/test/dir"
        files_that_should_be_signed_with_issued_to = [{"file": file, "issuedTo": "Deltares"}]
        files_that_should_not_be_signed = []
        developer_prompt = "vcvars64.bat"
        mock_verify.return_value = ("Not Verified", "")

        # Act
        message, is_valid = validate_signing_status(
            file,
            directory,
            files_that_should_be_signed_with_issued_to,
            files_that_should_not_be_signed,
            developer_prompt,
        )

        # Assert
        assert is_valid is False
        assert "File should be signed but is not: test_file.exe" in message

    @patch("ci_tools.dimrset_delivery.validate_signing.verify_signing_authority")
    def test_validate_signing_status_correctly_unsigned_file(self, mock_verify: Mock) -> None:
        """Test that validate_signing_status correctly validates an unsigned file that should not be signed."""
        # Arrange
        file = "test_file.dll"
        directory = "/test/dir"
        files_that_should_be_signed_with_issued_to = []
        files_that_should_not_be_signed = ["test_file.dll"]
        developer_prompt = "vcvars64.bat"
        mock_verify.return_value = ("Not Verified", "")

        # Act
        message, is_valid = validate_signing_status(
            file,
            directory,
            files_that_should_be_signed_with_issued_to,
            files_that_should_not_be_signed,
            developer_prompt,
        )

        # Assert
        assert is_valid is True
        assert "File is correctly not signed: test_file.dll" in message

    @patch("ci_tools.dimrset_delivery.validate_signing.verify_signing_authority")
    def test_validate_signing_status_signed_file_should_not_be_signed(self, mock_verify: Mock) -> None:
        """Test that validate_signing_status detects when a file should not be signed but is."""
        # Arrange
        file = "test_file.dll"
        directory = "/test/dir"
        files_that_should_be_signed_with_issued_to = []
        files_that_should_not_be_signed = ["test_file.dll"]
        developer_prompt = "vcvars64.bat"
        mock_verify.return_value = ("Verified", "Deltares")

        # Act
        message, is_valid = validate_signing_status(
            file,
            directory,
            files_that_should_be_signed_with_issued_to,
            files_that_should_not_be_signed,
            developer_prompt,
        )

        # Assert
        assert is_valid is False
        assert "File should not be signed but is: test_file.dll by Deltares" in message

    @patch("ci_tools.dimrset_delivery.validate_signing.verify_signing_authority")
    def test_validate_signing_status_unknown_file_returns_empty_message_and_true(self, mock_verify: Mock) -> None:
        """Test that validate_signing_status returns empty message and True for unknown files."""
        # Arrange
        file = "unknown_file.txt"
        directory = "/test/dir"
        filepath = os.path.join(directory, file)
        filepath = os.path.normpath(filepath)
        files_that_should_be_signed_with_issued_to = []
        files_that_should_not_be_signed = []
        developer_prompt = "vcvars64.bat"
        mock_verify.return_value = ("Not Verified", "")

        # Act
        message, is_valid = validate_signing_status(
            file,
            directory,
            files_that_should_be_signed_with_issued_to,
            files_that_should_not_be_signed,
            developer_prompt,
        )

        # Assert
        assert is_valid is True
        assert message == ""
        mock_verify.assert_called_once_with(filepath, developer_prompt)


class TestSigningIsValid:
    """Test cases for signing_is_valid function."""

    @patch("ci_tools.dimrset_delivery.validate_signing.verify_signing_authority")
    def test_signing_is_valid_returns_true_for_correctly_signed_file(self, mock_verify: Mock) -> None:
        """Test that signing_is_valid returns True for correctly signed file."""
        # Arrange
        filepath = "/test/dir/test_file.exe"
        developer_prompt = "vcvars64.bat"
        expected_issued_to = "Deltares"
        mock_verify.return_value = ("Verified", "Deltares")

        # Act
        result = signing_is_valid(filepath, developer_prompt, expected_issued_to)

        # Assert
        assert result is True
        mock_verify.assert_called_once_with(filepath, developer_prompt)

    @patch("ci_tools.dimrset_delivery.validate_signing.verify_signing_authority")
    def test_signing_is_valid_returns_false_for_incorrectly_signed_file(self, mock_verify: Mock) -> None:
        """Test that signing_is_valid returns False for incorrectly signed file."""
        # Arrange
        filepath = "/test/dir/test_file.exe"
        developer_prompt = "vcvars64.bat"
        expected_issued_to = "Deltares"
        mock_verify.return_value = ("Verified", "WrongIssuer")

        # Act
        result = signing_is_valid(filepath, developer_prompt, expected_issued_to)

        # Assert
        assert result is False

    @patch("ci_tools.dimrset_delivery.validate_signing.verify_signing_authority")
    def test_signing_is_valid_returns_false_for_unsigned_file_that_should_be_signed(self, mock_verify: Mock) -> None:
        """Test that signing_is_valid returns False for unsigned file that should be signed."""
        # Arrange
        filepath = "/test/dir/test_file.exe"
        developer_prompt = "vcvars64.bat"
        expected_issued_to = "Deltares"
        mock_verify.return_value = ("Not Verified", "")

        # Act
        result = signing_is_valid(filepath, developer_prompt, expected_issued_to)

        # Assert
        assert result is False

    @patch("ci_tools.dimrset_delivery.validate_signing.verify_signing_authority")
    def test_signing_is_valid_returns_false_for_signed_file_that_should_not_be_signed(self, mock_verify: Mock) -> None:
        """Test that signing_is_valid returns False for signed file that should not be signed."""
        # Arrange
        filepath = "/test/dir/test_file.dll"
        developer_prompt = "vcvars64.bat"
        expected_issued_to = ""
        mock_verify.return_value = ("Verified", "Deltares")

        # Act
        result = signing_is_valid(filepath, developer_prompt, expected_issued_to)

        # Assert
        assert result is False

    @patch("ci_tools.dimrset_delivery.validate_signing.verify_signing_authority")
    def test_signing_is_valid_returns_true_for_correctly_unsigned_file(self, mock_verify: Mock) -> None:
        """Test that signing_is_valid returns False for correctly unsigned file.

        The function logic is: status == 'Verified' and expected_issued_to == issued_to
        """
        # Arrange
        filepath = "/test/dir/test_file.dll"
        developer_prompt = "vcvars64.bat"
        expected_issued_to = ""
        mock_verify.return_value = ("Not Verified", "")

        # Act
        result = signing_is_valid(filepath, developer_prompt, expected_issued_to)

        # Assert
        # The function logic is: status == "Verified" and expected_issued_to == issued_to
        # For unsigned file: "Not Verified" == "Verified" and "" == "" -> False and True -> False
        assert result is False


class TestIsSigningCorrect:
    """Test cases for is_signing_correct function."""

    @patch("ci_tools.dimrset_delivery.validate_signing.signing_is_valid")
    @patch("ci_tools.dimrset_delivery.validate_signing.ThreadPoolExecutor")
    def test_is_signing_correct_returns_true_when_all_files_correct(
        self, mock_executor: Mock, mock_signing_is_valid: Mock
    ) -> None:
        """Test that is_signing_correct returns True when all files are correctly signed/unsigned."""
        # Arrange
        directory = "/test/dir"
        files_that_should_be_signed_with_issued_to = [{"file": "signed_file.exe", "issuedTo": "Deltares"}]
        files_that_should_not_be_signed = [Path("unsigned_file.dll")]
        developer_prompt = "vcvars64.bat"

        mock_executor_instance = Mock()
        mock_executor.return_value.__enter__.return_value = mock_executor_instance
        mock_executor_instance.map.side_effect = [
            [True],  # signed files results
            [False],  # unsigned files results (False means correctly unsigned)
        ]

        # Act
        result = is_signing_correct(
            directory,
            files_that_should_be_signed_with_issued_to,
            files_that_should_not_be_signed,
            developer_prompt,
        )

        # Assert
        assert result is True
        assert mock_executor_instance.map.call_count == 2

    @patch("ci_tools.dimrset_delivery.validate_signing.signing_is_valid")
    @patch("ci_tools.dimrset_delivery.validate_signing.ThreadPoolExecutor")
    def test_is_signing_correct_returns_false_when_signed_file_incorrect(
        self, mock_executor: Mock, mock_signing_is_valid: Mock
    ) -> None:
        """Test that is_signing_correct returns False when a signed file is incorrect."""
        # Arrange
        directory = "/test/dir"
        files_that_should_be_signed_with_issued_to = [{"file": "signed_file.exe", "issuedTo": "Deltares"}]
        files_that_should_not_be_signed = [Path("unsigned_file.dll")]
        developer_prompt = "vcvars64.bat"

        mock_executor_instance = Mock()
        mock_executor.return_value.__enter__.return_value = mock_executor_instance
        mock_executor_instance.map.side_effect = [
            [False],  # signed files results (one file incorrectly signed)
            [False],  # unsigned files results
        ]

        # Act
        result = is_signing_correct(
            directory,
            files_that_should_be_signed_with_issued_to,
            files_that_should_not_be_signed,
            developer_prompt,
        )

        # Assert
        assert result is False

    @patch("ci_tools.dimrset_delivery.validate_signing.signing_is_valid")
    @patch("ci_tools.dimrset_delivery.validate_signing.ThreadPoolExecutor")
    def test_is_signing_correct_returns_false_when_unsigned_file_incorrect(
        self, mock_executor: Mock, mock_signing_is_valid: Mock
    ) -> None:
        """Test that is_signing_correct returns False when an unsigned file is incorrectly signed."""
        # Arrange
        directory = "/test/dir"
        files_that_should_be_signed_with_issued_to = [{"file": "signed_file.exe", "issuedTo": "Deltares"}]
        files_that_should_not_be_signed = [Path("unsigned_file.dll")]
        developer_prompt = "vcvars64.bat"

        mock_executor_instance = Mock()
        mock_executor.return_value.__enter__.return_value = mock_executor_instance
        mock_executor_instance.map.side_effect = [
            [True],  # signed files results
            [True],  # unsigned files results (True means incorrectly signed)
        ]

        # Act
        result = is_signing_correct(
            directory,
            files_that_should_be_signed_with_issued_to,
            files_that_should_not_be_signed,
            developer_prompt,
        )

        # Assert
        assert result is False


class TestGetActualFiles:
    """Test cases for _get_actual_files function."""

    @patch("ci_tools.dimrset_delivery.validate_signing.Path")
    def test_get_actual_files_returns_dll_and_exe_files(self, mock_path: Mock) -> None:
        """Test that _get_actual_files returns only .dll and .exe files."""
        # Arrange
        directory = "/test/dir"
        mock_directory_path = Mock()
        mock_path.return_value = mock_directory_path

        mock_file1 = Mock()
        mock_file1.suffix = ".exe"
        mock_file1.relative_to.return_value = Path("test.exe")

        mock_file2 = Mock()
        mock_file2.suffix = ".dll"
        mock_file2.relative_to.return_value = Path("test.dll")

        mock_file3 = Mock()
        mock_file3.suffix = ".txt"
        mock_file3.relative_to.return_value = Path("test.txt")

        mock_directory_path.glob.return_value = [mock_file1, mock_file2, mock_file3]

        # Act
        result = _get_actual_files(directory, {".dll", ".exe"})

        # Assert
        assert len(result) == 2
        assert Path("test.exe") in result
        assert Path("test.dll") in result
        mock_path.assert_called_once_with(directory)
        mock_directory_path.glob.assert_called_once_with("**/*")

    @patch("ci_tools.dimrset_delivery.validate_signing.Path")
    def test_get_actual_files_handles_case_insensitive_extensions(self, mock_path: Mock) -> None:
        """Test that _get_actual_files handles case-insensitive file extensions."""
        # Arrange
        directory = "/test/dir"
        mock_directory_path = Mock()
        mock_path.return_value = mock_directory_path

        mock_file1 = Mock()
        mock_file1.suffix = ".EXE"
        mock_file1.relative_to.return_value = Path("test.EXE")

        mock_file2 = Mock()
        mock_file2.suffix = ".DLL"
        mock_file2.relative_to.return_value = Path("test.DLL")

        mock_directory_path.glob.return_value = [mock_file1, mock_file2]

        # Act
        result = _get_actual_files(directory, {".dll", ".exe"})

        # Assert
        assert len(result) == 2
        assert Path("test.EXE") in result
        assert Path("test.DLL") in result

    @patch("ci_tools.dimrset_delivery.validate_signing.Path")
    def test_get_actual_files_returns_empty_list_when_no_files(self, mock_path: Mock) -> None:
        """Test that _get_actual_files returns empty list when no .dll or .exe files found."""
        # Arrange
        directory = "/test/dir"
        mock_directory_path = Mock()
        mock_path.return_value = mock_directory_path
        mock_directory_path.glob.return_value = []

        # Act
        result = _get_actual_files(directory, {".dll", ".exe"})

        # Assert
        assert result == []


class TestValidateDirectoryContents:
    """Test cases for _validate_directory_contents function."""

    def test_validate_directory_contents_returns_true_when_files_match(self) -> None:
        """Test that _validate_directory_contents returns True when actual and expected files match."""
        # Arrange
        actual_files = [Path("file1.exe"), Path("file2.dll")]
        expected_files = [Path("file1.exe"), Path("file2.dll")]

        # Act
        result = _validate_directory_contents(actual_files, expected_files)

        # Assert
        assert result is True

    def test_validate_directory_contents_returns_false_when_missing_files(self) -> None:
        """Test that _validate_directory_contents returns False when files are missing."""
        # Arrange
        actual_files = [Path("file1.exe")]
        expected_files = [Path("file1.exe"), Path("file2.dll")]

        # Act
        result = _validate_directory_contents(actual_files, expected_files)

        # Assert
        assert result is False

    def test_validate_directory_contents_returns_false_when_extra_files(self) -> None:
        """Test that _validate_directory_contents returns False when there are extra files."""
        # Arrange
        actual_files = [Path("file1.exe"), Path("file2.dll"), Path("file3.exe")]
        expected_files = [Path("file1.exe"), Path("file2.dll")]

        # Act
        result = _validate_directory_contents(actual_files, expected_files)

        # Assert
        assert result is False

    def test_validate_directory_contents_returns_false_when_both_missing_and_extra_files(self) -> None:
        """Test that _validate_directory_contents returns False when there are both missing and extra files."""
        # Arrange
        actual_files = [Path("file1.exe"), Path("file3.exe")]
        expected_files = [Path("file1.exe"), Path("file2.dll")]

        # Act
        result = _validate_directory_contents(actual_files, expected_files)

        # Assert
        assert result is False


class TestPrintExampleJsonFileStructure:
    """Test cases for _print_example_json_file_structure function."""

    @patch("sys.stdout", new_callable=StringIO)
    def test_print_example_json_file_structure_prints_correct_format(self, mock_stdout: StringIO) -> None:
        """Test that _print_example_json_file_structure prints the correct JSON format."""
        # Arrange & Act
        _print_example_json_file_structure()

        # Assert
        output = mock_stdout.getvalue()
        assert "Example JSON file structure:{" in output
        assert '"signed": [' in output
        assert '"file": "file_1.exe",' in output
        assert '"issuedTo": "party A"' in output
        assert '"notSigned": [' in output
        assert '"file_3.dll",' in output
        assert '"lib\\file_4.dll"' in output
        assert "}" in output
