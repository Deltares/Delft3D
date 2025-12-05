"""Tests for the double precision converter."""
import pytest
from deltares_fortran_styler.double_precision_converter import DoublePrecisionConverter

class TestDoublePrecisionConverter:
    """Test suite for DoublePrecisionConverter."""

    @pytest.fixture
    def converter(self):
        """Create a converter instance for testing."""
        return DoublePrecisionConverter()

    def test_convert_simple_literal(self, converter):
        """Test conversion of simple double precision literal."""
        # Arrange
        text = "x = 1.0d0"
        expected = "x = 1.0_dp"

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted, "Should detect conversion is needed"
        assert result == expected, f"Expected '{expected}', got '{result}'"

    def test_convert_negative_literal(self, converter):
        """Test conversion of negative double precision literal."""
        # Arrange
        text = "x = -999d0"
        expected = "x = -999.0_dp"

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_convert_literal_with_exponent(self, converter):
        """Test conversion of literal with non-zero exponent."""
        # Arrange
        text = "x = 2.5d-3"
        expected = "x = 2.5e-3_dp"

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_convert_double_precision_declaration(self, converter):
        """Test conversion of double precision declaration."""
        # Arrange
        text = "   double precision :: x, y"
        expected = "   real(kind=dp) :: x, y"

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_convert_dble_function(self, converter):
        """Test conversion of dble() function call."""
        # Arrange
        text = "x = dble(5)"
        expected = "x = real(5, kind=dp)"

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_no_conversion_needed(self, converter):
        """Test that already converted code is not modified."""
        # Arrange
        text = "real(kind=dp) :: x = 1.0_dp"
        expected = text

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert not was_converted, "Should not detect conversion needed"
        assert result == expected

    def test_ignore_literal_in_comment(self, converter):
        """Test that literals in comments are not converted."""
        # Arrange
        text = "! This is a comment with 1.0d0"
        expected = text

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert not was_converted
        assert result == expected

    def test_ignore_literal_in_string(self, converter):
        """Test that literals in strings are not converted."""
        # Arrange
        text = "message = 'Value is 1.0d0'"
        expected = text

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert not was_converted
        assert result == expected

    def test_ignore_dble_in_comment(self, converter):
        """Test that dble() in comments is not converted."""
        # Arrange
        text = "! This uses dble(x) to convert"
        expected = text

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert not was_converted
        assert result == expected

    def test_module_with_conversions_adds_import(self, converter):
        """Test that use precision import is added for modules with conversions."""
        # Arrange
        text = """module test_module
   implicit none
contains
   subroutine test()
      double precision :: x
      x = 1.0d0
   end subroutine test
end module test_module"""

        # Act
        result, was_converted = converter.convert_text(text)
        result = converter.add_required_imports(result, was_converted)

        # Assert
        assert was_converted
        assert "use precision, only: dp" in result
        assert "real(kind=dp) :: x" in result
        assert "x = 1.0_dp" in result

    def test_submodule_with_conversions_adds_import(self, converter):
        """Test that use precision import is added for submodules."""
        # Arrange
        text = """submodule(parent) child
   implicit none
contains
   module subroutine test()
      double precision :: x
      x = 1.0d0
   end subroutine test
end submodule child"""

        # Act
        result, was_converted = converter.convert_text(text)
        result = converter.add_required_imports(result, was_converted)

        # Assert
        assert was_converted
        assert "use precision, only: dp" in result

    def test_does_not_match_module_subroutine(self, converter):
        """Test that 'module subroutine' is not treated as a module declaration."""
        # Arrange
        text = """module subroutine test()
   double precision :: x
   x = 1.0d0
end subroutine test"""

        # Act
        result, was_converted = converter.convert_text(text)
        result_with_imports = converter.add_required_imports(result, was_converted)

        # Assert
        assert was_converted, "Should convert the double precision code"
        # Should not add import because there's no module/submodule declaration
        assert result == result_with_imports, "Should not add imports without module/submodule"

    def test_submodule_with_module_subroutine_adds_import_correctly(self, converter):
        """Test that import is added after submodule declaration, not after module subroutine."""
        # Arrange
        text = """submodule(parent) child
   implicit none
contains
   module subroutine test()
      double precision :: x
      x = 1.0d0
   end subroutine test
end submodule child"""

        expected = """submodule(parent) child
   use precision, only: dp
   implicit none
contains
   module subroutine test()
      real(kind=dp) :: x
      x = 1.0_dp
   end subroutine test
end submodule child"""

        # Act
        result, was_converted = converter.convert_text(text)
        result = converter.add_required_imports(result, was_converted)

        # Assert
        assert was_converted
        assert result == expected

    def test_module_immediately_followed_by_code(self, converter):
        """Test import insertion when first line after module is code (no blank line, no existing uses).

        This tests the rare branch where insert_line_idx == 0 and lines[0] is NOT empty
        (module declaration not followed by newline in remaining_text).
        """
        # Arrange
        # Simulate module followed immediately by code (edge case)
        text = """module test
integer :: y
   double precision :: x
   x = 1.0d0"""

        expected = """module test
use precision, only: dp
integer :: y
   real(kind=dp) :: x
   x = 1.0_dp"""

        # Act
        result, was_converted = converter.convert_text(text)
        result = converter.add_required_imports(result, was_converted)

        # Assert
        assert was_converted
        assert result == expected

    def test_module_with_existing_use_statements(self, converter):
        """Test that import is added after existing use statements.

        This tests the branch where insert_line_idx != 0 and before_insert starts with newline.
        """
        # Arrange
        text = """module test_module
   use other_module
   implicit none
   double precision :: x
   x = 1.0d0
end module test_module"""

        expected = """module test_module
   use other_module
   use precision, only: dp
   implicit none
   real(kind=dp) :: x
   x = 1.0_dp
end module test_module"""

        # Act
        result, was_converted = converter.convert_text(text)
        result = converter.add_required_imports(result, was_converted)

        # Assert
        assert was_converted
        assert result == expected

    def test_module_with_multiple_use_statements_no_newline_after_module(self, converter):
        """Test import addition when module declaration has content on same line after it.

        This tests the branch where insert_line_idx != 0 and before_insert
        does NOT start with newline (rare edge case).
        """
        # Arrange - Module followed by semicolon and use statement on same line (unusual but valid Fortran)
        # Create a scenario where remaining_text doesn't start with \n
        text = """module test; use mod1
   use mod2
   implicit none
   double precision :: x
   x = 1.0d0
end module test"""

        # Act
        result, was_converted = converter.convert_text(text)
        result = converter.add_required_imports(result, was_converted)

        # Assert
        assert was_converted
        # Should add import after the use statements
        assert "use mod2\n" in result and "use precision, only: dp" in result
        assert "real(kind=dp) :: x" in result

    def test_check_text_returns_issues(self, converter):
        """Test that check_text identifies style issues."""
        # Arrange
        text = """double precision :: x
x = 1.0d0
y = dble(5)"""

        # Act
        issues = converter.check_text(text)

        # Assert
        assert len(issues) == 3, f"Expected 3 issues, found {len(issues)}"
        assert any(issue.error_code == "STYLE001" for issue in issues), "Should find literal issue"
        assert any(issue.error_code == "STYLE002" for issue in issues), "Should find declaration issue"
        assert any(issue.error_code == "STYLE003" for issue in issues), "Should find dble() issue"

    def test_get_conversion_stats(self, converter):
        """Test that conversion statistics are correctly computed."""
        # Arrange
        text = """double precision :: x, y
x = 1.0d0
y = 2.5d-3
z = dble(x) + dble(y)"""

        # Act
        stats = converter.get_conversion_stats(text)

        # Assert
        assert stats['literals'] == 2, "Should count 2 literals"
        assert stats['declarations'] == 1, "Should count 1 declaration"
        assert stats['dble_calls'] == 2, "Should count 2 dble calls"

    @pytest.mark.parametrize(
        ("input_text", "expected_output"),
        [
            pytest.param("1d0", "1.0_dp", id="integer_no_decimal"),
            pytest.param("1.d0", "1.0_dp", id="trailing_dot"),
            pytest.param(".5d0", "0.5_dp", id="leading_dot"),
            pytest.param("123.456d0", "123.456_dp", id="full_decimal"),
            pytest.param("+1.0d0", "+1.0_dp", id="explicit_positive"),
            pytest.param("-1.0d0", "-1.0_dp", id="negative"),
            pytest.param("1.0d+5", "1.0e+5_dp", id="positive_exponent"),
            pytest.param("1.0d-5", "1.0e-5_dp", id="negative_exponent"),
        ],
    )
    def test_literal_format_variations(self, converter, input_text, expected_output):
        """Test various formats of double precision literals."""
        # Arrange
        text = f"x = {input_text}"
        expected = f"x = {expected_output}"

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_multiple_literals_on_same_line(self, converter):
        """Test conversion of multiple literals on the same line."""
        # Arrange
        text = "x = 1.0d0 + 2.0d0 * 3.0d-1"
        expected = "x = 1.0_dp + 2.0_dp * 3.0e-1_dp"

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_nested_dble_calls(self, converter):
        """Test conversion of nested dble() function calls.

        Note: The current regex-based implementation converts from outside-in,
        so only the outermost dble() is converted in a single pass.
        """
        # Arrange
        text = "x = dble(dble(5) + 3)"
        # Only outermost dble() is converted in one pass
        expected = "x = real(dble(5) + 3, kind=dp)"

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_double_precision_with_comma_no_extra_space(self, converter):
        """Test that conversion doesn't add extra space before comma."""
        # Arrange
        text = "   double precision, save :: x, y"
        expected = "   real(kind=dp), save :: x, y"

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected, f"Expected '{expected}', got '{result}'"

    def test_check_reports_correct_line_number(self, converter):
        """Test that check_text reports correct line numbers."""
        # Arrange
        text = """module test
   implicit none
   integer :: a
   double precision :: x
   x = 1.0d0
end module"""

        # Act
        issues = converter.check_text(text)

        # Assert
        assert len(issues) == 2, f"Expected 2 issues, found {len(issues)}"
        # double precision declaration should be on line 4
        declaration_issue = [i for i in issues if i.error_code == "STYLE002"][0]
        assert declaration_issue.line_number == 4, f"Expected line 4, got {declaration_issue.line_number}"
        # literal should be on line 5
        literal_issue = [i for i in issues if i.error_code == "STYLE001"][0]
        assert literal_issue.line_number == 5, f"Expected line 5, got {literal_issue.line_number}"
