"""Tests for the semicolon separator converter."""
import pytest
from deltares_fortran_styler.semicolon_separator_converter import SemicolonSeparatorConverter

class TestSemicolonSeparatorConverter:
    """Test suite for SemicolonSeparatorConverter."""

    @pytest.fixture
    def converter(self):
        """Create a converter instance for testing."""
        return SemicolonSeparatorConverter()

    def test_convert_simple_statements(self, converter):
        """Test conversion of simple semicolon-separated statements."""
        # Arrange
        text = "a = 1; b = 2"
        expected = "a = 1\nb = 2"

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_convert_three_statements(self, converter):
        """Test conversion of three semicolon-separated statements."""
        # Arrange
        text = "a = 1; b = 2; c = 3"
        expected = "a = 1\nb = 2\nc = 3"

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_preserve_indentation(self, converter):
        """Test that indentation is preserved when splitting statements."""
        # Arrange
        text = "    a = 1; b = 2"
        expected = "    a = 1\n    b = 2"

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_no_conversion_needed(self, converter):
        """Test that lines without semicolons are not modified."""
        # Arrange
        text = "a = 1\nb = 2"
        expected = text

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert not was_converted
        assert result == expected

    def test_ignore_semicolon_in_comment(self, converter):
        """Test that semicolons in comments are not split."""
        # Arrange
        text = "a = 1 ! This is a comment; with semicolon"
        expected = text

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert not was_converted
        assert result == expected

    def test_ignore_semicolon_in_single_quote_string(self, converter):
        """Test that semicolons in single-quoted strings are not split."""
        # Arrange
        text = "message = 'Hello; World'"
        expected = text

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert not was_converted
        assert result == expected

    def test_ignore_semicolon_in_double_quote_string(self, converter):
        """Test that semicolons in double-quoted strings are not split."""
        # Arrange
        text = 'message = "Hello; World"'
        expected = text

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert not was_converted
        assert result == expected

    def test_mixed_statements_and_strings(self, converter):
        """Test statements with strings containing semicolons."""
        # Arrange
        text = "a = 'test;'; b = 2"
        expected = "a = 'test;'\nb = 2"

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_statement_with_comment(self, converter):
        """Test splitting statements where one has a trailing comment."""
        # Arrange
        text = "a = 1; b = 2 ! comment"
        expected = "a = 1\nb = 2 ! comment"

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_multiline_text(self, converter):
        """Test conversion across multiple lines."""
        # Arrange
        text = """program test
    a = 1; b = 2
    c = 3
end program"""
        expected = """program test
    a = 1
    b = 2
    c = 3
end program"""

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_multiple_lines_with_semicolons(self, converter):
        """Test multiple lines each with semicolons."""
        # Arrange
        text = """a = 1; b = 2
c = 3; d = 4"""
        expected = """a = 1
b = 2
c = 3
d = 4"""

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_complex_expressions(self, converter):
        """Test splitting statements with complex expressions."""
        # Arrange
        text = "x = a + b; y = c * d"
        expected = "x = a + b\ny = c * d"

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_function_calls(self, converter):
        """Test splitting statements with function calls."""
        # Arrange
        text = "call foo(); call bar()"
        expected = "call foo()\ncall bar()"

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_check_text_returns_issues(self, converter):
        """Test that check_text identifies semicolon-separated statements."""
        # Arrange
        text = """a = 1; b = 2
c = 3
d = 4; e = 5"""

        # Act
        issues = converter.check_text(text)

        # Assert
        assert len(issues) == 2
        assert all(issue.error_code == "STYLE005" for issue in issues)
        assert issues[0].line_number == 1
        assert issues[1].line_number == 3

    def test_get_conversion_stats(self, converter):
        """Test that conversion statistics are correctly computed."""
        # Arrange
        text = """a = 1; b = 2
c = 3; d = 4; e = 5
f = 6"""

        # Act
        stats = converter.get_conversion_stats(text)

        # Assert
        assert stats['semicolon_separators'] == 3

    def test_needs_import_returns_false(self, converter):
        """Test that semicolon separator converter doesn't need imports."""
        # Assert
        assert not converter.needs_import()

    def test_add_required_imports_returns_unchanged(self, converter):
        """Test that add_required_imports returns text unchanged."""
        # Arrange
        text = "some code"

        # Act
        result = converter.add_required_imports(text, was_converted=True)

        # Assert
        assert result == text

    def test_escaped_quotes_in_strings(self, converter):
        """Test handling of escaped quotes in strings."""
        # Arrange
        text = "msg = 'He''s here'; x = 1"
        expected = "msg = 'He''s here'\nx = 1"

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_spaces_around_semicolon(self, converter):
        """Test that spacing around semicolons doesn't affect conversion."""
        # Arrange
        text = "a = 1 ; b = 2"
        expected = "a = 1\nb = 2"

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_empty_statement(self, converter):
        """Test handling of trailing semicolon (empty statement)."""
        # Arrange
        text = "a = 1;"
        expected = "a = 1"  # Trailing semicolon should be removed

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_leading_semicolon(self, converter):
        """Test handling of leading semicolon."""
        # Arrange
        text = "; a = 1"
        expected = "a = 1"  # Leading semicolon should be removed

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_trailing_semicolon_with_whitespace(self, converter):
        """Test handling of trailing semicolon followed by whitespace."""
        # Arrange
        text = "         nd(n)%nod(m) = k; "
        expected = "         nd(n)%nod(m) = k"

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_fortran_do_loop_with_trailing_semicolon(self, converter):
        """Test real-world case: do loop with trailing semicolon in body."""
        # Arrange
        text = """do m = 1, nn
         k = netcell(n)%NOD(m)
         nd(n)%x(m) = xk(k)
         nd(n)%y(m) = yk(k)
         nd(n)%nod(m) = k; 
      end do"""
        expected = """do m = 1, nn
         k = netcell(n)%NOD(m)
         nd(n)%x(m) = xk(k)
         nd(n)%y(m) = yk(k)
         nd(n)%nod(m) = k
      end do"""

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_ignore_preprocessor_define(self, converter):
        """Test that preprocessor #define directives are not converted."""
        # Arrange
        text = "#define no_warning_unused_dummy_argument(x) associate( x => x ); end associate"
        expected = text

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert not was_converted
        assert result == expected

    def test_ignore_preprocessor_with_indentation(self, converter):
        """Test that indented preprocessor directives are not converted."""
        # Arrange
        text = "    #ifdef DEBUG\n    #define LOG(x) print *, x; flush(6)\n    #endif"
        expected = text

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert not was_converted
        assert result == expected

    def test_check_ignores_preprocessor(self, converter):
        """Test that check mode ignores preprocessor directives."""
        # Arrange
        text = "#define MACRO(x) a = 1; b = 2"

        # Act
        issues = converter.check_text(text)

        # Assert
        assert len(issues) == 0

    def test_mixed_preprocessor_and_code(self, converter):
        """Test file with both preprocessor directives and regular code."""
        # Arrange
        text = """#define MACRO(x) a = x; b = x
program test
    x = 1; y = 2
end program"""
        expected = """#define MACRO(x) a = x; b = x
program test
    x = 1
    y = 2
end program"""

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_real_fortran_example(self, converter):
        """Test with realistic Fortran code example."""
        # Arrange
        text = """    integer :: i, j
    i = 0; j = 0
    call init(); call process()
    x = 1.0"""
        expected = """    integer :: i, j
    i = 0
    j = 0
    call init()
    call process()
    x = 1.0"""

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected
