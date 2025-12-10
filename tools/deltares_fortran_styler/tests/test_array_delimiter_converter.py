"""Tests for the array delimiter converter."""
import pytest
from deltares_fortran_styler.array_delimiter_converter import ArrayDelimiterConverter

class TestArrayDelimiterConverter:
    """Test suite for ArrayDelimiterConverter."""

    @pytest.fixture
    def converter(self):
        """Create a converter instance for testing."""
        return ArrayDelimiterConverter()

    def test_convert_simple_array(self, converter):
        """Test conversion of simple array constructor."""
        # Arrange
        text = "x = (/ 1, 2, 3 /)"
        expected = "x = [1, 2, 3]"

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_convert_array_no_spaces(self, converter):
        """Test conversion of array constructor without spaces around delimiters."""
        # Arrange
        text = "x = (/1, 2, 3/)"
        expected = "x = [1, 2, 3]"

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_convert_array_with_extra_spaces(self, converter):
        """Test conversion preserves internal spacing."""
        # Arrange
        text = "x = (/  1,  2,  3  /)"
        expected = "x = [1,  2,  3]"

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_convert_nested_array(self, converter):
        """Test conversion of array with nested parentheses."""
        # Arrange
        text = "x = (/ func(a, b), func(c, d) /)"
        expected = "x = [func(a, b), func(c, d)]"

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_convert_multiline_array(self, converter):
        """Test conversion of multiline array constructor."""
        # Arrange
        text = """x = (/ 1, 2, &
     3, 4 /)"""
        expected = """x = [1, 2, &
     3, 4]"""

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_no_conversion_needed(self, converter):
        """Test that already converted arrays are not modified."""
        # Arrange
        text = "x = [1, 2, 3]"
        expected = text

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert not was_converted
        assert result == expected

    def test_ignore_array_in_comment(self, converter):
        """Test that arrays in comments are not converted."""
        # Arrange
        text = "! Old syntax: (/ 1, 2, 3 /)"
        expected = text

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert not was_converted
        assert result == expected

    def test_ignore_array_in_string(self, converter):
        """Test that arrays in strings are not converted."""
        # Arrange
        text = "message = 'Array: (/ 1, 2, 3 /)'"
        expected = text

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert not was_converted
        assert result == expected

    def test_convert_multiple_arrays_on_same_line(self, converter):
        """Test conversion of multiple array constructors on one line."""
        # Arrange
        text = "x = (/ 1, 2 /); y = (/ 3, 4 /)"
        expected = "x = [1, 2]; y = [3, 4]"

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_convert_real_literals_in_array(self, converter):
        """Test conversion of array with real number literals."""
        # Arrange
        text = "x = (/ 1.0, 2.5, 3.7 /)"
        expected = "x = [1.0, 2.5, 3.7]"

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_convert_expressions_in_array(self, converter):
        """Test conversion of array with arithmetic expressions."""
        # Arrange
        text = "x = (/ a + b, c * d, e / f /)"
        expected = "x = [a + b, c * d, e / f]"

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_check_text_returns_issues(self, converter):
        """Test that check_text identifies old-style arrays."""
        # Arrange
        text = """x = (/ 1, 2, 3 /)
y = (/ 4, 5 /)"""

        # Act
        issues = converter.check_text(text)

        # Assert
        assert len(issues) == 2
        assert all(issue.error_code == "STYLE004" for issue in issues)

    def test_get_conversion_stats(self, converter):
        """Test that conversion statistics are correctly computed."""
        # Arrange
        text = """x = (/ 1, 2 /)
y = (/ 3, 4 /)
z = (/ 5, 6, 7 /)"""

        # Act
        stats = converter.get_conversion_stats(text)

        # Assert
        assert stats['array_constructors'] == 3

    def test_deeply_nested_parentheses(self, converter):
        """Test conversion with deeply nested function calls."""
        # Arrange
        text = "x = (/ func(a(i, j), b(k, l)), c /)"
        expected = "x = [func(a(i, j), b(k, l)), c]"

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_empty_array(self, converter):
        """Test conversion of empty array constructor."""
        # Arrange
        text = "x = (/ /)"
        expected = "x = []"

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_array_with_string_literals(self, converter):
        """Test conversion of array with string literals."""
        # Arrange
        text = "x = (/ 'a', 'b', 'c' /)"
        expected = "x = ['a', 'b', 'c']"

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_needs_import_returns_false(self, converter):
        """Test that array delimiter converter doesn't need imports."""
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

    def test_regular_paren_inside_array_constructor(self, converter):
        """Test array constructor with regular closing paren that's not part of /)."""
        # Arrange - this tests line 73 (i += 1 when paren_depth > 0 after decrement)
        text = "x = (/ func(), 2 /)"
        expected = "x = [func(), 2]"

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_unclosed_array_constructor(self, converter):
        """Test handling of unclosed array constructor (no matching /)."""
        # Arrange - this tests line 79 (return -1 when no closing found)
        text = "x = (/ 1, 2, 3"
        expected = text  # Should remain unchanged

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert not was_converted
        assert result == expected

    def test_check_long_array_constructor(self, converter):
        """Test check_text with long array that gets truncated in snippet."""
        # Arrange - this tests line 143 (snippet += "...")
        text = "x = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 /)"

        # Act
        issues = converter.check_text(text)

        # Assert
        assert len(issues) == 1
        assert issues[0].error_code == "STYLE004"
        assert "..." in issues[0].original_text  # Snippet should be truncated
