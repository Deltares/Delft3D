"""Tests for the single-line if statement converter."""
import pytest
from deltares_fortran_styler.single_line_if_converter import SingleLineIfConverter


class TestSingleLineIfConverter:
    """Test suite for SingleLineIfConverter."""

    @pytest.fixture
    def converter(self):
        """Create a converter instance for testing."""
        return SingleLineIfConverter()

    def test_convert_simple_if_call(self, converter):
        """Test conversion of simple if statement with subroutine call."""
        # Arrange
        text = "if (.true.) call my_subroutine()"
        expected = """if (.true.) then
   call my_subroutine()
end if"""

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_convert_if_with_condition(self, converter):
        """Test conversion of if statement with complex condition."""
        # Arrange
        text = "if (x > 0) y = x + 1"
        expected = """if (x > 0) then
   y = x + 1
end if"""

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_convert_if_with_indentation(self, converter):
        """Test that conversion preserves existing indentation."""
        # Arrange
        text = "   if (.true.) call my_subroutine()"
        expected = """   if (.true.) then
      call my_subroutine()
   end if"""

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_convert_if_with_nested_parentheses(self, converter):
        """Test conversion of if statement with nested parentheses in condition."""
        # Arrange
        text = "if (func(a, b) > 0) call another_func()"
        expected = """if (func(a, b) > 0) then
   call another_func()
end if"""

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_convert_if_with_logical_operators(self, converter):
        """Test conversion with complex logical conditions."""
        # Arrange
        text = "if (x > 0 .and. y < 10) z = x + y"
        expected = """if (x > 0 .and. y < 10) then
   z = x + y
end if"""

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_no_conversion_for_then_statement(self, converter):
        """Test that if-then statements are not converted."""
        # Arrange
        text = """if (.true.) then
   call my_subroutine()
end if"""
        expected = text

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert not was_converted
        assert result == expected

    def test_ignore_if_in_comment(self, converter):
        """Test that if statements in comments are not converted."""
        # Arrange
        text = "! if (.true.) call my_subroutine()"
        expected = text

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert not was_converted
        assert result == expected

    def test_ignore_if_in_string(self, converter):
        """Test that if statements in strings are not converted."""
        # Arrange
        text = "message = 'if (.true.) call my_subroutine()'"
        expected = text

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert not was_converted
        assert result == expected

    def test_convert_multiple_if_statements(self, converter):
        """Test conversion of multiple if statements in same text."""
        # Arrange
        text = """if (x > 0) call func1()
if (y < 0) call func2()"""
        expected = """if (x > 0) then
   call func1()
end if
if (y < 0) then
   call func2()
end if"""

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_convert_if_with_function_call_statement(self, converter):
        """Test conversion with function call as statement."""
        # Arrange
        text = "if (condition) result = compute(a, b, c)"
        expected = """if (condition) then
   result = compute(a, b, c)
end if"""

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_convert_if_with_array_assignment(self, converter):
        """Test conversion with array element assignment."""
        # Arrange
        text = "if (i > 0) array(i) = value"
        expected = """if (i > 0) then
   array(i) = value
end if"""

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_check_text_returns_issues(self, converter):
        """Test that check_text identifies single-line if statements."""
        # Arrange
        text = """if (.true.) call func1()
if (.false.) call func2()"""

        # Act
        issues = converter.check_text(text)

        # Assert
        assert len(issues) == 2
        assert all(issue.error_code == "STYLE006" for issue in issues)

    def test_get_conversion_stats(self, converter):
        """Test that conversion statistics are correctly computed."""
        # Arrange
        text = """if (x > 0) y = x
if (a < b) call func()
if (condition) then
   statement
end if
if (z == 0) w = 1"""

        # Act
        stats = converter.get_conversion_stats(text)

        # Assert
        # Should count only single-line ifs (3), not the multi-line one
        assert stats['single_line_ifs'] == 3

    def test_needs_import_returns_false(self, converter):
        """Test that single-line if converter doesn't need imports."""
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

    def test_convert_if_with_return_statement(self, converter):
        """Test conversion with return statement."""
        # Arrange
        text = "if (error) return"
        expected = """if (error) then
   return
end if"""

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_convert_if_with_cycle_statement(self, converter):
        """Test conversion with cycle statement."""
        # Arrange
        text = "if (skip_iteration) cycle"
        expected = """if (skip_iteration) then
   cycle
end if"""

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_convert_if_with_exit_statement(self, converter):
        """Test conversion with exit statement."""
        # Arrange
        text = "if (done) exit"
        expected = """if (done) then
   exit
end if"""

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_convert_if_case_insensitive(self, converter):
        """Test that conversion works with different cases."""
        # Arrange
        text = "IF (.TRUE.) CALL MY_SUBROUTINE()"
        # Note: The 'if', 'then', and 'end if' keywords are normalized to lowercase
        expected = """if (.TRUE.) then
   CALL MY_SUBROUTINE()
end if"""

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_complex_condition_with_functions(self, converter):
        """Test conversion with complex condition containing function calls."""
        # Arrange
        text = "if (allocated(array) .and. size(array) > 0) call process(array)"
        expected = """if (allocated(array) .and. size(array) > 0) then
   call process(array)
end if"""

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_preserve_inline_comment_after_statement(self, converter):
        """Test conversion preserves inline comments after statement."""
        # Arrange
        text = "if (.true.) call func() ! Important call"
        expected = """if (.true.) then
   call func() ! Important call
end if"""

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_multiline_program_with_mixed_if_statements(self, converter):
        """Test conversion in context of larger program."""
        # Arrange
        text = """program test
   integer :: x, y
   x = 5
   if (x > 0) y = x * 2
   if (y > 10) then
      print *, y
   end if
   if (x < 10) call cleanup()
end program test"""

        expected = """program test
   integer :: x, y
   x = 5
   if (x > 0) then
      y = x * 2
   end if
   if (y > 10) then
      print *, y
   end if
   if (x < 10) then
      call cleanup()
   end if
end program test"""

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_convert_if_with_line_continuation_ampersand(self, converter):
        """Test conversion of single-line if with line continuation using &."""
        # Arrange
        text = """if (.true.) &
& call my_subroutine()"""
        expected = """if (.true.) then
   call my_subroutine()
end if"""

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_convert_if_with_continuation_no_leading_ampersand(self, converter):
        """Test conversion where continuation line lacks leading & (optional)."""
        # Arrange
        text = """if (.true.) &
call my_subroutine()"""
        expected = """if (.true.) then
   call my_subroutine()
end if"""

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_convert_if_continuation_with_indentation(self, converter):
        """Test conversion preserves indentation with line continuation."""
        # Arrange
        text = """   if (.false.) &
   & call another_func()"""
        expected = """   if (.false.) then
      call another_func()
   end if"""

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_convert_if_continuation_with_complex_statement(self, converter):
        """Test conversion with continuation and complex statement."""
        # Arrange
        text = """if (x > 0 .and. y < 10) &
& result = compute(a, b, c)"""
        expected = """if (x > 0 .and. y < 10) then
   result = compute(a, b, c)
end if"""

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_convert_if_continuation_multiple_lines(self, converter):
        """Test conversion with multiple continuation lines."""
        # Arrange
        text = """if (condition1 .and. &
& condition2 .or. &
& condition3) &
& call process_data()"""
        expected = """if (condition1 .and. condition2 .or. condition3) then
   call process_data()
end if"""

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_no_conversion_for_multiline_if_with_continuation(self, converter):
        """Test that multi-line if-then with continuation is not converted."""
        # Arrange
        text = """if (condition1 .and. &
& condition2) then
   call my_subroutine()
end if"""
        expected = text

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert not was_converted
        assert result == expected

    def test_convert_if_continuation_in_condition(self, converter):
        """Test conversion where continuation is only in condition."""
        # Arrange
        text = """if (very_long_variable_name > 0 .and. &
    another_long_variable < 100) call func()"""
        expected = """if (very_long_variable_name > 0 .and. another_long_variable < 100) then
   call func()
end if"""

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_convert_if_continuation_with_inline_comment(self, converter):
        """Test conversion with continuation and inline comments."""
        # Arrange
        text = """if (.true.) & ! Check condition
& call my_subroutine() ! Execute action"""
        expected = """if (.true.) then ! Check condition
   call my_subroutine() ! Execute action
end if"""

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_mixed_if_statements_with_and_without_continuation(self, converter):
        """Test conversion of mixed if statements with and without continuations."""
        # Arrange
        text = """if (x > 0) call func1()
if (y < 0) &
& call func2()
if (z == 0) call func3()"""
        expected = """if (x > 0) then
   call func1()
end if
if (y < 0) then
   call func2()
end if
if (z == 0) then
   call func3()
end if"""

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_convert_if_continuation_with_array_operation(self, converter):
        """Test conversion with continuation and array operations."""
        # Arrange
        text = """if (allocated(data)) &
& data(:) = 0.0"""
        expected = """if (allocated(data)) then
   data(:) = 0.0
end if"""

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_convert_if_with_leading_blank_lines(self, converter):
        """Test conversion doesn't add extra blank lines when input has leading newlines."""
        # Arrange
        text = "\n      if (ndx == ndxjac .and. lnx == lnxjac) return\n"
        expected = "\n      if (ndx == ndxjac .and. lnx == lnxjac) then\n         return\n      end if\n"

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_convert_if_with_surrounding_blank_lines(self, converter):
        """Test conversion handles blank lines before and after statement correctly."""
        # Arrange
        text = "\n\n      if (condition) statement\n\n"
        expected = "\n\n      if (condition) then\n         statement\n      end if\n\n"

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected

    def test_convert_if_preserves_blank_lines_between_statements(self, converter):
        """Test that blank lines between statements are preserved."""
        # Arrange
        text = """if (a > 0) x = 1

if (b > 0) y = 2"""
        expected = """if (a > 0) then
   x = 1
end if

if (b > 0) then
   y = 2
end if"""

        # Act
        result, was_converted = converter.convert_text(text)

        # Assert
        assert was_converted
        assert result == expected
