"""
base_converter.py - Base class and interfaces for Fortran style converters

This module defines the abstract base class that all style converters must implement.
Each converter should be independent and handle a specific aspect of Fortran code styling.
"""

from abc import ABC, abstractmethod
from typing import Tuple, List

class ConversionIssue:
    """Represents a style issue found during checking."""

    def __init__(self, line_number: int, error_code: str, message: str, original_text: str):
        self.line_number = line_number
        self.error_code = error_code
        self.message = message
        self.original_text = original_text

class FortranConverter(ABC):
    """
    Abstract base class for Fortran style converters.

    Each converter should:
    1. Be independent and focused on one aspect of style
    2. Be able to check for issues without modifying text
    3. Be able to convert text with the appropriate changes
    4. Report what was changed for logging purposes
    """

    @abstractmethod
    def get_name(self) -> str:
        """Return the name of this converter for logging/reporting."""
        pass

    @abstractmethod
    def convert_text(self, text: str) -> Tuple[str, bool]:
        """
        Convert the given text according to this converter's rules.

        Args:
            text: The input Fortran source code

        Returns:
            Tuple of (converted_text, was_modified)
        """
        pass

    @abstractmethod
    def check_text(self, text: str) -> List[ConversionIssue]:
        """
        Check the given text for style issues without modifying it.

        Args:
            text: The input Fortran source code

        Returns:
            List of ConversionIssue objects describing what needs to be fixed
        """
        pass

    @abstractmethod
    def needs_import(self) -> bool:
        """
        Return whether this converter requires adding import statements.

        Returns:
            True if this converter may add imports to the file
        """
        pass

    @abstractmethod
    def add_required_imports(self, text: str, was_converted: bool) -> str:
        """
        Add any required imports to the text if conversions were made.

        Args:
            text: The converted text
            was_converted: Whether any conversions were performed

        Returns:
            Text with imports added if necessary
        """
        pass

    def _is_in_string_or_comment(self, text: str, pos: int) -> bool:
        """
        Check if position is inside a string literal or comment.

        This is a common utility method that converters can use or override.
        """
        # Check for comments first (everything after ! to end of line)
        lines = text[:pos].split('\n')
        current_line = lines[-1] if lines else ""
        comment_pos = current_line.find('!')
        if comment_pos != -1:
            pos_in_line = len(current_line)
            if pos_in_line > comment_pos:
                return True

        # Check for string literals (both single and double quotes)
        # This is a simplified check - could be enhanced for Fortran-specific strings
        in_single_quote = False
        in_double_quote = False
        i = 0
        line_start = text.rfind('\n', 0, pos) + 1

        # Start from beginning of current line
        while i < pos:
            if i < line_start:
                i = line_start
                continue

            char = text[i]

            # Check for comment on this line
            if char == '!' and not in_single_quote and not in_double_quote:
                # Rest of line is comment
                line_end = text.find('\n', i)
                if line_end == -1:
                    line_end = len(text)
                if pos <= line_end:
                    return True
                i = line_end + 1
                in_single_quote = False
                in_double_quote = False
                continue

            if char == "'" and not in_double_quote:
                # Check for doubled single quote (escape)
                if i + 1 < len(text) and text[i + 1] == "'":
                    i += 2
                    continue
                in_single_quote = not in_single_quote
            elif char == '"' and not in_single_quote:
                # Check for doubled double quote (escape)
                if i + 1 < len(text) and text[i + 1] == '"':
                    i += 2
                    continue
                in_double_quote = not in_double_quote

            i += 1

        return in_single_quote or in_double_quote
