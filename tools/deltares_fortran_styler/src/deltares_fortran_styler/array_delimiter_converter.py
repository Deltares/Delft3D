"""
array_delimiter_converter.py - Converter for Fortran array delimiters

This converter handles standardizing array constructor delimiters:
- Converts old-style (/ ... /) to modern [...]
- Example: (/ 1, 2, 3 /) -> [1, 2, 3]

This follows modern Fortran standards (Fortran 2003+) which prefer square brackets
for array constructors.
"""

import re
from typing import Tuple, List

try:
    from deltares_fortran_styler.base_converter import FortranConverter, ConversionIssue
except ImportError:
    from base_converter import FortranConverter, ConversionIssue


class ArrayDelimiterConverter(FortranConverter):
    """Converter for Fortran array constructor delimiters."""

    def __init__(self):
        # Pattern to match old-style array constructors (/.../)
        # This needs to handle nested parentheses and various content
        self.old_array_pattern = re.compile(
            r'\(\s*/\s*',  # Opening (/ with optional whitespace
            re.VERBOSE
        )

        # Pattern to find closing /)
        self.closing_pattern = re.compile(r'\s*/\s*\)')

    def get_name(self) -> str:
        return "ArrayDelimiterConverter"

    def needs_import(self) -> bool:
        return False

    def _find_matching_closing(self, text: str, start_pos: int) -> int:
        """
        Find the matching /) for a (/ at start_pos.

        Returns the position of the closing ), or -1 if not found.
        Handles nested parentheses correctly.
        """
        paren_depth = 1  # We've already seen the opening (
        i = start_pos

        while i < len(text) and paren_depth > 0:
            if self._is_in_string_or_comment(text, i):
                i += 1
                continue

            char = text[i]

            if char == '(':
                paren_depth += 1
                i += 1
            elif char == ')':
                paren_depth -= 1
                if paren_depth == 0:
                    # Found potential closing - check if preceded by /
                    if i > 0 and text[i - 1] == '/':
                        return i  # Position of closing )
                    else:
                        # Just a regular closing paren, not part of /)
                        i += 1
                else:
                    i += 1
            else:
                i += 1

        return -1  # No matching closing found

    def convert_text(self, text: str) -> Tuple[str, bool]:
        """Convert old-style array constructors to new square bracket style."""
        result = []
        i = 0
        conversions_made = False

        while i < len(text):
            # Look for (/ pattern
            match = self.old_array_pattern.match(text, i)

            if match and not self._is_in_string_or_comment(text, match.start()):
                # Found an opening (/ - find the matching /)
                closing_pos = self._find_matching_closing(text, match.end())

                if closing_pos != -1:
                    # Successfully found matching closing
                    # Extract the content between (/ and /)
                    content_start = match.end()
                    content_end = closing_pos - 1  # Position of / before )
                    content = text[content_start:content_end]

                    # Convert to square brackets
                    result.append('[')
                    result.append(content.rstrip())
                    result.append(']')

                    conversions_made = True
                    i = closing_pos + 1
                    continue

            # No match or couldn't convert, keep original character
            result.append(text[i])
            i += 1

        converted_text = ''.join(result)
        return converted_text, conversions_made

    def check_text(self, text: str) -> List[ConversionIssue]:
        """Check for old-style array constructor delimiters."""
        issues = []

        i = 0
        while i < len(text):
            match = self.old_array_pattern.match(text, i)

            if match and not self._is_in_string_or_comment(text, match.start()):
                # Found an opening (/ - verify it has matching closing
                closing_pos = self._find_matching_closing(text, match.end())

                if closing_pos != -1:
                    line_num = text[:match.start()].count('\n') + 1
                    # Extract a snippet for display
                    snippet_end = min(match.start() + 20, closing_pos + 1)
                    snippet = text[match.start():snippet_end]
                    if snippet_end < closing_pos + 1:
                        snippet += "..."

                    issues.append(ConversionIssue(
                        line_number=line_num,
                        error_code="STYLE004",
                        message="Old-style array constructor found: should use [...] instead of (/ ... /)",
                        original_text=snippet
                    ))
                    i = closing_pos + 1
                    continue

            i += 1

        return issues

    def add_required_imports(self, text: str, was_converted: bool) -> str:
        """No imports needed for array delimiter conversion."""
        return text

    def get_conversion_stats(self, original_text: str) -> dict:
        """Get statistics about conversions that would be made."""
        count = 0
        i = 0

        while i < len(original_text):
            match = self.old_array_pattern.match(original_text, i)

            if match and not self._is_in_string_or_comment(original_text, match.start()):
                closing_pos = self._find_matching_closing(original_text, match.end())
                if closing_pos != -1:
                    count += 1
                    i = closing_pos + 1
                    continue

            i += 1

        return {'array_constructors': count}
