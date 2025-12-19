"""
semicolon_separator_converter.py - Converter for semicolon-separated statements

This converter ensures that semicolon-separated statements are placed on separate lines:
- Converts statements like "a = 1; b = 2" to separate lines
- Example: "a = 1; b = 2" becomes:
  a = 1
  b = 2

This follows the Deltares style guide which requires each statement on a new line.
Semicolons are removed and statements are properly indented.
"""

import re
from typing import Tuple, List

try:
    from deltares_fortran_styler.base_converter import FortranConverter, ConversionIssue
except ImportError:
    from base_converter import FortranConverter, ConversionIssue


class SemicolonSeparatorConverter(FortranConverter):
    """Converter for splitting semicolon-separated statements onto separate lines."""

    def get_name(self) -> str:
        return "SemicolonSeparatorConverter"

    def needs_import(self) -> bool:
        return False

    def _get_line_indentation(self, line: str) -> str:
        """Extract the indentation (leading whitespace) from a line."""
        match = re.match(r'^(\s*)', line)
        return match.group(1) if match else ''

    def convert_text(self, text: str) -> Tuple[str, bool]:
        """Split semicolon-separated statements onto separate lines."""
        lines = text.split('\n')
        result_lines = []
        conversions_made = False

        for line_num, line in enumerate(lines):
            # Skip preprocessor directives (lines starting with #)
            if line.lstrip().startswith('#'):
                result_lines.append(line)
                continue

            # Check if line contains semicolons
            if ';' not in line:
                result_lines.append(line)
                continue

            # Parse the line character by character to handle strings and comments
            segments = []
            current_segment = []
            in_single_quote = False
            in_double_quote = False
            in_comment = False
            found_semicolon = False
            i = 0

            while i < len(line):
                char = line[i]

                # Handle comment - rest of line is comment
                if char == '!' and not in_single_quote and not in_double_quote:
                    in_comment = True
                    current_segment.append(char)
                    i += 1
                    continue

                # If in comment, just add the character
                if in_comment:
                    current_segment.append(char)
                    i += 1
                    continue

                # Handle string literals
                if char == "'" and not in_double_quote:
                    # Check for doubled single quote (escape)
                    if i + 1 < len(line) and line[i + 1] == "'":
                        current_segment.append(char)
                        current_segment.append(line[i + 1])
                        i += 2
                        continue
                    in_single_quote = not in_single_quote
                    current_segment.append(char)
                    i += 1
                    continue

                if char == '"' and not in_single_quote:
                    # Check for doubled double quote (escape)
                    if i + 1 < len(line) and line[i + 1] == '"':
                        current_segment.append(char)
                        current_segment.append(line[i + 1])
                        i += 2
                        continue
                    in_double_quote = not in_double_quote
                    current_segment.append(char)
                    i += 1
                    continue

                # Handle semicolon (only outside strings and comments)
                if char == ';' and not in_single_quote and not in_double_quote:
                    # Mark that we found a semicolon
                    found_semicolon = True
                    # Save current segment
                    segment_text = ''.join(current_segment).strip()
                    if segment_text:
                        segments.append(segment_text)
                    current_segment = []
                    i += 1
                    continue

                # Regular character
                current_segment.append(char)
                i += 1

            # Add final segment (including any comment)
            segment_text = ''.join(current_segment).strip()
            if segment_text:
                segments.append(segment_text)

            # If we found any semicolons, we need to convert
            if found_semicolon:
                conversions_made = True
                # Get original indentation
                indent = self._get_line_indentation(line)

                # Add each segment as a separate line with same indentation
                for seg in segments:
                    result_lines.append(indent + seg)
            else:
                # No semicolons found
                result_lines.append(line)

        converted_text = '\n'.join(result_lines)
        return converted_text, conversions_made

    def check_text(self, text: str) -> List[ConversionIssue]:
        """Check for semicolon-separated statements."""
        issues = []
        lines = text.split('\n')

        for line_num, line in enumerate(lines, 1):
            # Skip preprocessor directives (lines starting with #)
            if line.lstrip().startswith('#'):
                continue

            # Quick check if line has semicolons
            if ';' not in line:
                continue

            # Parse line to find semicolons outside strings and comments
            in_single_quote = False
            in_double_quote = False
            in_comment = False
            semicolons_found = []

            i = 0
            while i < len(line):
                char = line[i]

                # Handle comment
                if char == '!' and not in_single_quote and not in_double_quote:
                    in_comment = True
                    i += 1
                    continue

                if in_comment:
                    i += 1
                    continue

                # Handle strings
                if char == "'" and not in_double_quote:
                    if i + 1 < len(line) and line[i + 1] == "'":
                        i += 2
                        continue
                    in_single_quote = not in_single_quote
                    i += 1
                    continue

                if char == '"' and not in_single_quote:
                    if i + 1 < len(line) and line[i + 1] == '"':
                        i += 2
                        continue
                    in_double_quote = not in_double_quote
                    i += 1
                    continue

                # Check for semicolon outside strings/comments
                if char == ';' and not in_single_quote and not in_double_quote:
                    semicolons_found.append(i)

                i += 1

            # If we found semicolons, create an issue
            if semicolons_found:
                snippet = line.strip()
                if len(snippet) > 60:
                    snippet = snippet[:57] + "..."

                issues.append(ConversionIssue(
                    line_number=line_num,
                    error_code="STYLE005",
                    message="Semicolon-separated statements found: each statement should be on a separate line",
                    original_text=snippet
                ))

        return issues

    def add_required_imports(self, text: str, was_converted: bool) -> str:
        """No imports needed for semicolon separator conversion."""
        return text

    def get_conversion_stats(self, original_text: str) -> dict:
        """Get statistics about conversions that would be made."""
        count = 0
        lines = original_text.split('\n')

        for line in lines:
            # Skip preprocessor directives
            if line.lstrip().startswith('#'):
                continue

            if ';' not in line:
                continue

            # Count semicolons outside strings and comments
            in_single_quote = False
            in_double_quote = False
            in_comment = False

            for i, char in enumerate(line):
                if char == '!' and not in_single_quote and not in_double_quote:
                    in_comment = True
                    continue

                if in_comment:
                    continue

                if char == "'" and not in_double_quote:
                    if i + 1 < len(line) and line[i + 1] == "'":
                        continue
                    in_single_quote = not in_single_quote
                    continue

                if char == '"' and not in_single_quote:
                    if i + 1 < len(line) and line[i + 1] == '"':
                        continue
                    in_double_quote = not in_double_quote
                    continue

                if char == ';' and not in_single_quote and not in_double_quote:
                    count += 1

        return {'semicolon_separators': count}
