"""
single_line_if_converter.py - Converter for Fortran single-line if statements

This converter handles:
- Converting single-line if statements to multi-line then/end if format
- Example: if (.true.) call my_subroutine() -> if (.true.) then\n   call my_subroutine()\nend if

This follows modern Fortran style guidelines for better readability and maintainability.
"""

import re
from typing import Tuple, List

try:
    from deltares_fortran_styler.base_converter import FortranConverter, ConversionIssue
except ImportError:
    from base_converter import FortranConverter, ConversionIssue


class SingleLineIfConverter(FortranConverter):
    """Converter for single-line if statements to multi-line then/end if format."""

    def __init__(self):
        # Pattern to match the beginning of an if statement
        # We'll manually parse the condition to handle nested parentheses
        # Use [ \t] instead of \s to avoid matching newlines in the indent
        self.if_start_pattern = re.compile(
            r'^(?P<indent>[ \t]*)if\s*\(',
            re.MULTILINE | re.IGNORECASE
        )

    def get_name(self) -> str:
        return "SingleLineIfConverter"

    def needs_import(self) -> bool:
        return False

    def _remove_line_continuations(self, text: str) -> str:
        """
        Remove Fortran line continuations (&) and join continued lines.

        Args:
            text: Text potentially containing line continuations

        Returns:
            Text with continuations resolved into single logical lines
        """
        lines = text.split('\n')
        result_lines = []
        i = 0

        while i < len(lines):
            line = lines[i]

            # Check if line ends with & (continuation character)
            stripped = line.rstrip()
            if stripped.endswith('&'):
                # Start building the continued line
                # Remove the trailing & and any comment after it
                base_line = stripped.rstrip('&').rstrip()

                # Look ahead for continuation lines
                i += 1
                while i < len(lines):
                    next_line = lines[i]
                    next_stripped = next_line.lstrip()

                    # Remove leading & if present (optional in Fortran)
                    if next_stripped.startswith('&'):
                        next_stripped = next_stripped[1:].lstrip()

                    # Check if this line also continues
                    if next_stripped.rstrip().endswith('&'):
                        # Remove trailing & and continue
                        next_stripped = next_stripped.rstrip().rstrip('&').rstrip()
                        base_line += ' ' + next_stripped
                        i += 1
                    else:
                        # This is the last continuation line
                        base_line += ' ' + next_stripped
                        i += 1
                        break

                result_lines.append(base_line)
            else:
                result_lines.append(line)
                i += 1

        return '\n'.join(result_lines)

    def _find_balanced_parentheses(self, text: str, start_pos: int) -> int:
        """
        Find the closing parenthesis that matches the opening one at start_pos.
        This handles line continuations within the condition.

        Args:
            text: The full text
            start_pos: Position of the opening parenthesis

        Returns:
            Position of the matching closing parenthesis, or -1 if not found
        """
        if start_pos >= len(text) or text[start_pos] != '(':
            return -1

        depth = 1
        i = start_pos + 1

        while i < len(text) and depth > 0:
            if self._is_in_string_or_comment(text, i):
                i += 1
                continue

            # Skip line continuation characters
            if text[i] == '&':
                # Skip the & and any whitespace/newlines until next content
                i += 1
                while i < len(text) and text[i] in ' \t\n':
                    i += 1
                # Skip optional leading & on continuation line
                if i < len(text) and text[i] == '&':
                    i += 1
                    while i < len(text) and text[i] in ' \t':
                        i += 1
                continue

            if text[i] == '(':
                depth += 1
            elif text[i] == ')':
                depth -= 1
                if depth == 0:
                    return i
            i += 1

        return -1

    def _extract_condition(self, text: str, paren_start: int, paren_end: int) -> str:
        """
        Extract and clean up the condition from within parentheses.
        Removes line continuations and normalizes whitespace.

        Args:
            text: The full text
            paren_start: Position of opening parenthesis
            paren_end: Position of closing parenthesis

        Returns:
            Cleaned condition string
        """
        raw_condition = text[paren_start + 1:paren_end]

        # Remove line continuations from the condition
        lines = raw_condition.split('\n')
        cleaned_parts = []

        for line in lines:
            # Remove leading/trailing whitespace
            stripped = line.strip()

            # Remove continuation characters
            if stripped.endswith('&'):
                stripped = stripped[:-1].rstrip()
            if stripped.startswith('&'):
                stripped = stripped[1:].lstrip()

            if stripped:
                cleaned_parts.append(stripped)

        return ' '.join(cleaned_parts)

    def _parse_if_statement(self, text: str, start_pos: int):
        """
        Parse an if statement starting at start_pos.

        Returns:
            Tuple of (indent, condition, statement, condition_comment, end_pos) or None if not a single-line if
        """
        # Find the line start to get indentation
        line_start = text.rfind('\n', 0, start_pos) + 1

        # Find the opening parenthesis
        match = self.if_start_pattern.match(text, line_start)
        if not match:
            return None

        indent = match.group('indent')
        paren_start = match.end() - 1  # Position of '('

        # Find the matching closing parenthesis
        paren_end = self._find_balanced_parentheses(text, paren_start)
        if paren_end == -1:
            return None

        # Extract condition and clean up continuations
        condition = self._extract_condition(text, paren_start, paren_end)

        # Check what comes after the closing parenthesis
        after_paren = paren_end + 1

        # Skip whitespace
        while after_paren < len(text) and text[after_paren] in ' \t':
            after_paren += 1

        # Check for continuation character and comment after condition
        condition_comment = ""
        if after_paren < len(text) and text[after_paren] == '&':
            # Look for a comment on this line after the &
            line_end = text.find('\n', after_paren)
            if line_end == -1:
                line_end = len(text)

            rest_of_line = text[after_paren + 1:line_end].lstrip()
            if rest_of_line.startswith('!'):
                condition_comment = ' ' + rest_of_line

            after_paren = line_end + 1  # Move to start of next line

            # Skip whitespace and optional leading & on continuation line
            while after_paren < len(text) and text[after_paren] in ' \t':
                after_paren += 1
            if after_paren < len(text) and text[after_paren] == '&':
                after_paren += 1
                while after_paren < len(text) and text[after_paren] in ' \t':
                    after_paren += 1

        # Check if it's already a then statement
        if after_paren < len(text) - 3:
            next_word = text[after_paren:after_paren + 4].lower()
            if next_word == 'then':
                return None  # Already multi-line format

        # Find the end of the logical line, accounting for continuations
        # Start from after_paren and look for statement
        current_pos = after_paren
        statement_parts = []

        while current_pos < len(text):
            line_end = text.find('\n', current_pos)
            if line_end == -1:
                line_end = len(text)

            # Get the rest of this physical line
            line_content = text[current_pos:line_end]

            # Check if line ends with continuation character & (before any comment)
            # First, strip the line and check for comments
            stripped_line = line_content.rstrip()

            # Find if there's a comment (not in a string)
            comment_pos = -1
            in_string = False
            string_char = None
            for i, char in enumerate(stripped_line):
                if char in ('"', "'"):
                    if not in_string:
                        in_string = True
                        string_char = char
                    elif char == string_char:
                        in_string = False
                elif char == '!' and not in_string:
                    comment_pos = i
                    break

            # Check for continuation before the comment
            has_continuation = False
            code_part = stripped_line

            if comment_pos >= 0:
                code_part = stripped_line[:comment_pos].rstrip()

            if code_part.endswith('&'):
                has_continuation = True
                code_part = code_part.rstrip('&').rstrip()

            if has_continuation:
                # Don't include the comment from the continuation line
                if code_part:
                    statement_parts.append(code_part)
                current_pos = line_end + 1  # Move to next line

                # Skip leading & on next line if present
                if current_pos < len(text):
                    # Skip whitespace and optional leading &
                    while current_pos < len(text) and text[current_pos] in ' \t':
                        current_pos += 1
                    if current_pos < len(text) and text[current_pos] == '&':
                        current_pos += 1
                        # Skip more whitespace after &
                        while current_pos < len(text) and text[current_pos] in ' \t':
                            current_pos += 1
            else:
                # No continuation, this is the end of the statement
                # Include the comment if it's on the final line
                statement_parts.append(stripped_line)
                # Return the position at the end of this line
                return (indent, condition, ' '.join(statement_parts).strip(), condition_comment, line_end)

        # If we get here, statement extends to end of text
        statement = ' '.join(statement_parts).strip()
        if not statement:
            return None  # No statement, not a single-line if

        return (indent, condition, statement, condition_comment, len(text))

    def _convert_single_if(self, indent: str, condition: str, statement: str, condition_comment: str = "") -> str:
        """Convert a single-line if statement to multi-line format."""
        # Use 3 spaces for the inner indentation (common Fortran style)
        inner_indent = indent + "   "

        result = f"{indent}if ({condition}) then{condition_comment}\n"
        result += f"{inner_indent}{statement}\n"
        result += f"{indent}end if"

        return result

    def convert_text(self, text: str) -> Tuple[str, bool]:
        """Convert single-line if statements to multi-line then/end if format."""
        conversions_made = False
        result = []
        i = 0

        # Use regex to find potential if statements instead of checking every character
        for match in self.if_start_pattern.finditer(text):
            match_start = match.start()

            # Append everything from current position up to (but not including) the match
            chunk = text[i:match_start]
            result.append(chunk)

            # Check if this match is in a string or comment
            if self._is_in_string_or_comment(text, match_start):
                # Keep the matched portion as-is and continue searching
                result.append(text[match_start:match.end()])
                i = match.end()
                continue

            # Try to parse the if statement
            parsed = self._parse_if_statement(text, match_start)

            if parsed:
                indent, condition, statement, condition_comment, end_pos = parsed

                # Convert to multi-line format
                converted = self._convert_single_if(indent, condition, statement, condition_comment)
                result.append(converted)

                conversions_made = True
                i = end_pos
            else:
                # Couldn't parse (e.g., already multi-line if-then), keep original matched part
                result.append(text[match_start:match.end()])
                i = match.end()

        # Append any remaining text after the last match
        final_chunk = text[i:]
        result.append(final_chunk)

        return ''.join(result), conversions_made

    def check_text(self, text: str) -> List[ConversionIssue]:
        """Check for single-line if statements without modifying text."""
        issues = []

        # Use regex to find potential if statements instead of checking every character
        for match in self.if_start_pattern.finditer(text):
            match_start = match.start()

            # Check if this match is in a string or comment
            if self._is_in_string_or_comment(text, match_start):
                continue

            # Try to parse the if statement
            parsed = self._parse_if_statement(text, match_start)

            if parsed:
                indent, condition, statement, condition_comment, end_pos = parsed

                line_num = text[:match_start].count('\n') + 1
                line_start = text.rfind('\n', 0, match_start) + 1
                line_end = text.find('\n', match_start)
                if line_end == -1:
                    line_end = len(text)
                original_line = text[line_start:line_end]

                issues.append(ConversionIssue(
                    line_number=line_num,
                    error_code="STYLE006",
                    message="Single-line if statement found: should use 'if (...) then' / 'end if' format",
                    original_text=original_line.strip()
                ))

        return issues

    def add_required_imports(self, text: str, was_converted: bool) -> str:
        """No imports needed for single-line if conversion."""
        return text

    def get_conversion_stats(self, original_text: str) -> dict:
        """Get statistics about conversions that would be made."""
        count = 0

        # Use regex to find potential if statements instead of checking every character
        for match in self.if_start_pattern.finditer(original_text):
            match_start = match.start()

            # Check if this match is in a string or comment
            if self._is_in_string_or_comment(original_text, match_start):
                continue

            # Try to parse the if statement
            parsed = self._parse_if_statement(original_text, match_start)

            if parsed:
                count += 1

        return {'single_line_ifs': count}
