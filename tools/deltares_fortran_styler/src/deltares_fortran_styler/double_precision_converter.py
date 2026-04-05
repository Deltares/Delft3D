"""
double_precision_converter.py - Converter for Fortran double precision literals and declarations

This converter handles:
1. Converting double precision literals from `1d0` to `1.0_dp` format
2. Converting variable declarations from `double precision :: var` to `real(kind=dp) :: var`
3. Converting dble() function calls from `dble(x)` to `real(x, kind=dp)`

Based on Fortran standard syntax for double precision literals.
"""

import re
from typing import Tuple, List

try:
    from deltares_fortran_styler.base_converter import FortranConverter, ConversionIssue
except ImportError:
    from base_converter import FortranConverter, ConversionIssue


class DoublePrecisionConverter(FortranConverter):
    """Converter for Fortran double precision literals and declarations."""

    def __init__(self):
        # Pattern to match Fortran double precision literals with D exponent
        # This matches patterns like: 1d0, 1.0d0, .5d0, 123d-5, 0.123d+10, etc.
        self.d_literal_pattern = re.compile(
            r'''
            (?<!\w)                     # Not preceded by word character (negative lookbehind)
            (?P<sign>[+-]?)             # Optional sign
            (?P<significand>
                (?:\.\d+)               # . digit-string (e.g., .5, .123)
                |                       # OR
                (?:\d+\.?\d*)           # digit-string . [digit-string] (e.g., 1.0, 1., 123.456)
                |                       # OR
                (?:\d+)                 # digit-string only (e.g., 123) - for cases like 123d0
            )
            [dD]                        # Exponent letter D or d
            (?P<exponent>[+-]?\d+)      # Signed exponent
            (?!_)                       # Not followed by underscore (avoid already converted)
            (?!\w)                      # Not followed by word character (negative lookahead)
            ''',
            re.VERBOSE
        )

        # Pattern to match double precision variable declarations
        self.double_precision_pattern = re.compile(
            r'''
            (?P<indent>[ \t]*)          # Capture leading whitespace (spaces and tabs only, not newlines)
            double\s+precision          # "double precision" keywords
            (?P<rest>.*)                # Everything else on the line
            ''',
            re.VERBOSE | re.IGNORECASE
        )

        # Pattern to match dble() function calls
        self.dble_pattern = re.compile(
            r'\bdble\s*\(',
            re.IGNORECASE
        )

        # Pattern to find module or submodule declaration (but not module subroutine/function/procedure)
        # submodule declarations can be: submodule(parent) name or submodule (parent) name
        self.module_pattern = re.compile(
            r'^\s*(?:submodule\s*\([^)]+\)\s*\w+|module\s+(?!subroutine\b|function\b|procedure\b)\w+)',
            re.MULTILINE | re.IGNORECASE
        )

        # Pattern to find existing precision import
        self.precision_import_pattern = re.compile(
            r'^\s*use\s+precision\b',
            re.MULTILINE | re.IGNORECASE
        )

        # Pattern to find the end of use statements section
        self.use_section_end_pattern = re.compile(
            r'^\s*(?:implicit\s+none|contains|private|public|integer|real|character|logical|type|interface)\s*',
            re.MULTILINE | re.IGNORECASE
        )

    def get_name(self) -> str:
        return "DoublePrecisionConverter"

    def needs_import(self) -> bool:
        return True

    def _convert_literal(self, match) -> str:
        """Convert a single D literal to _dp format."""
        sign = match.group('sign')
        significand = match.group('significand')
        exponent = match.group('exponent')

        # Handle significand formatting
        if '.' not in significand:
            # Add .0 if no decimal point (e.g., 1d0 -> 1.0)
            significand = significand + '.0'
        elif significand.endswith('.'):
            # Add 0 after trailing decimal (e.g., 1. -> 1.0)
            significand = significand + '0'
        elif significand.startswith('.'):
            # Add 0 before leading decimal (e.g., .5 -> 0.5)
            significand = '0' + significand

        # Convert D exponent to E exponent and add _dp kind
        if exponent == '0':
            # Simple case: no exponent needed
            return f"{sign}{significand}_dp"
        else:
            # Include exponent with E
            return f"{sign}{significand}e{exponent}_dp"

    def _convert_double_precision_declaration(self, match) -> str:
        """Convert a double precision declaration to real(kind=dp) format."""
        indent = match.group('indent')
        rest = match.group('rest').strip()
        # Add space only if rest doesn't start with a comma or other punctuation
        separator = '' if rest.startswith(',') else ' '
        return f"{indent}real(kind=dp){separator}{rest}"

    def _convert_dble_calls(self, text: str) -> str:
        """Convert dble(expr) to real(expr, kind=dp)."""
        result = []
        i = 0
        while i < len(text):
            # Check if we're at the start of a dble call
            match = self.dble_pattern.match(text, i)
            if match and not self._is_in_string_or_comment(text, match.start()):
                # Found dble( - need to find matching closing parenthesis
                paren_start = match.end() - 1  # Position of opening (
                paren_count = 1
                j = paren_start + 1

                # Find the matching closing parenthesis
                while j < len(text) and paren_count > 0:
                    if text[j] == '(' and not self._is_in_string_or_comment(text, j):
                        paren_count += 1
                    elif text[j] == ')' and not self._is_in_string_or_comment(text, j):
                        paren_count -= 1
                    j += 1

                if paren_count == 0:
                    # Successfully found matching parenthesis
                    inner_expr = text[paren_start + 1:j - 1]
                    result.append(f"real({inner_expr}, kind=dp)")
                    i = j
                else:
                    # Unmatched parenthesis, keep original
                    result.append(text[i])
                    i += 1
            else:
                result.append(text[i])
                i += 1

        return ''.join(result)

    def convert_text(self, text: str) -> Tuple[str, bool]:
        """Convert all D literals and double precision declarations in the given text."""
        original_text = text
        conversions_made = False

        # Convert D literals
        text_for_literal_check = text
        def replace_literal_match(match):
            if self._is_in_string_or_comment(text_for_literal_check, match.start()):
                return match.group(0)  # Return unchanged
            return self._convert_literal(match)

        text = self.d_literal_pattern.sub(replace_literal_match, text)
        if text != original_text:
            conversions_made = True

        # Convert double precision declarations
        text_for_declaration_check = text
        def replace_declaration_match(match):
            if self._is_in_string_or_comment(text_for_declaration_check, match.start()):
                return match.group(0)  # Return unchanged
            return self._convert_double_precision_declaration(match)

        original_after_literals = text
        text = self.double_precision_pattern.sub(replace_declaration_match, text)
        if text != original_after_literals:
            conversions_made = True

        # Convert dble() function calls
        original_after_declarations = text
        text = self._convert_dble_calls(text)
        if text != original_after_declarations:
            conversions_made = True

        return text, conversions_made

    def check_text(self, text: str) -> List[ConversionIssue]:
        """Check for double precision style issues without modifying text."""
        issues = []

        # Check for D literals
        for match in self.d_literal_pattern.finditer(text):
            if not self._is_in_string_or_comment(text, match.start()):
                line_num = text[:match.start()].count('\n') + 1
                literal_text = match.group(0)
                issues.append(ConversionIssue(
                    line_number=line_num,
                    error_code="STYLE001",
                    message=f"Double precision literal found: '{literal_text}' should be converted to _dp format",
                    original_text=literal_text
                ))

        # Check for double precision declarations
        for match in self.double_precision_pattern.finditer(text):
            if not self._is_in_string_or_comment(text, match.start()):
                line_num = text[:match.start()].count('\n') + 1
                issues.append(ConversionIssue(
                    line_number=line_num,
                    error_code="STYLE002",
                    message="Double precision declaration found, should be 'real(kind=dp)'",
                    original_text=match.group(0).strip()
                ))

        # Check for dble() function calls
        for match in self.dble_pattern.finditer(text):
            if not self._is_in_string_or_comment(text, match.start()):
                line_num = text[:match.start()].count('\n') + 1
                issues.append(ConversionIssue(
                    line_number=line_num,
                    error_code="STYLE003",
                    message="dble() function found, should be 'real(..., kind=dp)'",
                    original_text="dble()"
                ))

        return issues

    def add_required_imports(self, text: str, was_converted: bool) -> str:
        """Add 'use precision, only: dp' import if conversions were made."""
        if not was_converted:
            return text

        # Check if precision import already exists
        if self.precision_import_pattern.search(text):
            return text

        # Find module or submodule declaration
        module_match = self.module_pattern.search(text)
        if not module_match:
            # No module or submodule found, return text unchanged
            return text

        # Find where to insert the use statement
        module_end = module_match.end()
        remaining_text = text[module_end:]
        lines = remaining_text.split('\n')

        insert_line_idx = 0
        use_statements_found = False

        for i, line in enumerate(lines):
            stripped_line = line.strip()

            # Skip empty lines and comments at the beginning
            if not stripped_line or stripped_line.startswith('!'):
                continue

            # Check if this is a use statement
            if re.match(r'^\s*use\s+', line, re.IGNORECASE):
                use_statements_found = True
                insert_line_idx = i + 1  # Insert after this use statement
                continue

            # If we've found use statements and now hit something else, insert here
            if use_statements_found:
                insert_line_idx = i
                break

            # If this is 'implicit none' or other declaration, insert before it
            if self.use_section_end_pattern.match(line):
                insert_line_idx = i
                break

            # If we haven't found any use statements yet, this might be the first declaration
            insert_line_idx = i
            break

        # Determine indentation
        indent = "   "  # Default indentation
        if use_statements_found and insert_line_idx > 0:
            # Use indentation from previous use statement
            prev_line = lines[insert_line_idx - 1]
            indent = prev_line[:len(prev_line) - len(prev_line.lstrip())]
        elif insert_line_idx < len(lines):
            # Use indentation from the next line
            next_line = lines[insert_line_idx]
            if next_line.strip():
                indent = next_line[:len(next_line) - len(next_line.lstrip())]

        # Insert the precision import
        precision_import = f"{indent}use precision, only: dp"

        # Reconstruct the text
        before_module = text[:module_end]
        if insert_line_idx == 0:
            # Insert right after module declaration, before first non-empty line
            # If first line is empty (from newline after module), keep it that way
            if lines and not lines[0]:
                # Format: module\n\n use precision... \n implicit none...
                new_text = before_module + '\n' + precision_import + '\n' + '\n'.join(lines[1:])
            else:
                # No newline after module, insert with newline
                new_text = before_module + '\n' + precision_import + '\n' + '\n'.join(lines)
        else:
            before_insert = '\n'.join(lines[:insert_line_idx])
            after_insert = '\n'.join(lines[insert_line_idx:])
            # If before_insert starts with newline (from empty first line), don't add extra newline
            if before_insert and not before_insert.startswith('\n'):
                new_text = before_module + '\n' + before_insert + '\n' + precision_import + '\n' + after_insert
            elif before_insert:
                # before_insert starts with \n (from empty lines[0]), so skip one newline
                new_text = before_module + before_insert + '\n' + precision_import + '\n' + after_insert
            else:
                # before_insert is completely empty
                new_text = before_module + '\n' + precision_import + '\n' + after_insert

        return new_text

    def get_conversion_stats(self, original_text: str) -> dict:
        """Get statistics about conversions that would be made."""
        literal_count = sum(1 for match in self.d_literal_pattern.finditer(original_text)
                           if not self._is_in_string_or_comment(original_text, match.start()))

        declaration_count = sum(1 for match in self.double_precision_pattern.finditer(original_text)
                               if not self._is_in_string_or_comment(original_text, match.start()))

        dble_count = sum(1 for match in self.dble_pattern.finditer(original_text)
                        if not self._is_in_string_or_comment(original_text, match.start()))

        return {
            'literals': literal_count,
            'declarations': declaration_count,
            'dble_calls': dble_count
        }
